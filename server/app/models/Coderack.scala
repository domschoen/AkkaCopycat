
package models

import akka.actor.Actor
import akka.actor.ActorLogging
import akka.event.LoggingReceive
import akka.actor.ActorRef
import akka.actor.Props
import com.typesafe.config.ConfigFactory

import scala.xml.Utility
import play.api.Logger
import play.api.libs.ws._
import play.api.libs.json._
import play.api.libs.json.Reads._
import play.api.libs.functional.syntax._

import scala.concurrent.Future
import scala.concurrent.duration._
import play.api.Play.current
import javax.inject._
import models.SlipNode.SlipNodeRep
import models.Temperature.CheckClamped
import models.Workspace.Initialize
import models.codelet.CodeletType
import models.codelet.Codelet
import play.api.Configuration
import play.api.libs.concurrent.InjectedActorSupport

import scala.collection.mutable.ListBuffer




object Coderack {
  def props(workspace: ActorRef, slipnet: ActorRef, temperature: ActorRef, executionRun: ActorRef): Props = Props(new Coderack(workspace, slipnet, temperature, executionRun))

  case class Run(initialString: String, modifiedString: String, targetString: String)
  case class FinishInitializingWorkspaceStrings(number_of_objects: Int)
  case class PostInitialCodelets(number_of_objects: Int)
  case class UpdateEverythingResponse(t: Double)
  case class SlipnetUpdateEverythingResponse(t: Double)
  case class ProcessChooseAndRun(number_of_objects: Int, temperature: Double)
  case class ChooseAndRun(number_of_objects: Int, temperature: Double)

  case class ChooseAndRun2(temperature: Double)
  case object ChooseAndRun3
  case object Finish
  case object Step
  case object Initializing
  case class Post(codelet: ActorRef, urgency: Int)
  case class ProposeCorrespondence(
                                    correspondenceID: String,
                                    distiguishingConceptMappingSize: Int,
                                    distiguishingConceptMappingTotalStrength: Double
                                  )
  case class ProposeBond(bondID: String, d: Double)
  case class ProposeGroup(groupID: String, d: Double)
  case class ProposeRule(groupID: String, d: Double)

  case class ProposeDescription(descriptionID: String, urgency: Double)
  case class PostBondBuilder(bondID: String, strength: Double)
  case class PostDescriptionBuilder(descriptionID: String, strength: Double)
  case class PostGroupBuilder(groupID: String, strength: Double)
  case class PostCorrespondenceBuilder(correspondenceID: String, strength: Double)
  case class PostRuleBuilder(ruleID: String, strength: Double)

}


class Coderack(workspace: ActorRef, slipnet: ActorRef, temperature: ActorRef, executionRun: ActorRef) extends Actor with ActorLogging with InjectedActorSupport {
  import Coderack._
  import Codelet.Finished
  var woAppActor: Option[ActorRef] = None

  var speed_up_bonds = false
//  static Area CoderackArea,CoderackSmall,CoderackInfoArea;
//  static Frames MaximiseCoderack,MinimiseInfoArea;
//  static Caption[] Codelet_Captions;
//  static Caption[] CodeletInfo_Captions;
//  static Caption Codelet_Run;
  var codelets = ListBuffer.empty[ActorRef]

  var codelets_run = 0
  var number_of_bins = 7
  var last_update = 0
  var remove_breaker_codelets = false
  var remove_terraced_scan = false


  var initialCodelets : List[CodeletType] = List(
    CodeletType.BottomUpBondScout,
    CodeletType.ReplacementFinder,
    CodeletType.BottomUpCorrespondenceScout)
  var number_of_objects = 0
  var runTemperature: Double = 100.0
  var codeletsUrgency = Map.empty[ActorRef, Int]
  var initialString = ""
  var modifiedString =""
  var targetString = ""


  // define the codelet types
  def get_urgency_bin(urgency: Double): Int = {
    //int bin = (int)(urgency*random.rnd());
    val bin = ((urgency.toInt * number_of_bins) / 100).toInt
    val udjustedBin = if (bin>=number_of_bins) number_of_bins-1 else bin
    udjustedBin + 1
  }

  protected def createCodelet(codeletType: CodeletType, urgency: Int, arguments: Option[Any]) = {
    println(s"create codelet $codeletType")
    context.actorOf(Codelet.props(codeletType, urgency, workspace, slipnet, temperature, arguments))
  }


  def update_Everything() = {
//  GUI  Graph.GraphFrame.Redraw = true;
//  GUI  Graph.GraphMinFrame.Redraw = true;
    // update the strength values of all structures in the workspace
    workspace ! models.Workspace.UpdateEverything

    // Done in PostInitialCodelets
//    if (codelets_run > 0) {
//      post_top_down_codelets();
//      coderack.post_bottom_up_codelets();
//    }
    slipnet ! models.Slipnet.UpdateEverything

    // See Workspace
    // WorkspaceFormulas.update_temperature();

    // GUI Coderack_Pressure.calculate_Pressures()
  }


  def receive = LoggingReceive {
    // to the browser
    case Run(initialS, modifiedS, targetS) =>
      log.debug(s"Run with initial $initialString, modified: $modifiedString and target: $targetString")
      // TODO init
      initialString = initialS
      modifiedString = modifiedS
      targetString = targetS
      workspace ! Initialize(self, initialS, modifiedS, targetS)

    case FinishInitializingWorkspaceStrings(nbwos) =>
      log.debug("Coderack: FinishInitilizingWorkspaceStrings")
      number_of_objects = nbwos
      context.become(initializing)
      self ! Initializing

  }


  private def initializing: Receive = {
    case Initializing => {
      log.debug("Initializing")
      reset()
      temperature ! Temperature.GetTemperature
    }

    case Temperature.TemperatureResponse(t) =>
      log.debug("TemperatureResponse " + t)
      runTemperature = t
      // At beginning we post initial codelets because anyway codelets is empty
      context.become(startingRun)
      self ! ProcessChooseAndRun(number_of_objects,t)


  }

  private def startingRun: Receive = {
        case Finish =>
          temperature ! Temperature.GetClampTime
          context.become(finishRun)

        case PostInitialCodelets(number_of_objects) =>
          log.debug("PostInitialCodelets " + number_of_objects)
          val urgency: Int = 1
          val createdCodelets = for (codeletType <- initialCodelets;
                                 x <- (1 to number_of_objects);
                                 y <- (0 to 1)
                                 ) yield createCodelet(codeletType, urgency, None)
          log.debug("PostInitialCodelets createdCodelets size " + createdCodelets.size)
          log.debug("PostInitialCodelets codelets size " + codelets.size)

          //println("codeletsUrgency " + codeletsUrgency)
          for (newCodelet <- createdCodelets) {
            self ! Post(newCodelet, 1)
          }
          self ! ChooseAndRun3

        case Post(codelet: ActorRef, urgency) => {
          // TODO
          addCodelet(codelet, urgency)
          // Graphic stuff
          // Coderack_Pressure.AddCodelet(c);

          // removes a codelet if there are too many
          if (codelets.size > 100) {
            val nc = chooseOldCodelet();
            removeCodelet(nc)
          }

          // Graphics
          // if (CoderackArea.Visible) update_captions();
        }
        case ChooseAndRun(nbobjs, temperature) =>
          codelets_run += 1
          self ! ProcessChooseAndRun(nbobjs, temperature)


        case ProcessChooseAndRun(nbobjs, temperature) =>
          number_of_objects = nbobjs
          // How to ask all codelets for the urgency for having the sum of all codeleets urgencies
          // https://medium.com/kenshoos-engineering-blog/assembling-requests-from-multiple-actors-44434c18e69d
          // => ask patter is a solution
          // second solution: coderack has a map codelet -> urgency and it is updated by notifications
          log.debug(s"mainloop | codelets_run ${codelets_run} clamp_time ${Temperature.clamp_time} " +
            s" last_update ${last_update} time_step_length ${Slipnet.time_step_length}")

          // TODO
          //temperature ! CheckClamped(codelets_run)
          if (((codelets_run - last_update) >= Slipnet.time_step_length) || (codelets_run == 0)) {
            log.debug("update_Everything")
            workspace ! models.Workspace.UpdateEverything(temperature)
            //update_Everything();
          } else {
            self ! ChooseAndRun2(temperature)
          }


        case UpdateEverythingResponse(t) =>
          slipnet ! models.Slipnet.UpdateEverything(t)

        case SlipnetUpdateEverythingResponse(t) =>
          last_update = codelets_run
          self ! ChooseAndRun2(t)


        case ChooseAndRun2(t) =>
          runTemperature = t

          // if coderack is empty, clamp initially clamped slipnodes and
          // post initial_codelets;
          log.debug(s"ChooseAndRun2 $total_num_of_codelets T: $t")
          if (total_num_of_codelets()==0){
            self ! PostInitialCodelets(number_of_objects)
          } else {
            self ! ChooseAndRun3
          }
        case ChooseAndRun3 =>
          // From Coderack.choose().java.60
          // let's change the view point: The urgency of a codelet is only in the context of a coderack => not a property of the codelet
          println("choose T: " + runTemperature);

          val scale: Double = (100.0 - runTemperature + 10.0) / 15.0
          println("Choose codelet codelets size " + codelets.size + " scale " + scale)

          val urgencies = codelets.map(c => Math.pow(codeletsUrgency(c),scale))
          log.debug(s"urgencies: $urgencies")
          // then we choose a random number in the urgency sum and we choose the codelet at this random number looking
          // from first codelet up to this random number in terms of urgency
          val index = Utilities.valueProportionalRandomIndexInValueList(urgencies.toList)
          println("Choose codelet at index " + index)
          val chosenCodelet = codelets(index)

          // we remove this codelet from codelets and we run it
          codelets = codelets.filter(e => e != chosenCodelet)
          log.debug("ChooseAndRun: codelets found " + chosenCodelet)

          chosenCodelet ! Codelet.Run(initialString, modifiedString, targetString, runTemperature)


        case ProposeCorrespondence(
          correspondenceID,
          distiguishingConceptMappingSize,
          distiguishingConceptMappingTotalStrength
        ) =>
          val dv = distiguishingConceptMappingSize.toDouble
          val urgencyRaw = if (dv > 0.0) distiguishingConceptMappingTotalStrength / dv else distiguishingConceptMappingTotalStrength

          // this is GUI ncd.Pressure_Type = orig.Pressure_Type;
          val urgency = get_urgency_bin(urgencyRaw)
          val newCodelet = createCodelet(CodeletType.CorrespondenceStrengthTester, urgency, Some(correspondenceID))
          self ! Post(newCodelet, urgency)
          sender() ! Finished

        case ProposeBond(bondID, bond_degree_of_association) =>
          val urgencyRaw = bond_degree_of_association
          println("propose_bond urgency " + urgencyRaw)
          val urgency = get_urgency_bin(urgencyRaw)
          println("propose_bond urgency " + urgency)

          val newCodelet = createCodelet(CodeletType.BondStrengthTester, urgency, Some(bondID))
          self ! Post(newCodelet, urgency)
          sender() ! Finished

        case ProposeGroup(groupID, urgencyRaw) =>
          val urgency = get_urgency_bin(urgencyRaw)
          val newCodelet = createCodelet(CodeletType.GroupStrengthTester, urgency, Some(groupID))
          self ! Post(newCodelet, urgency)
          sender() ! Finished

        case  ProposeDescription(descriptionID, urgencyRaw) =>
          val urgency = get_urgency_bin(urgencyRaw)
          val newCodelet = createCodelet(CodeletType.DescriptionStrengthTester, urgency, Some(descriptionID))
          // ignored ncd.Pressure_Type = orig.Pressure_Type;
          self ! Post(newCodelet, urgency)
          sender() ! Finished

        case ProposeRule(ruleID, urgencyRaw) =>
          val urgency = get_urgency_bin(urgencyRaw)
          val newCodelet = createCodelet(CodeletType.GroupStrengthTester, urgency, Some(ruleID))
          self ! Post(newCodelet, urgency)
          sender() ! Finished


        case PostBondBuilder(bondID, strength) =>
          val urgency = get_urgency_bin(strength)

          // Pressure_Type argument is missing
          val newCodelet = createCodelet(CodeletType.BondBuilder, urgency, Some(bondID))
          self ! Post(newCodelet, urgency)
          sender() ! Finished

        case PostDescriptionBuilder(descriptionID, strength) =>
          val urgency = get_urgency_bin(strength)
          val newCodelet = createCodelet(CodeletType.DescriptionBuilder, urgency, Some(descriptionID))
          // Pressure_Type argument is missing
//          nc.Pressure_Type = this.Pressure_Type;
          self ! Post(newCodelet, urgency)
          sender() ! Finished


        case PostGroupBuilder(groupID, strength) =>
          val urgency = get_urgency_bin(strength)

          // Pressure_Type argument is missing
          val newCodelet = createCodelet(CodeletType.GroupBuilder, urgency, Some(groupID))
          self ! Post(newCodelet, urgency)
          sender() ! Finished

        case PostCorrespondenceBuilder(correspondenceID, strength) =>
          val urgency = get_urgency_bin(strength)
          // Pressure_Type argument is missing
          val newCodelet = createCodelet(CodeletType.CorrespondenceBuilder, urgency, Some(correspondenceID))
          self ! Post(newCodelet, urgency)
          sender() ! Finished

        case PostRuleBuilder(ruleID, strength) =>
          val urgency = get_urgency_bin(strength)

          // Pressure_Type argument is missing
          val newCodelet = createCodelet(CodeletType.RuleBuilder, urgency, Some(ruleID))
          self ! Post(newCodelet,urgency)
          sender() ! Finished

  }

  def total_num_of_codelets() = {
    codelets.size
  }

  def reset() = {
  }



  private  def finishRun: Receive = {
    case Temperature.ClampTime(clampTime) =>
      if (codelets_run >= clampTime) {
        temperature ! Temperature.SetClamped(false)
      }
      if ((codelets_run-last_update)>= Slipnet.time_step_length) {
        executionRun ! ExecutionRun.UpdateEverything
        last_update = codelets_run
      }
      if (codelets.size == 0) {
        self ! PostInitialCodelets
        context.become(receive)
      }
  }

  private def chooseOldCodelet(): ActorRef = {
    codelets(0)
  }
  private def removeCodelet(nc: ActorRef) = {
    codelets -= nc
  }
  private def addCodelet(codelet: ActorRef, urgency: Int) = {
    codelets += codelet
    codeletsUrgency += (codelet -> urgency)
  }
}

