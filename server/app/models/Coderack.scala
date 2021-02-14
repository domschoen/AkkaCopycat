
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
import models.Workspace.{GetNumCodeletsResponse, Initialize, UpdateEverythingFollowUp}
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
  case class SlipnetUpdateEverythingResponse(t: Double)
  case class ProcessChooseAndRun(number_of_objects: Int, temperature: Double)
  case class ChooseAndRun(number_of_objects: Int, temperature: Double)
  case class ChooseAndRun2(temperature: Double)
  case object ChooseAndRun3
  case object Finish
  case object Step
  case object Initializing
  case class Post(codelet: CodeletWrapper, rndOpt: Option[Double]) // , name: String)
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
  case class PostCodelets(codeletToPost: List[(String,Either[Double, Int], Option[String], Option[Double])], t: Double)
  case class GetNumCodelets(t:Double)

  case class CodeletWrapper (
                              codelet: ActorRef,
                              time_stamp: Int,
                              urgency: Int,
                              name: String
                            )
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
  var codelets = ListBuffer.empty[CodeletWrapper]

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
  var initialString = ""
  var modifiedString =""
  var targetString = ""


  // define the codelet types
  def get_urgency_bin(urgency: Double): Int = {
    //int bin = (int)(urgency*Random.rnd(null));
    val bin = ((urgency.toInt * number_of_bins) / 100).toInt
    val udjustedBin = if (bin>=number_of_bins) number_of_bins-1 else bin
    udjustedBin + 1
  }

  protected def createCodelet(codeletType: CodeletType, urgency: Int, arguments: Option[Any]) = {
    val name = Codelet.stringWithCodeletType(codeletType)
    println(s"create codelet $name urgency $urgency time_stamp $codelets_run")
    val codelet = context.actorOf(Codelet.props(codeletType, urgency, workspace, slipnet, temperature, arguments))
    CodeletWrapper(codelet, codelets_run, urgency, name)
  }

/*
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
  }*/


  def receive = LoggingReceive {
    // to the browser
    case Run(initialS, modifiedS, targetS) =>
      log.debug(s"${getClass.getName}. Run with initial $initialString, modified: $modifiedString and target: $targetString")
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

      Random.setseed(0)

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
            self ! Post(newCodelet, None)
          }
          self ! ChooseAndRun3

        case Post(codelet: CodeletWrapper, rndOpt) => {

          addCodelet(codelet)
          // Graphic stuff
          // Coderack_Pressure.AddCodelet(c);
          log.debug(s"Post Codelet codelets size ${codelets.size} rndOpt $rndOpt name ${codelet.name}")

          // removes a codelet if there are too many
          if (codelets.size > 100) {
            log.debug("Post. codelets.size > 100");
            val ncOpt = chooseOldCodelet(rndOpt)
            ncOpt match {
              case Some(nc) => removeCodelet(nc)
              case None => ()
            }
          }
          log.debug("printUrgencies urgencies: " + codelets.map(_.urgency));


          // Graphics
          // if (CoderackArea.Visible) update_captions();
        }



        case ChooseAndRun(nbobjs, temperature) =>
          log.debug(s"mainloop | codelets_run ${codelets_run} clamp_time ${Temperature.clamp_time} " +
            s" last_update ${last_update} time_step_length ${Slipnet.time_step_length}")

          //if(Random.rndseed > 1998) {
//            log.debug("On arrete pour l'instant")
//          } else {
            self ! ProcessChooseAndRun(nbobjs, temperature)

//          }


        case ProcessChooseAndRun(nbobjs, temperature) =>
          number_of_objects = nbobjs
          // How to ask all codelets for the urgency for having the sum of all codeleets urgencies
          // https://medium.com/kenshoos-engineering-blog/assembling-requests-from-multiple-actors-44434c18e69d
          // => ask patter is a solution
          // second solution: coderack has a map codelet -> urgency and it is updated by notifications

//          if (codelets_run > 345) {
//            log.debug("Stop")
//          } else {
            // TODO
            //temperature ! CheckClamped(codelets_run)
            if (((codelets_run - last_update) >= Slipnet.time_step_length) || (codelets_run == 0)) {
              log.debug("update_Everything")
              workspace ! models.Workspace.UpdateEverything(codelets_run, temperature)
              //update_Everything();
            } else {
              self ! ChooseAndRun2(temperature)
            }
//          }



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
          log.debug("choose T: " + runTemperature);

          val scale: Double = (100.0 - runTemperature + 10.0) / 15.0
          log.debug("Choose codelet codelets size " + codelets.size + " scale " + scale)

          val urgues = codelets.map(c => c.urgency)
          val urgencies = codelets.map(c => Math.pow(c.urgency,scale))
          log.debug(s"urgencies: $urgues")
          // then we choose a random number in the urgency sum and we choose the codelet at this random number looking
          // from first codelet up to this random number in terms of urgency
          val index = Utilities.valueProportionalRandomIndexInValueList(urgencies.toList)
          log.debug("Choose codelet at index " + index)
          val chosenCodelet = codelets(index)

          // we remove this codelet from codelets and we run it
          codelets = codelets.filter(e => e != chosenCodelet)
          log.debug("ChooseAndRun: codelets found " + chosenCodelet)

          codelets_run = codelets_run + 1
          chosenCodelet.codelet ! Codelet.Run(initialString, modifiedString, targetString, runTemperature)


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
          self ! Post(newCodelet, None)
          sender() ! Finished

        case ProposeBond(bondID, bond_degree_of_association) =>
          val urgencyRaw = bond_degree_of_association
          println("propose_bond urgency " + urgencyRaw)
          val urgency = get_urgency_bin(urgencyRaw)
          println("propose_bond urgency " + urgency)

          val newCodelet = createCodelet(CodeletType.BondStrengthTester, urgency, Some(bondID))
          self ! Post(newCodelet, None)
          sender() ! Finished

        case ProposeGroup(groupID, urgencyRaw) =>
          val urgency = get_urgency_bin(urgencyRaw)
          val newCodelet = createCodelet(CodeletType.GroupStrengthTester, urgency, Some(groupID))
          self ! Post(newCodelet, None)
          sender() ! Finished

        case  ProposeDescription(descriptionID, urgencyRaw) =>
          val urgency = get_urgency_bin(urgencyRaw)
          val newCodelet = createCodelet(CodeletType.DescriptionStrengthTester, urgency, Some(descriptionID))
          // ignored ncd.Pressure_Type = orig.Pressure_Type;
          self ! Post(newCodelet, None)
          sender() ! Finished

        case ProposeRule(ruleID, urgencyRaw) =>
          val urgency = get_urgency_bin(urgencyRaw)
          log.debug("Coderack. ProposeRule " + ruleID)
          val newCodelet = createCodelet(CodeletType.RuleStrengthTester, urgency, Some(ruleID))
          self ! Post(newCodelet, None)
          sender() ! Finished


        case PostBondBuilder(bondID, strength) =>
          val urgency = get_urgency_bin(strength)
          log.debug(s"PostBondBuilder urgency $strength get_urgency_bin $urgency")

          // Pressure_Type argument is missing
          val newCodelet = createCodelet(CodeletType.BondBuilder, urgency, Some(bondID))
          self ! Post(newCodelet, None)
          sender() ! Finished

        case PostDescriptionBuilder(descriptionID, strength) =>
          val urgency = get_urgency_bin(strength)
          val newCodelet = createCodelet(CodeletType.DescriptionBuilder, urgency, Some(descriptionID))
          // Pressure_Type argument is missing
//          nc.Pressure_Type = this.Pressure_Type;
          self ! Post(newCodelet, None)
          sender() ! Finished


        case PostGroupBuilder(groupID, strength) =>
          val urgency = get_urgency_bin(strength)

          // Pressure_Type argument is missing
          val newCodelet = createCodelet(CodeletType.GroupBuilder, urgency, Some(groupID))
          self ! Post(newCodelet, None)
          sender() ! Finished

        case PostCorrespondenceBuilder(correspondenceID, strength) =>
          val urgency = get_urgency_bin(strength)
          log.debug(s"PostCorrespondenceBuilder urgency $strength get_urgency_bin $urgency")

          // Pressure_Type argument is missing
          val newCodelet = createCodelet(CodeletType.CorrespondenceBuilder, urgency, Some(correspondenceID))
          self ! Post(newCodelet, None)
          sender() ! Finished

        case PostRuleBuilder(ruleID, strength) =>
          val urgency = get_urgency_bin(strength)

          // Pressure_Type argument is missing
          val newCodelet = createCodelet(CodeletType.RuleBuilder, urgency, Some(ruleID))
          self ! Post(newCodelet,None)
          sender() ! Finished

        case PostCodelets(codeletToPost, t) =>
          for ((st, rawUrgency, argOpt, rndOpt) <- codeletToPost) {
            val codeletType = Codelet.codeletTypeWithString(st)
            val urgency = rawUrgency match {
              case Right(x) => x
              case Left(x) => get_urgency_bin(x)
            }
            System.out.println("PostCodelets " + codeletType + " rawUrgency" + rawUrgency + " urgency_bin " + urgency);

            val newCodelet = createCodelet(codeletType, urgency, argOpt)
            self ! Post(newCodelet, rndOpt)
          }
          slipnet ! models.Slipnet.UpdateEverything(t)



        case GetNumCodelets(t) =>
          sender() ! GetNumCodeletsResponse(codelets.size, t)
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

  private def chooseOldCodelet(rndOpt: Option[Double]): Option[CodeletWrapper] = {
    // selects an old codelet to remove from the coderack
    // more likely to select lower urgency codelets

    if (codelets.isEmpty) {
      None
    } else {
      var urgency_values = codelets.map( c =>  {
        if (codelets_run == 225) {
          log.debug("choose_old_codelet c.time_stamp " + c.time_stamp + " c.urgency " + c.urgency + " name " + c.name);
        }
        (codelets_run - c.time_stamp).toDouble * (7.5 - c.urgency).toDouble
      })
      val urgsum = urgency_values.sum


      val rndValue = rndOpt match {
        case Some(rnd) => rnd
        case None => Random.rnd(null)
      }
      val chosen = rndValue  * urgsum;
      log.debug("choose_old_codelet chosen " + chosen + " codelets_run " + codelets_run + " rndValue " + rndValue + " urgsum " + urgsum);

      val index = Utilities.valueProportionalIndexInValueListAtValue(0.0, 0, urgency_values.toList, chosen)
      Some(codelets(index))
    }
  }
  private def removeCodelet(nc: CodeletWrapper) = {
    codelets -= nc
    log.debug("removeCodelet codelets size " + codelets.size)
  }
  private def addCodelet(codelet: CodeletWrapper) = {
    codelets += codelet
  }
}

