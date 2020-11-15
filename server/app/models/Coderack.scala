
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
import models.Slipnet.WorkspaceStructureRep
import models.Workspace.Initialize
import models.codelet.CodeletType
import models.codelet.Codelet
import play.api.Configuration
import play.api.libs.concurrent.InjectedActorSupport

import scala.collection.mutable.ListBuffer




object Coderack {
  def props(workspace: ActorRef, slipnet: ActorRef, temperature: ActorRef, executionRun: ActorRef): Props = Props(new Coderack(workspace, slipnet, temperature, executionRun))

  case class Run(initialString: String, modifiedString: String, targetString: String)
  case object PostInitialCodelets
  case object ChooseAndRun
  case object Finish
  case object Step
  case object Initializing
  case class Post(codelet: ActorRef)
  case class ProposeCorrespondence(
                                    correspondenceID: String,
                                    distiguishingConceptMappingSize: Int,
                                    distiguishingConceptMappingTotalStrength: Double
                                  )
  case class ProposeBond(bondID: String,
                         d: Double)
}


class Coderack(workspace: ActorRef, slipnet: ActorRef, temperature: ActorRef, executionRun: ActorRef) extends Actor with ActorLogging with InjectedActorSupport {
  import Coderack._
  import Codelet.Finished
  var woAppActor: Option[ActorRef] = None
  var codeletRuns = 0
  var lastUpdate = 0
  var codelets = ListBuffer.empty[ActorRef]
  var initialCodelets : List[CodeletType] = List(
    CodeletType.BottomUpBondScout,
    CodeletType.ReplacementFinder,
    CodeletType.BottomUpCorrespondenceScout)
  var workspaceNumElements = 0
  var runTemperature: Double = 100.0
  var codeletsUrgency = Map.empty[ActorRef, Int]
  val r = scala.util.Random
  var initialString = ""
  var modifiedString =""
  var targetString = ""
  var number_of_bins = 7

  // define the codelet types
  def get_urgency_bin(urgency: Double): Int = {
    //int bin = (int)(urgency*random.rnd());
    val bin = ((urgency.toInt * number_of_bins) / 100).toInt
    val udjustedBin = if (bin>=number_of_bins) number_of_bins-1 else bin
    udjustedBin + 1
  }

  protected def createCodelet(codeletType: CodeletType, urgency: Int, arguments: Option[Any]) =
    context.actorOf(Codelet.props(codeletType, urgency, workspace, slipnet, temperature, arguments))

  def receive = LoggingReceive {
    // to the browser
    case Run(initialS, modifiedS, targetS) => {
      log.debug(s"Run with initial $initialString, modified: $modifiedString and target: $targetString")
      // TODO init
      initialString = initialS
      modifiedString = modifiedS
      targetString = targetS
      workspace ! Initialize(initialS, modifiedS, targetS)
      context.become(initializing)
      self ! Initializing

    }
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
      self ! PostInitialCodelets


  }

  private def startingRun: Receive = {
        case Finish =>
          temperature ! Temperature.GetClampTime
          context.become(finishRun)

        case PostInitialCodelets =>
          log.debug("PostInitialCodelets")
          val urgency: Int = 1
          codelets = (for (codeletType <- initialCodelets;
                                 x <- (0 to workspaceNumElements);
                                 y <- (0 to 2)
                                 ) yield createCodelet(codeletType, urgency, None)).to[ListBuffer]

          codeletsUrgency = codelets.zip(List.fill(codelets.size)(urgency)).toMap
          for (newCodelet <- codelets) {
            self ! Post(newCodelet)
          }
          self ! ChooseAndRun

        case Post(codelet: ActorRef) => {
          // TODO
          codelets = codelets :+ codelet
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

        case ChooseAndRun =>
          log.debug("ChooseAndRun")
          if (codelets.size == 0) {
            log.debug("ChooseAndRun: codelets is empty => stop")
          } else {
            // How to ask all codelets for the urgency for having the sum of all codeleets urgencies
            // https://medium.com/kenshoos-engineering-blog/assembling-requests-from-multiple-actors-44434c18e69d
            // => ask patter is a solution

            // let's change the view point: The urgency of a codelet is only in the context of a coderack => not a property of the codelet
            val scale: Double = (100.0 - runTemperature + 10.0) / 15.0

            val urgencies = codelets.map(c => Math.pow(codeletsUrgency(c),scale))
            // then we choose a random number in the urgency sum and we choose the codelet at this random number looking
            // from first codelet up to this random number in terms of urgency
            val index = Utilities.valueProportionalRandomIndexInValueList(urgencies.toList)

            val chosenCodelet = codelets(index)

            // we remove this codelet from codelets and we run it
            codelets = codelets.filter(e => e != chosenCodelet)
            log.debug("ChooseAndRun: codelets found " + chosenCodelet)

            chosenCodelet ! Codelet.Run(initialString, modifiedString, targetString, runTemperature)
          }

        case ProposeCorrespondence(
          correspondenceID,
          distiguishingConceptMappingSize,
          distiguishingConceptMappingTotalStrength
        ) =>
          val dv = distiguishingConceptMappingSize.toDouble
          val urgency = if (dv > 0.0) distiguishingConceptMappingTotalStrength / dv else distiguishingConceptMappingTotalStrength

          // this is GUI ncd.Pressure_Type = orig.Pressure_Type;

          val newCodelet = createCodelet(CodeletType.CorrespondenceStrengthTester, get_urgency_bin(urgency), Some(correspondenceID))
          self ! Post(newCodelet)
          sender() ! Finished

        case ProposeBond(bondID, bond_degree_of_association) =>
          val urgency = bond_degree_of_association
          val newCodelet = createCodelet(CodeletType.BondStrengthTester, get_urgency_bin(urgency), Some(bondID))
          self ! Post(newCodelet)
          sender() ! Finished


  }


  def reset() = {
  }



  private  def finishRun: Receive = {
    case Temperature.ClampTime(clampTime) =>
      if (codeletRuns >= clampTime) {
        temperature ! Temperature.SetClamped(false)
      }
      if ((codeletRuns-lastUpdate)>= Slipnet.time_step_length) {
        executionRun ! ExecutionRun.UpdateEverything
        lastUpdate = codeletRuns
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

}

