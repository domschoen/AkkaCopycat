package models

import java.util.UUID
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
import models.Bond.BondRep
import models.ConceptMapping.ConceptMappingRep
import models.Group.{FutureGroupRep, GroupRep}
import models.SlipNode.SlipNodeRep
import models.Slipnet.DirValue.DirValue
import models.Slipnet.DescriptionTypeInstanceLinksToNodeInfo
import models.Workspace.{GoWithBottomUpCorrespondenceScout3, GoWithDescriptionBuilder, GoWithGroupScoutWholeString, GoWithGroupStrengthTester, GoWithImportantObjectCorrespondenceScout, GoWithImportantObjectCorrespondenceScout2, GoWithImportantObjectCorrespondenceScout3, GoWithRuleBuilder, GoWithRuleScout, GoWithRuleScout2, GoWithRuleScout3, GoWithRuleStrengthTester, GoWithRuleTranslator, GoWithRuleTranslator2, GoWithTopDownBondScout2, GoWithTopDownBondScoutWithResponse, GoWithTopDownDescriptionScout2, GoWithTopDownGroupScoutCategory, GoWithTopDownGroupScoutDirection2, InitializeWorkspaceStringsResponse, SlipnetLookAHeadForNewBondCreationResponse, SlippageListShell, UpdateEverything, WorkspaceProposeBondResponse, WorkspaceProposeRule, WorkspaceProposeRuleResponse}
import models.WorkspaceObject.WorkspaceObjectRep
import models.WorkspaceStructure.WorkspaceStructureRep
import models.codelet.BottomUpBondScout.{GoWithBottomUpBondScout2Response, GoWithBottomUpBondScoutResponse}
import models.codelet.BottomUpCorrespondenceScout.GoWithBottomUpCorrespondenceScout3Response
import models.codelet.BottomUpDescriptionScout.GoWithBottomUpDescriptionScoutResponse
import models.codelet.Codelet.{Finished, PrepareDescriptionResponse}
import models.codelet.DescriptionStrengthTester.GoWithDescriptionStrengthTesterResponse
import models.codelet.GroupScoutWholeString.{GoWithGroupScoutWholeStringResponse, GroupScoutWholeString2Response, GroupScoutWholeString3Response}
import models.codelet.GroupStrengthTester.GoWithGroupStrengthTesterResponse
import models.codelet.ImportantObjectCorrespondenceScout.{GoWithImportantObjectCorrespondenceScout2Response, GoWithImportantObjectCorrespondenceScout3Response, GoWithImportantObjectCorrespondenceScoutResponse}
import models.codelet.RuleScout.{GoWithRuleScout2Response, GoWithRuleScout3Response, GoWithRuleScoutResponse, RuleScoutProposeRule}
import models.codelet.RuleStrengthTester.GoWithRuleStrengthTesterResponse
import models.codelet.RuleTranslator.GoWithRuleTranslatorResponse
import models.codelet.TopDownDescriptionScout.GoWithTopDownDescriptionScoutResponse
import models.codelet.TopDownGroupScoutCategory.{GoWithTopDownGroupScoutCategory2Response, GoWithTopDownGroupScoutCategoryResponse}
import models.codelet.TopDownGroupScoutDirection.GoWithTopDownGroupScoutDirectionResponse
import models.codelet.{Codelet, CodeletType}
import play.api.Configuration
import play.api.libs.concurrent.InjectedActorSupport

import scala.collection.mutable.ListBuffer
import scala.util.control.Breaks._





object Workspace {
  def props(slipnet: ActorRef, temperature: ActorRef): Props = Props(new Workspace(slipnet, temperature))

  case class Run(executionRun: ActorRef, initialString: String, modifiedString: String, targetString: String)
  case class Initialize(initialS: String, modifiedS: String, targetS: String)
  case class InitializeWorkspaceStringsResponse(
                                                 initialDescriptions: List[WorkspaceObjectRep],
                                                 modifiedDescriptions: List[WorkspaceObjectRep],
                                                 targetDescriptions: List[WorkspaceObjectRep])
  case object Step
  case object Found
  case class GoWithBreaker(temperature: Double)
  case class BondWithNeighbor(temperature: Double)
  case class GoWithBottomUpBondScout2(from: WorkspaceObjectRep, to:WorkspaceObjectRep, fromFacets: List[SlipNodeRep], toFacets: List [SlipNodeRep])
  case class WorkspaceProposeBond(bondFrom: WorkspaceObjectRep,
                                      bondTo: WorkspaceObjectRep,
                                      bondCategory: SlipNodeRep,
                                      bondFacet: SlipNodeRep,
                                      fromDescriptor: Option[SlipNodeRep],
                                      toDescriptor: Option[SlipNodeRep],
                                      slipnetLeft: SlipNodeRep,
                                      slipnetRight: SlipNodeRep
                                     )
  case class WorkspaceProposeBondResponse(bondID: String)

  case class WorkspaceProposeGroup(
                                    object_rep_list: List[WorkspaceObjectRep],
                                    bls: List[BondRep],
                                    group_category: String,
                                    direction_category: Option[String],
                                    bond_facet: String,
                                  )

  case class WorkspaceProposeGroupResponse(groupID: String)

  case object GoWithReplacementFinder
  case class GoWithTopDownGroupScoutCategory(slipNodeID: String, bondFocus: String, t: Double)
  case class GoWithTopDownGroupScoutCategory2(
                                              groupID: String,
                                              direction: SlipNodeRep,
                                              fromob: WorkspaceObjectRep,
                                              bond_category: SlipNodeRep,
                                              temperature: Double,
                                              lengthActivation: Double
                                             )


  case class GoWithBottomUpDescriptionScout(temperature: Double)
  case class GoWithTopDownDescriptionScout(descriptionTypeID: String, temperature: Double)
  case class GoWithTopDownDescriptionScout2(chosen_object: WorkspaceObjectRep, i: DescriptionTypeInstanceLinksToNodeInfo)

  case class PrepareDescription(chosen_object: WorkspaceObjectRep,
                                chosen_propertyRep: SlipNodeRep,
                                description_typeRep: SlipNodeRep)
  case class GoWithBottomUpCorrespondenceScout(temperature: Double)
  case class GoWithBottomUpCorrespondenceScout3(fg: FutureGroupRep, obj2: WorkspaceObjectRep)
  case class GoWithBottomUpCorrespondenceScout2(
                                                 obj1: WorkspaceObjectRep,
                                                 obj2: WorkspaceObjectRep,
                                                 concept_mapping_list : List[ConceptMappingRep],
                                                 flip_obj2: Boolean,
                                                 distiguishingConceptMappingSize: Int,
                                                 distiguishingConceptMappingTotalStrength: Double,
                                                 temperature: Double
                                               )
  case class GoWithTopDownBondScoutCategory(slipNodeID: String, temperature: Double)
  case class GoWithTopDownBondScout2(fromob: WorkspaceObjectRep, toob: WorkspaceObjectRep, todtypes: List[SlipNodeRep])
  case class GoWithTopDownBondScoutDirection(slipNodeID: String, temperature: Double)

  case class GoWithTopDownBondScoutWithResponse(from: WorkspaceObjectRep, to: WorkspaceObjectRep, fromdtypes: List[SlipNodeRep], todtypes: List[SlipNodeRep])

  case class GoWithDescriptionBuilder(descriptionID: String, temperature: Double)

  case class GoWithGroupBuilder(temperature: Double, groupID: String)
  case class GoWithDescriptionStrengthTester(temperature: Double, descriptionID: String)
  case class GoWithBondStrengthTester(temperature: Double, bondID: String)


  case class GoWithBondBuilder(temperature: Double, bondID: String)

  case class GoWithTopDownGroupScoutDirection(groupID: String, direction: SlipNodeRep, fromobrep: WorkspaceObjectRep, t:Double)
  case class GoWithTopDownGroupScoutDirection2(group_category: Option[SlipNodeRep], fromob: WorkspaceObjectRep, firstBondUUID: String, bond_category: SlipNodeRep)
  case class CommonSubProcessing(id: String, fromobUUID: String, firstBondUUID: String, bond_category: SlipNodeRep)

  case class GoWithGroupScoutWholeString(t: Double)
  case class GoWithGroupScoutWholeString2(left_most: WorkspaceObjectRep,slipnetLeft: SlipNodeRep,
                                          slipnetRight: SlipNodeRep)
  case class GoWithGroupStrengthTester(temperature: Double, bondID: String)
  case class LookAHeadForNewBondCreation(s: ActorRef, groupID: String, index: Int, incg: List[String], newBondList: List[BondRep])
  case class SlipnetLookAHeadForNewBondCreationResponse(
                                                         s: ActorRef,
                                                         g: GroupRep,
                                                         index: Int,
                                                         incg: List[String],
                                                         newBondList: List[BondRep],
                                                         related: Option[SlipNodeRep],
                                                         from_obj_id: String,
                                                         to_obj_id: String,
                                                         bond_facet: SlipNodeRep,
                                                         slipnetLeft: SlipNodeRep,
                                                         slipnetRight: SlipNodeRep
                                                       )
  case object GoWithRuleScout
  case class WorkspaceProposeRule(
                                   facet: Option[String],
                                  description: Option[String],
                                  objectCategory: Option[String],
                                  relation: Option[String],
                                  lengthSlipNode: SlipNodeRep,
                                  predecessorSlipNode: SlipNodeRep,
                                  successorSlipNode: SlipNodeRep
                                 )
  case class WorkspaceProposeRuleResponse(ruleID: String)
  case class GoWithRuleScout2(changed: WorkspaceObjectRep, string_position_category: SlipNodeRep, letter_category: SlipNodeRep)
  case class GoWithRuleScout3(slippage_list_rep: List[ConceptMappingRep], object_list: List[SlipNodeRep],obj2: WorkspaceObjectRep)

  case class SlippageListShell(
                                sl: List[ConceptMappingRep],
                                slippageCandidates: List[ConceptMappingRep]
                              )
  case class GoWithRuleStrengthTester(temperature: Double, ruleID: String)
  case class GoWithRuleBuilder(ruleID: String)
  case class GoWithRuleTranslator(t: Double)
  case class GoWithRuleTranslator2(slippage_list_rep: List[ConceptMappingRep])
  //case class SlipnodeActivationChanged(id: String, activation: Double)
  case class GoWithImportantObjectCorrespondenceScout(t: Double)
  case object GoWithImportantObjectCorrespondenceScout2
  case class GoWithImportantObjectCorrespondenceScout3(slippage_list_rep: List[ConceptMappingRep], s: SlipNodeRep, t:Double, obj1: WorkspaceObjectRep)

  case object UpdateEverything
}

class Workspace(slipnet: ActorRef, temperature: ActorRef) extends Actor with ActorLogging with InjectedActorSupport {
  import Workspace.{
    Initialize,
    Found,
    GoWithBottomUpCorrespondenceScout,
    GoWithBottomUpCorrespondenceScout2,
    GoWithBottomUpDescriptionScout,
    GoWithTopDownDescriptionScout,
    GoWithTopDownBondScoutCategory,
    GoWithTopDownBondScoutDirection,
    GoWithTopDownGroupScoutCategory,
    GoWithTopDownGroupScoutCategory2,
    WorkspaceProposeGroup,
    WorkspaceProposeGroupResponse,
    PrepareDescription,
    GoWithReplacementFinder,
    GoWithBondBuilder,
    GoWithGroupBuilder,
    GoWithDescriptionStrengthTester,
    GoWithBondStrengthTester,
    BondWithNeighbor,
    GoWithBottomUpBondScout2,
    WorkspaceProposeBond,
    GoWithBreaker,
    GoWithTopDownGroupScoutDirection,
    CommonSubProcessing,
    GoWithGroupScoutWholeString2,
    LookAHeadForNewBondCreation
  }

  import Slipnet._
  import Coderack._
  import models.codelet.BottomUpCorrespondenceScout.{
    GoWithBottomUpCorrespondenceScoutWorkspaceReponse,
    GoWithBottomUpCorrespondenceScout2Response
  }
  import models.codelet.BondStrengthTester.GoWithBondStrengthTesterResponse

  var executionRunActor: ActorRef = null
  var coderack: ActorRef = null

  var found_answer = false;
  // var structures = ListBuffer.empty[WorkspaceStructure]
  // initial ---> target
  // modified ----> ?
  var initial: WorkspaceString = null
  var target: WorkspaceString = null
  var modified: WorkspaceString = null
  val r = scala.util.Random
  var changed_object = Option.empty[WorkspaceObject]
  var structureRefs = Map.empty[String, WorkspaceStructure]
  // Because a Map doesn't garantee the order, we need also an array
  var structures = ListBuffer.empty[WorkspaceStructure]
  //var objectRefs = Map.empty[String, WorkspaceObject]
  var rule = Option.empty[Rule]

  var total_unhappiness = 0.0;
  var intra_string_unhappiness = 0.0;
  var inter_string_unhappiness = 0.0;

  var chaleur = 100.0;
  var actual_temperature = 100.0;
  var total_happiness_values = ListBuffer.empty[Double]
  var temperature_values = ListBuffer.empty[Double]
  var clamp_temperature = false;  // external clamp


  def objectRefs(): Map[String, WorkspaceObject] = {
    val subset = structureRefs.filter { case (k,v) => v.isInstanceOf[WorkspaceObject] }
    subset.asInstanceOf[Map[String, WorkspaceObject]]
  }



  def updateWorkspaceStringWithDescriptionReps(ws: WorkspaceString, wosToUpdate: List[WorkspaceObjectRep]) = {
    val letterRefs = ws.objects.map(_.uuid).zip(ws.objects).toMap
    for(woRep <- wosToUpdate) {
      val letter = letterRefs(woRep.uuid)
      for(d <- woRep.descriptions) {
        letter.add_description(d.descriptionType, d.descriptor)
      }
      build_descriptions(letter)
    }
  }

  def addStructure(ws: WorkspaceStructure): Unit = {
    structureRefs += (ws.uuid -> ws)
    structures += ws
  }
  def removeStructure(ws: WorkspaceStructure): Unit = {
    structureRefs -= ws.uuid
    structures -= ws

  }

  def addBond(b: Bond) = {
    addStructure(b)
    b.build_bond()
  }
  def break_bond(b: Bond) = {
    // GUI WorkspaceArea.DeleteObject(this);
    // GUI WorkspaceSmall.DeleteObject(this);
    removeStructure(b)
    b.break_bond()
  }
  def break_group(gr: Group): Unit = {
    for(d <- gr.descriptions) {
      break_description(d)
    }
    if (gr.group.isDefined) {
      break_group(gr.group.get)
    }
//   GUI  workspace.WorkspaceArea.DeleteObject(this);
//   GUI  workspace.WorkspaceSmall.DeleteObject(this);
    removeStructure(gr)

    // Now calculated    workspace.workspace_objects.removeElement(this);
    if (gr.correspondence.isDefined) break_correspondence(gr.correspondence.get)

    // check_visibility();
    if (gr.left_bond.isDefined) break_bond(gr.left_bond.get)
    if (gr.right_bond.isDefined) break_bond(gr.right_bond.get)

    // GUI workspace.WorkspaceArea.Redraw = true;

    gr.break_group()
  }

  def break_correspondence(c: Correspondence) = {
//  GUI  WorkspaceArea.DeleteObject(this);
//  GUI  WorkspaceSmall.DeleteObject(this);
    removeStructure(c)
    c.break_correspondence()
// GUI   workspace.WorkspaceArea.Redraw = true;
  }

  def break_description(d: Description) = {
    removeStructure(d)
    d.break_description()

// GUI   workspace.WorkspaceArea.DeleteObject(this);
// GUI   workspace.check_visibility();
// GUI   workspace.WorkspaceArea.Redraw=true;

  }


  def receive = LoggingReceive {


//    case SlipnodeActivationChanged(slipNodeID, activation) =>
//      for (structure <- structures) {
//        if (structure.isInstanceOf[Correspondence]) {
//          (structure.asInstanceOf[Correspondence]).slipNodeActivationChanged(slipNodeID, activation)
//        }
//      }

    case Initialize(initialS, modifiedS, targetS) =>
      coderack = sender()
      reset(initialS, modifiedS, targetS)
      slipnet ! InitializeWorkspaceStrings(
        initial.letterSlipnetComplements(),
        modified.letterSlipnetComplements(),
        target.letterSlipnetComplements()
      )

    case InitializeWorkspaceStringsResponse(initialDescriptions, modifiedDescriptions, targetDescriptions) =>
      updateWorkspaceStringWithDescriptionReps(initial, initialDescriptions)
      updateWorkspaceStringWithDescriptionReps(modified, modifiedDescriptions)
      updateWorkspaceStringWithDescriptionReps(target, targetDescriptions)
      coderack ! FinishInitilizingWorkspaceStrings

    case models.Workspace.UpdateEverything =>
      for (ws <- structures) {
        ws.update_strength_value()
      }

      // update the the object values of all objects in the workspace
      for (wo <- workspaceObjects()) {
        wo.update_object_value()
      }

      // update the relative importances of initial and target strings
      initial.update_relative_importance();
      target.update_relative_importance();

      // update the intra string unhappiness of initial and target strings
      initial.update_intra_string_unhappiness();
      target.update_intra_string_unhappiness();

      update_temperature()

    case Found =>
      found_answer = true


    case Step =>
      if (found_answer) {
        executionRunActor ! ExecutionRun.Found
      } else {
        coderack ! ChooseAndRun
      }

    // GoWithBreaker, see Codelet.java.68
    case GoWithBreaker(t) =>
      val candidateStructures = structures.filter(s => s.isInstanceOf[Group] ||
        s.isInstanceOf[Bond] ||
        s.isInstanceOf[Correspondence]
      )
      if (candidateStructures.isEmpty) {
        log.debug("There are no structures built: fizzle")
        sender() ! Finished
      } else {
        val wsize = candidateStructures.size
        // Codelet.java.84
        val p = (r.nextDouble() * wsize).toInt
        val pos = if (p >= wsize) 0 else p
        val ws = structures(pos);

        val probability = 1.0 - WorkspaceFormulas.temperature_adjusted_probability(ws.total_strength / 100.0, t)
        val st = if (ws.isInstanceOf[WorkspaceObject]) {
          val wo = ws.asInstanceOf[WorkspaceObject]
          if (wo.workspaceString().equals(initial)) " from initial string" else " from target string"
        } else ""
        log.debug(s"object chosen = ${ws}${st} break probability = ${probability}")


        val grOpt: Option[Group] = if (ws.isInstanceOf[Bond]) {
          val b = ws.asInstanceOf[Bond]
          val ob = b.from_obj
          val g = ob.group
          if (g == b.to_obj.group) {
            if (g.isDefined) g else None
          } else None
        } else None
        val break_objects = List(Some(ws), grOpt).flatten
        // break all objects
        for (w <- break_objects) {
          if (w.isInstanceOf[Bond]) {
            break_bond(w.asInstanceOf[Bond])
          };
          if (w.isInstanceOf[Group]) {
            break_group(w.asInstanceOf[Group])
          }
          if (w.isInstanceOf[Correspondence]) {
            break_correspondence(w.asInstanceOf[Correspondence])
          }
        }
        sender() ! Finished
      }

    // bottom-up-bond-scout in codelet.java.240
    case BondWithNeighbor(temperature) =>
      //          workspace_object fromob = workspace_formulas.choose_object("intra_string_salience",workspace.workspace_objects);
      val fromOpt = chooseObject(workspaceObjects(), TemperatureAjustmentVariable.Intra_string_salience, temperature)
      fromOpt match {
        case None =>
          log.debug("BondWithNeighbor | failed with empty from")
          sender() ! Finished
        case Some(from) =>
          val toOpt = chooseNeighbor(from, conditionalNeighbor)
          toOpt match {
            case None =>
              log.debug("BondWithNeighbor | object has no neighbour - fizzle")
              sender() ! Finished

            case Some(to) =>
              log.debug(s"initial object chosen: $from in ${if (from.workspaceString() == initial) "initial" else "target"} string")
              log.debug(s"ito object: $to")

              sender() ! GoWithBottomUpBondScoutResponse(from.workspaceObjectRep(), to.workspaceObjectRep())
          }
      }
      sender() ! Finished


    case GoWithBottomUpBondScout2(fromRep: WorkspaceStructureRep, toRep: WorkspaceStructureRep, fromFacets: List[SlipNodeRep], toFacets: List[SlipNodeRep]) =>
      // workspace_formulas.java.207
      if (fromFacets.isEmpty) {
        log.debug(s" no possible bond-facet - fizzle")
        sender() ! Finished

      } else {
        val from = objectRefs()(fromRep.uuid)
        val to = objectRefs()(toRep.uuid)
        val probs = toFacets.map(sn => {
          total_description_type_support(sn, from.workspaceString().get)
        })
        val index = Utilities.valueProportionalRandomIndexInValueList(probs)
        val bondFacet = toFacets(index)

        // bottom-up-bond-scout in codelet.java.258
        log.debug(s"chosen bond facet: ${bondFacet.id}")
        val fromDescriptorOpt = from.get_descriptor(bondFacet)
        val toDescriptorOpt = to.get_descriptor(bondFacet)
        if ((fromDescriptorOpt.isEmpty) || (toDescriptorOpt.isEmpty)) {
          log.debug(" no possible bond-facet - fizzle")
          sender() ! Finished

        } else {
          log.debug(s"from object descriptor: ${fromDescriptorOpt}")
          log.debug(s"to object descriptor: ${toDescriptorOpt}")

          // Here it goes to slipnet
          sender() ! GoWithBottomUpBondScout2Response(bondFacet, fromDescriptorOpt, toDescriptorOpt)
        }
      }
    case WorkspaceProposeBond(bondFromRep, bondToRep, bondCategory, bondFacet, fromDescriptor, toDescriptor, slipnetLeft, slipnetRight) =>
      val bondFrom = objectRefs()(bondFromRep.uuid)
      val bondTo = objectRefs()(bondToRep.uuid)
      val nb = new Bond(bondFrom, bondTo, bondCategory, bondFacet, fromDescriptor, toDescriptor, slipnetLeft, slipnetRight, slipnet)
      // if (!remove_terraced_scan) workspace.WorkspaceArea.AddObject(nb,1);

      sender ! WorkspaceProposeBondResponse(nb.uuid)

    // codelet.java.994
    case GoWithReplacementFinder =>
      val initialLetters = lettersOf(initial)
      val size = initialLetters.size
      val posBase = size * r.nextDouble()
      val pos = if (posBase >= size) size - 1.0 else posBase
      val i_letter = initialLetters(pos.toInt)
      log.debug(s"selected letter in initial string = ${i_letter}")
      if (i_letter.replacement.isDefined) {
        log.debug(s"replacement already found for this object. Fizzle!")
        sender() ! Finished

      } else {
        val position = i_letter.left_string_position
        val m_letterOpt = modified.objects.find(s => (
          s.isInstanceOf[Letter] &&
            s.left_string_position == position))
        m_letterOpt match {
          case None =>
            log.debug(s"Error -no corresponding letter could be found.Fizzle ! ")
            sender() ! Finished

          case Some(m_letter) =>
            val index = position - 1
            val modifiedChar: Char = modified.s(index)
            val relationOpt = initial.s(index) match {
              case mc if mc == modifiedChar => Some(Slipnet.RelationType.Sameness)
              case mc if mc == (modifiedChar - 1).toChar => Some(Slipnet.RelationType.Sameness)
              case mc if mc == (modifiedChar + 1).toChar => Some(Slipnet.RelationType.Predecessor)
              case _ => None
            }
            if (relationOpt.isDefined) {
              log.debug(s"Workspace | ReplaceLetter | ${relationOpt.get} relation found")
            } else {
              log.debug(s"Workspace | ReplaceLetter | no relation found")
            }
            i_letter.replacement = Some(new Replacement(i_letter, m_letter, relationOpt))
            if (relationOpt.isDefined && relationOpt.get == Slipnet.RelationType.Sameness) {
              i_letter.changed = true;
              changed_object = Some(i_letter)
            }
            log.debug(s"building replacement")
            sender() ! Finished

        }
      }


    // codelet.java.127
    case GoWithBottomUpDescriptionScout(t) =>
      val chosen_objectOpt: Option[WorkspaceObject] = chooseObjectFromList(workspaceObjects(), TemperatureAjustmentVariable.Total_salience)
      chosen_objectOpt match {
        case None =>
          log.debug("GoWithBottomUpDescriptionScout | failed with empty chosen object")
          sender() ! Finished

        case Some(chosen_object) =>
          log.debug(s"chosen object: ${chosen_object} from ${initialOrTargetText(chosen_object)} string")

          val dOpt = WorkspaceFormulas.choose_relevant_description_by_activation(chosen_object)
          dOpt match {
            case Some(d) =>
              val chosen_descriptorOpt = d.descriptor
              chosen_descriptorOpt match {
                case Some(chosen_descriptor) =>
                  log.debug("chosen descriptor = " + chosen_descriptor.id);
                  sender() ! GoWithBottomUpDescriptionScoutResponse(chosen_object.workspaceObjectRep(), chosen_descriptor)

                case None =>
                  log.debug(s"GoWithBottomUpDescriptionScout | Oups choosen description is not defined")
                  sender() ! Finished
              }

            case None =>
              log.debug("no relevant descriptions: Fizzle");
              sender() ! Finished
          }
      }
    case PrepareDescription(chosen_object, chosen_propertyRep, description_typeRep) =>
      val ob = objectRefs()(chosen_object.uuid)

      val descriptor = chosen_propertyRep
      val d = new models.Description(ob, description_typeRep, Some(descriptor))
      slipnet ! SetSlipNodeBufferValue(descriptor.id, 100.0)
      val urgency = description_typeRep.activation
      sender() ! PrepareDescriptionResponse(d.uuid, urgency)


    // codelet.java.165
    case GoWithTopDownDescriptionScout(descriptionTypeID, t) =>
      val chosen_objectOpt: Option[WorkspaceObject] = chooseObjectFromList(workspaceObjects(), TemperatureAjustmentVariable.Total_salience)
      chosen_objectOpt match {
        case None =>
          log.debug("GoWithTopDownDescriptionScout | failed with empty chosen_object")
          sender() ! Finished
        case Some(chosen_object) =>
          log.debug(s"chosen object: ${chosen_object} from ${initialOrTargetText(chosen_object)} string. looking for ${descriptionTypeID} descriptor")
          sender() ! GoWithTopDownDescriptionScoutResponse(chosen_object.workspaceObjectRep())
      }


    // codelet.java.173
    //
    case GoWithTopDownDescriptionScout2(chosen_objectRep, i: DescriptionTypeInstanceLinksToNodeInfo) =>
      val chosen_object = objectRefs()(chosen_objectRep.uuid)
      val v = chosen_object.get_possible_descriptions(i)
      if (v.isEmpty) {
        log.debug("couldn't find any descriptions");
        sender() ! Finished
      } else {
        val act = v.map(sn => sn.activation)
        val chosen_property = v(Utilities.valueProportionalRandomIndexInValueList(act))
        sender() ! GoWithTopDownDescriptionScoutResponse2(chosen_property)
      }

    case GoWithDescriptionBuilder(descriptionID, t) =>
      val d = structureRefs(descriptionID).asInstanceOf[Description]
      log.debug(d.toString());
      if (!workspaceObjects().contains(d.wObject)) {
        log.debug("object no longer exists: Fizzle!");
        sender() ! Finished
      } else {
        d.descriptor match {
          case Some(descriptor) =>
            if (d.wObject.has_slipnode_description(descriptor)) {
              print("description already exists: Fizzle!");

              slipnet ! SetSlipNodeBufferValue(d.descriptionType.id, 100.0)
              slipnet ! SetSlipNodeBufferValue(descriptor.id, 100.0)
              sender() ! Finished
            } else {
              log.info("building description");
              build_description(d)
            }
          case None =>
            log.debug("GoWithDescriptionBuilder descriptor is empty: Fizzle!");
            sender() ! Finished
        }
      }

    // codelet.java.1233
    case GoWithBottomUpCorrespondenceScout(t) =>
      val obj1Opt: Option[WorkspaceObject] = chooseObjectFromList(initial.objects.toList, TemperatureAjustmentVariable.Inter_string_salience)
      obj1Opt match {
        case None =>
          log.debug("GoWithBottomUpCorrespondenceScout | failed with empty obj1")
          sender() ! Finished

        case Some(obj1) =>
          val obj2Opt = chooseObjectFromList(target.objects.toList, TemperatureAjustmentVariable.Inter_string_salience)
          obj2Opt match {
            case None =>
              log.debug("GoWithBottomUpCorrespondenceScout | failed with empty obj2")
              sender() ! Finished

            case Some(obj2) =>
              log.debug(s"GoWithBottomUpCorrespondenceScout | trying a correspondence between $obj1 and $obj2")
              // if one object spans the string and the other doesn't - fizzle
              if (obj1.spans_string != obj2.spans_string) {
                // fizzle
                log.debug("GoWithBottomUpCorrespondenceScout | only one object spans the string: fizzle");
                sender() ! Finished

              } else {
                // He we continue in the slipnet
                // then the slipnet will potentially tell the workspace to create a Correspondence
                // then workspace tell the coderack to post a "correspondence-strength-tester" codelet

                sender() ! GoWithBottomUpCorrespondenceScoutWorkspaceReponse(
                  obj1.workspaceObjectRep(),
                  obj2.workspaceObjectRep()
                )
              }
          }
      }

    // Coderack.java.323
    case GoWithBottomUpCorrespondenceScout2(
    obj1Rep,
    obj2Rep,
    concept_mapping_list,
    flip_obj2,
    distiguishingConceptMappingSize,
    distiguishingConceptMappingTotalStrength,
    temperature
    ) =>
      val obj1 = objectRefs()(obj1Rep.uuid)
      val obj2 = objectRefs()(obj2Rep.uuid)

      val nc = new Correspondence(obj1, obj2, concept_mapping_list, flip_obj2);
      // TODO if (!remove_terraced_scan) WorkspaceArea.AddObject(nc,1);

      sender ! GoWithBottomUpCorrespondenceScout2Response(
        nc.uuid,
        distiguishingConceptMappingSize,
        distiguishingConceptMappingTotalStrength
      )

      // flipped case
    case GoWithBottomUpCorrespondenceScout3(futureGroup: FutureGroupRep, obj2) =>
      val obj2Group = objectRefs()(obj2.uuid).asInstanceOf[Group]
      val bond_list = futureGroup.bond_list.map(bondRep => structureRefs(bondRep.uuid).asInstanceOf[Bond]).to[ListBuffer]

      val newObj2 =
        new Group(
          obj2Group.wString.get,
          futureGroup.groupCategorySlipNodeID,
          futureGroup.directionCategorySlipNodeID,
          futureGroup.bondFacetSlipNodeID,
          obj2Group.object_list,
          bond_list,
          slipnet
        )
      structureRefs += (newObj2.uuid -> newObj2)


      sender() ! GoWithBottomUpCorrespondenceScout3Response(newObj2.workspaceObjectRep())

    // codelet.java.880
    case GoWithGroupBuilder(temperature, groupID) =>
      val g = objectRefs()(groupID).asInstanceOf[Group]
      logTrying(g, g)

      if (WorkspaceFormulas.group_present(g)) {
        print("already exists...activate descriptors & fizzle");
        g.activate_descriptions();
        val woOpt = WorkspaceFormulas.equivalent_group(g)
        woOpt match {
          case Some(wo) => addDescriptionsToWorkspaceObject(wo, g.descriptions.toList)
          case None =>
            log.debug("GoWithGroupBuilder stop with no equivalent group")
        }
        sender() ! Finished
      } else {
        // check to see if all objects are still there
        val objectNotThere = g.object_list.find(wo => !workspaceObjects().contains(wo))
        if (objectNotThere.isDefined) {
          print("objects no longer exist! - fizzle");
          sender() ! Finished
        } else {
          // check to see if bonds are there of the same direction
          val gObjectList = g.object_list
          val incompatibleBondListRaw = for (i <- 1 to gObjectList.size) yield {
            val ol = gObjectList(i)
            val bOpt = ol.left_bond
            if (bOpt.isDefined) {
              val b = bOpt.get
              val ob2 = b.left_obj
              if ((ob2 != gObjectList(i-1)) || (b.direction_category.map(_.id) != g.directionCategorySlipNodeID)) {
                Some(b)
              } else None
            } else None
          }
          val bondCandidate = if (gObjectList.size > 1) {
            val bOpt = gObjectList(0).right_bond
            if (bOpt.isDefined) {
              val b = bOpt.get
              val ob2 = b.right_obj
              if ((ob2 != gObjectList(1)) || (b.direction_category.map(_.id) != g.directionCategorySlipNodeID)) Some(b) else None
            } else None
          } else None

          val incompatibleBondList = (bondCandidate :: incompatibleBondListRaw.toList).flatten

          // if incompatible bonds exist - fight
          g.update_strength_value()
          val incbFightSucceeded = if (incompatibleBondList.size != 0) {
            print("fighting incompatible bonds");
            // try to break all incompatible groups
            if (fight_it_out(g,1.0, incompatibleBondList,1.0)){
              // beat all competing groups
              print("won!")
              true
            }
            else {
              print("couldn't break incompatible bonds: fizzle!");
              false
            }
          } else true

          if (incbFightSucceeded) {
            // fight incompatible groups
            // fight all groups containing these objects
            val incg = WorkspaceFormulas.get_incompatible_groups(g);
            val incgFightSucceeded = if (incg.size != 0){
              print("fighting incompatible groups");
              // try to break all incompatible groups
              if (fight_it_out(g,1.0, incg,1.0)){
                // beat all competing groups
                print("won");
                true
              }
              else {
                print("couldn't break incompatible groups: fizzle");
                false
              }
            } else true
            if (incgFightSucceeded) {
              // destroy incompatible bonds
              for (b <- incompatibleBondList) b.break_bond()

              // create new bonds
              g.bond_list = ListBuffer.empty[Bond]

              self ! LookAHeadForNewBondCreation(sender(), g.uuid, 1, incg.map(_.uuid), List.empty[BondRep])

            } else {
              sender() ! Finished
            }

          } else {
            sender() ! Finished
          }
        }
      }

    case LookAHeadForNewBondCreation(s: ActorRef, groupID, i, incg, newBondList) =>
      val g = objectRefs()(groupID).asInstanceOf[Group]
      if (i < g.object_list.size) {
        val ob1 = g.object_list(i-1)
        val ob2 = g.object_list(i)

        if (ob1.right_bond.isEmpty){
          val isForward = g.directionCategorySlipNodeID.isDefined && g.directionCategorySlipNodeID.get == SlipNode.id.right
          val from_obj = if (isForward) ob1 else ob2
          val to_obj = if (isForward) ob2 else ob1

          slipnet ! SlipnetLookAHeadForNewBondCreation(s, g.groupRep(), i, incg, newBondList, from_obj.uuid, to_obj.uuid)

        } else {
          self ! LookAHeadForNewBondCreation(s, groupID, i + 1, incg, ob1.right_bond.get.bondRep() :: newBondList)
        }
      } else {
        // destroy incompatible groups
        for (gr <- incg) {
          val grp = objectRefs()(gr).asInstanceOf[Group]
          break_group(grp)
        }

        build_group(g)
// already done in build_group        g.activate_descriptions();
        print("building group");
        s ! Finished
      }

    case SlipnetLookAHeadForNewBondCreationResponse(s, g_rep, i, incg, newBondList, bond_categoryOpt, from_obj_id, to_obj_id, bond_facet,slipnetLeft,
    slipnetRight) =>
      bond_categoryOpt match {
        case Some(bond_category) =>
          val from_obj = objectRefs()(from_obj_id)
          val to_obj = objectRefs()(to_obj_id)
          val g = objectRefs()(g_rep.workspaceObjectRep.uuid).asInstanceOf[Group]
          val from_obj_descriptor = from_obj.get_description(bond_facet)
          val to_obj_descriptor = to_obj.get_description(bond_facet)

          val nb = new Bond(from_obj,to_obj,bond_category,bond_facet, from_obj_descriptor,to_obj_descriptor, slipnetLeft,slipnetRight, slipnet)
          build_bond(nb)
          self ! LookAHeadForNewBondCreation(s, g.uuid, i + 1, incg, nb.bondRep() :: newBondList)

        case None =>
          println("Big mess")
          s ! Finished
      }

    // Codelet.java.425
    case GoWithBondBuilder(temperature, bondID) =>
      val b = objectRefs()(bondID).asInstanceOf[Bond]
      logTrying(b, b.left_obj)

      b.update_strength_value();
      print("strength = " + b.total_strength);
      //val competitors = b.get_incompatible_bonds();

      if ((workspaceObjects().contains(b.from_obj)) &&
        (workspaceObjects().contains(b.to_obj))) {
        val existingBonds: List[Bond] = b.workspaceString() match {
          case Some(wstring) =>
            wstring.bonds().filter(b2 =>
              (
                (b.left_obj == b2.left_obj) && (b.right_obj == b2.right_obj)
                ) &&
                // check to see if this is the same bond
                (b.direction_category == b2.direction_category) &&
                (b.bond_category == b2.bond_category)
            )
          case None => List.empty[Bond]
        }

        if (!existingBonds.isEmpty) {
          // bond already exists
          b.activateDescriptor()
          print("already exists: activate descriptors & Fizzle!");
          sender() ! Finished
        } else {
          // check for incompatible structures
          val incb = b.get_incompatible_bonds();
          val brokeIncb = if (!incb.isEmpty) {
            log.debug("trying to break incompatible bonds");
            // try to break all incompatible bonds
            // Side-effect !
            if (fight_it_out(b, 1.0, incb, 1.0)) {
              // beat all competing bonds
              log.debug("won, beat all competing bonds");
              true
            } else {
              log.debug("failed: Fizzle!");
              false;
            }
          } else {
            print("no incompatible bonds!")
            true
          }
          if (!brokeIncb) {
            sender() ! Finished
          } else {
            // fight all incompatible correspondences
            var incc: List[Correspondence] = if (b.left_obj.leftmost || b.right_obj.rightmost) {
              // ignore if (b.direction_category!=null) {
              //System.out.println("looking for incompatible correspondences")
              b.get_incompatible_correspondences(initial)
            } else List.empty[Correspondence]

            if (!incc.isEmpty) {
              log.debug("trying to break incompatible correspondences")
              if (!fight_it_out(b, 2.0, incc, 3.0)) {
                log.debug("lost the fight: Fizzle!")
                sender() ! Finished
              } else {
                log.debug("won")

                // fight all groups containing these objects
                val incg = WorkspaceFormulas.get_common_groups(b.from_obj, b.to_obj);
                if (incg.isEmpty) {
                  log.debug("no incompatible groups!")
                }
                val failedFight = if (incg.isEmpty) false else {
                  log.debug("trying to break incompatible groups")
                  // try to break all incompatible groups
                  if (fight_it_out(b, 1.0, incg, 1.0)) {
                    // beat all competing groups
                    log.debug("won");
                    false
                  }
                  else {
                    print("failed: Fizzle!");
                    true
                  }
                }
                if (failedFight) {
                  sender() ! Finished
                } else {

                  for (br <- incb) {
                    break_bond(br)
                  }
                  for (gr <- incg) {
                    break_group(gr)
                  }
                  for (c <- incc) {
                    break_correspondence(c)
                  }
                  print("building bond");
                  addBond(b)
                  sender() ! Finished
                }
              }
            } else {
              sender() ! Finished
            }
          }
        }
      } else {
        log.debug("objects do no longer exists: Fizzle!");
        sender() ! Finished
      }

    case GoWithDescriptionStrengthTester(temperature, descriptionID) =>
      slipnet ! SetSlipNodeBufferValue(descriptionID, 100.0)
      val d = objectRefs()(descriptionID).asInstanceOf[Description]
      d.update_strength_value()

      val strength = d.total_strength
      log.debug("GoWithDescriptionStrengthTester" + d.toString())

      val prob = WorkspaceFormulas.temperature_adjusted_probability(strength / 100.0, temperature)
      log.debug("GoWithDescriptionStrengthTester description strength = " + strength);

      if (!WorkspaceFormulas.flip_coin(prob)) {
        log.debug("not strong enough: Fizzle!");
        sender() ! Finished
      }
      // it is strong enough - post builder  & activate nodes

      log.debug("succeeded: posting description-builder")
      sender() ! GoWithDescriptionStrengthTesterResponse(strength)


    // codelet.java.395
    case GoWithBondStrengthTester(temperature, bondID) =>
      val b = objectRefs()(bondID).asInstanceOf[Bond]
      b.update_strength_value()
      val strength = b.total_strength;
      val workingString = if (b.left_obj.wString == initial) "initial" else "target"
      log.info(s"bond = ${bondID} in ${workingString} string")

      val prob = WorkspaceFormulas.temperature_adjusted_probability(strength / 100.0, temperature)
      log.info("bond strength = " + strength)
      if (!WorkspaceFormulas.flip_coin(prob)) {
        log.debug("not strong enough: Fizzle!");
        sender() ! Finished
      }
      // it is strong enough - post builder  & activate nodes
      slipnet ! SetSlipNodeBufferValue(b.bond_facet.id, 100.0)
      if (b.from_obj_descriptor.isDefined)
        slipnet ! SetSlipNodeBufferValue(b.from_obj_descriptor.get.id, 100.0)
      if (b.to_obj_descriptor.isDefined)
        slipnet ! SetSlipNodeBufferValue(b.to_obj_descriptor.get.id, 100.0)

      log.info("succeeded: will post bond-builder");
      sender() ! GoWithBondStrengthTesterResponse(strength)


    // codelet.java.278
    case GoWithTopDownBondScoutCategory(bondCategoryID: String, temperature) =>
      log.info("searching for " + bondCategoryID);
      val i_relevance = WorkspaceFormulas.local_relevance(initial, Some(bondCategoryID), (b: Bond) => Some(b.bond_category))
      val t_relevance = WorkspaceFormulas.local_relevance(target, Some(bondCategoryID), (b: Bond) => Some(b.bond_category))

      val fromOpt = chooseObjectWith(bondCategoryID, i_relevance, t_relevance, temperature)
      fromOpt match {
        case None =>
          log.debug("GoWithTopDownBondScoutWith | failed with empty from")
          sender() ! Finished
        case Some(fromob) =>
          // choose neighbour
          print("initial object: " + fromob);

          val toOpt = chooseNeighbor(fromob, conditionalNeighbor)
          toOpt match {
            case None =>
              log.debug("GoWithTopDownBondScoutCategory | object has no neighbour - fizzle")
              sender() ! Finished

            case Some(toob) =>
              //log.debug(s"initial object chosen: $from in ${if (from.workspaceString() == initial) "initial" else "target"} string")
              // log.debug(s"ito object: $to")
              log.debug("to object : " + toob);

              // workspace_formulas.choose_bond_facet
              val fromdtypes = fromob.descriptions.toList.map(d => d.descriptionType)
              val todtypes = toob.descriptions.toList.map(d => d.descriptionType)

              sender() ! GoWithTopDownBondScoutWithResponse(
                fromob.workspaceObjectRep(),
                toob.workspaceObjectRep(),
                fromdtypes,
                todtypes
              )
          }
      }

    case GoWithTopDownBondScout2(fromobrep, toobrep, bond_facets) =>
      val fromob = objectRefs()(fromobrep.uuid)
      fromob.wString match {
        case Some(fromobString) =>
          val object_probs = bond_facets.map(ob => {
            total_description_type_support(ob, fromobString)
          })
          val bond_facet = bond_facets(Utilities.valueProportionalRandomIndexInValueList(object_probs))
          print("chosen bond facet :" + bond_facet.id)
          val toob = objectRefs()(toobrep.uuid)

          val fromDescriptorOpt = fromob.get_descriptor(bond_facet)
          val toDescriptorOpt = toob.get_descriptor(bond_facet)

          if ((fromDescriptorOpt.isEmpty) || (toDescriptorOpt.isEmpty)) {
            log.debug("both objects do not have this descriptor: Fizzle!")
            sender() ! Finished
          } else {
            log.debug("from object descriptor: " + fromDescriptorOpt)
            log.debug("to object descriptor: " + toDescriptorOpt)
            sender() ! GoWithTopDownBondScout2Response(bond_facet, fromDescriptorOpt, toDescriptorOpt)

          }

        case None =>
          sender() ! Finished
      }
    // codelet.java.340
    case GoWithTopDownBondScoutDirection(directionID: String, temperature) =>
      log.info("searching for " + directionID);
      val i_relevance = WorkspaceFormulas.local_relevance(initial, Some(directionID), (b: Bond) => b.direction_category)
      val t_relevance = WorkspaceFormulas.local_relevance(target, Some(directionID), (b: Bond) => b.direction_category)

      val fromOpt = chooseObjectWith(directionID, i_relevance, t_relevance, temperature)
      fromOpt match {
        case None =>
          log.debug("GoWithTopDownBondScoutWith | failed with empty from")
          sender() ! Finished
        case Some(fromob) =>
          // choose neighbour
          print("initial object: " + fromob);
          val conditional = if (directionID == "lf") conditionalLeftNeighbor else conditionalRightNeighbor
          val toOpt = chooseNeighbor(fromob, conditional)
          toOpt match {
            case None =>
              log.debug("GoWithTopDownBondScoutCategory | object has no neighbour - fizzle")
              sender() ! Finished

            case Some(toob) =>
              //log.debug(s"initial object chosen: $from in ${if (from.workspaceString() == initial) "initial" else "target"} string")
              // log.debug(s"ito object: $to")
              log.debug("to object : " + toob);

              // workspace_formulas.choose_bond_facet
              val fromdtypes = fromob.descriptions.toList.map(d => d.descriptionType)
              val todtypes = toob.descriptions.toList.map(d => d.descriptionType)

              sender() ! GoWithTopDownBondScoutWithResponse(
                fromob.workspaceObjectRep(),
                toob.workspaceObjectRep(),
                fromdtypes,
                todtypes
              )
          }
      }
    case GoWithTopDownGroupScoutCategory(slipNodeID: String, bondFocus: String, t) =>
      val bondFocusing = bondFocus match {
        case "bond_category" => (b: Bond) => Some(b.bond_category)
        case "direction_category" => (b: Bond) => b.direction_category
        case _ => (b: Bond) => Some(b.bond_category)
      }
      val i_relevance = WorkspaceFormulas.local_relevance(initial, Some(slipNodeID), bondFocusing)
      val t_relevance = WorkspaceFormulas.local_relevance(target, Some(slipNodeID), bondFocusing)

      val fromOpt = chooseObjectWith(slipNodeID, i_relevance, t_relevance, t)
      fromOpt match {
        case None =>
          log.debug("GoWithTopDownBondScoutWith | failed with empty from")
          sender() ! Finished
        case Some(fromob) =>
          log.debug("object chosen: " + fromob);
          if (fromob.spans_string) {
            log.debug("chosen object spans the string. fizzle");
            sender() ! Finished
          } else {
            val direction = if (fromob.leftmost) DirValue.Right else if (fromob.rightmost) DirValue.Left else DirValue.None
            sender() ! GoWithTopDownGroupScoutCategoryResponse(direction, fromob.workspaceObjectRep())
          }
      }
    case GoWithTopDownGroupScoutCategory2(groupID: String, direction, fromobrep, bond_category, t, lengthActivation) =>
      var fromob = objectRefs()(fromobrep.uuid)
      val first_bondOpt = if (direction == DirValue.Left) fromob.left_bond else fromob.right_bond
      if ((first_bondOpt.isEmpty) || (first_bondOpt.get.bond_category != bond_category)) {
        // check the other side of object
        val newFirst_bondOpt = if (direction == DirValue.Right) fromob.left_bond else fromob.right_bond
        if ((newFirst_bondOpt.isEmpty) || (newFirst_bondOpt.get.bond_category != bond_category)) {
          // this is a single letter group
          if ((bond_category.id != "sm") || (!(fromob.isInstanceOf[Letter]))) {
            print("no bonds of this type found: fizzle!");
            sender() ! Finished
          }
          else {
            print("thinking about a single letter group");
            print("thinking about a single letter group");
            val oblist = ListBuffer(fromob)
            val samegrp = "smg"
            val letter_category = "lc"
            val g = new Group(
              fromob.wString.get,
              samegrp,
              None,
              letter_category,
              oblist,
              ListBuffer.empty[Bond],
              slipnet)

            val prob = g.single_letter_group_probability(lengthActivation, t);
            if (r.nextDouble() < prob) {
              // propose single letter group
              print("single letter group proposed");


              val group_category = samegrp
              val direction_category = Option.empty[String]
              val bond_facet = letter_category
              val object_list = oblist.toList.map(_.workspaceObjectRep())
              val bond_list = List.empty[BondRep]

              sender() ! GoWithTopDownGroupScoutCategory2Response(
                group_category,
                direction_category,
                bond_facet,
                object_list,
                bond_list
              )
            } else {
              print("failed")
              sender() ! Finished
            }
          }
        }
      } else {
        self.forward(CommonSubProcessing(groupID, fromob.uuid,first_bondOpt.get.uuid, bond_category))
      }

    case CommonSubProcessing(id: String, fromobUUID, firstBondUUID, bond_category: SlipNodeRep) =>
      var fromob = objectRefs()(fromobUUID)
      var first_bond = structureRefs(firstBondUUID).asInstanceOf[Bond]
      var direction = first_bond.direction_category;
      var bond_facetOpt: Option[SlipNodeRep] = None
      //var object_list = new Vector();
      //var bond_list = new Vector();
      // find leftmost object in group with these bonds
      var search = true
      while (search) {
        search = false;
        if (fromob.left_bond.isDefined) {
          //val left_bond = slipNodeRefs(fromob.left_bond.get.
          val left_bond = fromob.left_bond.get
          if (left_bond.bond_category.id == bond_category.id) {
            if ((left_bond.direction_category.isEmpty) ||
              (left_bond.direction_category == direction)) {
              if ((bond_facetOpt.isEmpty) || (bond_facetOpt.get == left_bond.bond_facet)) {
                bond_facetOpt = Some(left_bond.bond_facet)
                direction = left_bond.direction_category
                fromob = left_bond.left_obj
                search = true
              }
            }
          }
        }
      }
      // find rightmost object in group with these bonds
      search = true
      var toob = fromob
      while (search) {
        search = false;
        if (toob.right_bond.isDefined) {
          val right_bond = toob.right_bond.get
          if (right_bond.bond_category.id == bond_category.id) {
            if ((right_bond.direction_category.isEmpty) ||
              (right_bond.direction_category == direction)) {
              if ((bond_facetOpt.isEmpty) || (bond_facetOpt.get == right_bond.bond_facet)) {
                bond_facetOpt = Some(right_bond.bond_facet)
                direction = right_bond.direction_category
                toob = right_bond.right_obj;
                search = true;
              }
            }
          }
        }
      }
      if (bond_facetOpt.isEmpty) {
        print("bond_facet is empty - fizzle");
        sender() ! Finished
      } else {
        val bond_facet = bond_facetOpt.get
        if (toob == fromob) {
          print("no possible group - fizzle");
          sender() ! Finished
        } else {
          print("proposing group from " + fromob + " to " + toob);
          var object_list = ListBuffer(fromob)
          var bond_list = ListBuffer.empty[Bond]
          breakable {
            while (fromob != toob) {
              if (fromob.right_bond.isDefined) {
                val right_bond = fromob.right_bond.get
                bond_list += right_bond
                object_list += right_bond.right_obj
                fromob = right_bond.right_obj
              } else break
            }
          }

          val object_rep_list = object_list.toList.map(_.workspaceObjectRep())
          val bond_rep_list = bond_list.toList.map(_.bondRep())
          sender() ! GoWithTopDownGroupScoutCategory2Response(
            id,
            direction.map(_.id),
            bond_facet.id,
            object_rep_list,
            bond_rep_list
          )
        }
      }


    case WorkspaceProposeGroup(
    object_rep_list: List[WorkspaceStructureRep],
    bls: List[WorkspaceStructureRep],
    group_category: String,
    direction_category: Option[String],
    bond_facet: String,
    ) =>
      val wStringFromWo = object_rep_list.head
      val wString = objectRefs()(wStringFromWo.uuid).wString.get
      val object_list = object_rep_list.map(ol => objectRefs()(ol.uuid)).to[ListBuffer]
      val bond_list = bls.map(ol => structureRefs(ol.uuid).asInstanceOf[Bond]).to[ListBuffer]


      val ng = new Group(wString, group_category, direction_category, bond_facet, object_list, bond_list, slipnet)
      sender ! WorkspaceProposeGroupResponse(ng.uuid)

    case GoWithTopDownGroupScoutDirection(directionID: String, direction: SlipNodeRep, fromobrep: WorkspaceObjectRep, t) =>
      var fromob = objectRefs()(fromobrep.uuid)
      var newDirection: Option[SlipNodeRep] = Some(direction)
      var fizzle = false

      val first_bondOpt = if (direction == DirValue.Left) fromob.left_bond else fromob.right_bond
      if ((first_bondOpt.isDefined) && (first_bondOpt.get.direction_category.isEmpty)) {
        newDirection = None
      }
      if ((first_bondOpt.isEmpty) || (first_bondOpt.isDefined && first_bondOpt.get.direction_category != direction)) {
        val newFirst_bondOpt = if (direction == DirValue.Right) fromob.left_bond else fromob.right_bond
        if ((newFirst_bondOpt.isEmpty) || (newFirst_bondOpt.get.direction_category.isEmpty)) {
          newDirection = None
        }
        if ((first_bondOpt.isEmpty) || (first_bondOpt.isDefined && first_bondOpt.get.direction_category != direction)) {
          fizzle = true
        }
      }
      if (fizzle) {
        log.debug("no possible group: fizzle!")
        sender() ! Finished
      } else {
        val bond_category = first_bondOpt.get.bond_category;

        // Is this possible ?
//        if (bond_category==null){
//          print("no bond in the "+direction.pname+" direction was found: fizzle.");
//          return false;
//        }
        sender() ! GoWithTopDownGroupScoutDirectionResponse(bond_category, first_bondOpt.get.uuid)
      }

    case GoWithTopDownGroupScoutDirection2(group_categoryOpt, fromob, firstBondUUID, bond_category: SlipNodeRep) =>
      group_categoryOpt match {
        case Some(group_category) =>
          self.forward(CommonSubProcessing(group_category.id, fromob.uuid, firstBondUUID, bond_category))
        case None =>
          print("Look's like we can't go this way ? : fizzle!")
          sender() ! Finished
      }

    case GoWithGroupScoutWholeString(t: Double) =>
      log.debug("about to choose string");
      val wString = if (r.nextDouble() > 0.5) target else initial
      val wStringText = if (wString == initial) "initial" else "target"
      log.debug(s"${wStringText} string selected")

      // find leftmost object & the highest group to which it belongs
      val leftmost = wString.objects.find(w => w.leftmost).get

      // Codelet.java.805
      sender() ! GoWithGroupScoutWholeStringResponse(leftmost.workspaceObjectRep())

      // leftmost.group.map(_.groupRep())
    //   Codelet.java.807
    case GoWithGroupScoutWholeString2(left_mostRep, slipnetLeft, slipnetRight) =>
      var leftmost = objectRefs()(left_mostRep.uuid)
      if (leftmost.spans_string){
        // the object already spans the string - propose this object
        val g = leftmost.asInstanceOf[Group]

        print("selected object already spans string: propose");

        sender() ! GroupScoutWholeString2Response(
          g.groupCategorySlipNodeID,
          g.directionCategorySlipNodeID,
          g.bondFacetSlipNodeID,
          g.object_list.toList.map(_.workspaceObjectRep()),
          g.bond_list.toList.map(_.bondRep())
        )
      } else {
        var bond_list = ListBuffer.empty[Bond]
        var object_list = ListBuffer(leftmost)

        while (leftmost.right_bond.isDefined){
          val right_bond = leftmost.right_bond.get
          bond_list += right_bond
          leftmost = right_bond.right_obj
          object_list += leftmost
        }
        if (!(leftmost.rightmost)){
          print("no spanning bonds - fizzle");
          sender() ! Finished
        } else {
          // choose a random bond from list
          val posRaw = (r.nextDouble() * bond_list.size).toInt
          val pos = if (posRaw >= bond_list.size) 0 else posRaw

          val chosen_bond = bond_list(pos)
          val bond_category = chosen_bond.bond_category
          val direction_category = chosen_bond.direction_category;
          val bond_facet = chosen_bond.bond_facet;
          val bond_listOpt = WorkspaceFormulas.possible_group_bond_list(bond_category,
            direction_category, bond_facet, bond_list.toList, slipnetLeft, slipnetRight, slipnet)
          if (bond_listOpt.isEmpty){
            print("no possible group - fizzle");
            sender() ! Finished
          } else {
            print("proposing "+bond_category.id+" group");

            sender() ! GroupScoutWholeString3Response(
              bond_category.id,
              direction_category.map(_.id),
              bond_facet.id,
              object_list.toList.map(_.workspaceObjectRep()),
              bond_list.toList.map(_.bondRep())
            )
          }


        }

      }

    case GoWithGroupStrengthTester(temperature: Double, groupID: String) =>
      val g = objectRefs()(groupID).asInstanceOf[Group]
      g.update_strength_value()
      val strength = g.total_strength;
      val workingString = if (g.wString == initial) "initial" else "target"
      log.info(s"evaluating group = ${groupID} in ${workingString} string")

      val prob = WorkspaceFormulas.temperature_adjusted_probability(strength / 100.0, temperature)
      log.info(s"strength = $strength, adjusted prob.= $prob")
      if (r.nextDouble() > prob){
        print("not strong enough: fizzled!");
        sender() ! Finished
      } else {
        // it is strong enough - post builder  & activate nodes
        sender() ! GoWithGroupStrengthTesterResponse(g.groupRep(), strength)

      }
    case GoWithRuleScout =>
      if (unreplaced_objects().size != 0) {
        print("not all replacements have been found. Fizzle");
        sender() ! Finished
      } else {
        var changedOpt: Option[WorkspaceObject] = None
        // find changed object;
        for (wo <- initial.objects) {
          if (wo.changed) changedOpt = Some(wo)
        }
        // if there are no changed objects, propose a rule with no changes
        if (changedOpt.isEmpty) {
          print("there are no changed objects!");
          print("proposing null rule");
          val facet = Option.empty[String]
          val description = Option.empty[String]
          val objectCategory = Option.empty[String]
          val relation = Option.empty[String]

          sender() ! RuleScoutProposeRule(None, None, None, None)
        } else {
          // generate a list of distinguishing descriptions for the first object
          // ie. string-position (leftmost,rightmost,middle or whole) or letter category
          // if it is the only one of its type in the string
          val changed = changedOpt.get
          sender() ! GoWithRuleScoutResponse(changed.workspaceObjectRep())
        }

      }

    case WorkspaceProposeRule(facet, description, objectCategory, relation, lengthSplipNode, predecessorSlipNode, successorSlipNode) =>
      val r = new Rule(facet, description, objectCategory, relation, slipnet, lengthSplipNode, predecessorSlipNode, successorSlipNode)
      structureRefs += (r.uuid -> r)
      sender() ! WorkspaceProposeRuleResponse(r.uuid)

    case GoWithRuleScout2(changedRep, string_position_category, letter_category) =>
      val changed = objectRefs()(changedRep.uuid)
      var object_list = ListBuffer.empty[SlipNodeRep]
      val positionOpt = changed.get_description(string_position_category);
      if (positionOpt.isDefined) {
        object_list += positionOpt.get
      };
      val letterOpt = changed.get_description(letter_category)
      val noLetterFound = initial.objects.find(wo => (wo.get_description_type(letterOpt).isDefined)&&(wo!=changed))
      var only_letter = noLetterFound.isEmpty;  // if this is true, the letter can be thought of as a distinguishing feature
      if (only_letter) object_list += letterOpt.get

      // if this object corresponds to another object in the workspace
      // object_list = the union of this and the distingushing descriptors
      if (changed.correspondence.isDefined) {

        if (changed.replacement.isEmpty) {
          println("Changed replacement is empty. Fizzle")
          sender() ! Finished
        } else {
          val replacement = changed.replacement.get
          val letterCategory = replacement.to.get_description_with_id(SlipNode.id.letter_category)
          if (letterCategory.isEmpty) {
            println("Changed replacement get_description_with_id letter_category is empty. Fizzle")
            sender() ! Finished
          } else {
            val obj2 = changed.correspondence.get.obj2
            val slippagesShell = slippage_list()

            sender() ! GoWithRuleScout2Response(
              obj2.workspaceObjectRep(),
              slippagesShell,
              object_list.toList,
              replacement.relation,
              letterCategory.get
            )
          }
        }
      } else {
        sender() ! GoWithRuleScout3Response(object_list.toList.map(_.id))
      }

    case GoWithRuleScout3(slippage_list_rep, object_list,obj2_rep) =>
      val obj2 = objectRefs()(obj2_rep.uuid)
      val new_object_list = object_list.map(s => {
        val sID = Rule.apply_slippages(Some(s.id), slippage_list_rep)
        if (sID.isDefined && obj2.has_slipnode_description_with_id(sID.get) && obj2.distinguishing_descriptor(sID.get))
            Some(sID.get)
        else None
      }).flatten
      sender() ! GoWithRuleScout3Response(new_object_list)

    case GoWithRuleStrengthTester(temperature: Double, ruleID) =>
      val rule = structureRefs(ruleID).asInstanceOf[Rule]
      println("testing: "+rule.toString())

      rule.update_strength_value()
      val strength = rule.total_strength
      val prob = WorkspaceFormulas.temperature_adjusted_probability(strength / 100.0, temperature)
      log.info(s"strength = $strength, adjusted prob.= $prob")
      if (r.nextDouble() > prob){
        print("not strong enough: fizzled!");
        sender() ! Finished
      } else {
        // it is strong enough - post builder  & activate nodes
        sender() ! GoWithRuleStrengthTesterResponse(rule.workspaceStructureRep(), strength)
      }

    case GoWithRuleBuilder(ruleID) =>
      val myrule = structureRefs(ruleID).asInstanceOf[Rule]
      println("trying to build "+r.toString());
      if (myrule.rule_equal(rule)){
        // rule already exists: fizzle, but activate concepts
        print("already exists - activate concepts");
        myrule.activate_rule_descriptions()
        sender() ! Finished
      } else {
        myrule.update_strength_value()
        val strength = myrule.total_strength
        if (strength == 0.0) {
          print("the rule is incompatible with correspondences: Fizzle");
          sender() ! Finished
        } else {
          // fight against other rules
          if (rule.isDefined){
            print("Fighting against existing rule");
            print("existing rule strength: "+ rule.get.total_strength);
            print("this rule strength: "+ rule.get.total_strength);
            if (!structure_vs_structure(
              myrule,1.0,rule.get,1.0)){
              // lost the fight
              println("lost the fight: fizzle!");
              sender() ! Finished
            }
            else println("won!");
          }
          build_rule(myrule)
          print("building rule");

        }
      }
    case GoWithRuleTranslator(t) =>

      if (rule.isEmpty){
        println("Empty rule: fizzle!");
        sender() ! Finished
      } else {
        val bond_densityRaw = if (
          (initial.length == 1) &&
          (target.length == 1)
        ) 1.0 else {
          (initial.bonds.size + target.bonds.size).toDouble / (initial.length + target.length-2).toDouble
        }

        val bond_density = if (bond_densityRaw > 1.0) 1.0 else bond_densityRaw
        println("bond density : "+bond_density)
        val distribution = if (bond_density>0.8)
          WorkspaceFormulas.very_low_distribution
        else if (bond_density>0.6)
          WorkspaceFormulas.low_distribution
        else if (bond_density>0.4)
          WorkspaceFormulas.medium_distribution
        else if (bond_density>0.2)
          WorkspaceFormulas.high_distribution;
        else WorkspaceFormulas.very_high_distribution

        val cutoff = WorkspaceFormulas.choose(distribution) * 10.0
        println("temperature cutoff = "+cutoff)
        if (cutoff < t) { // formulas.actual_temperature = t ???
          // not high enough
          println("not high enough: Fizzle")
          sender() ! Finished
        } else {
          print("building translated rule!");
          print("Slippages used in translation:");
          val slippagesShell = slippage_list()

          sender() ! GoWithRuleTranslatorResponse(slippagesShell)
        }
      }
    case GoWithRuleTranslator2(slippage_list_rep) =>
      val found_answerOpt = rule.get.build_translated_rule(slippage_list_rep, target.s, target.objects.toList)

      if (found_answerOpt.isDefined) {
        println("Found answer " + found_answerOpt.get)
        found_answer = true
      } else {

        // How to do that ????
//        Temperature.clamp_time = coderack.codelets_run+100;
//        Temperature.clamped = true;
//        formulas.temperature = 100.0;
      }
    case GoWithImportantObjectCorrespondenceScout(t) =>
      val obj1Opt = chooseObject(initial.objects.toList, TemperatureAjustmentVariable.Relative_importance, t)

      obj1Opt match {
        case Some(obj1) =>
          print("object chosen from initial string: "+obj1);
          val v = obj1.relevant_distinguishing_descriptors()
          sender() ! GoWithImportantObjectCorrespondenceScoutResponse(obj1.workspaceObjectRep(), v)

        case None =>
          println("Popopo obj1 is null: Fizzle")
          sender() ! Finished
      }

    case GoWithImportantObjectCorrespondenceScout2 =>
      val sShell = slippage_list()
      sender() ! GoWithImportantObjectCorrespondenceScout2Response(sShell)

    case GoWithImportantObjectCorrespondenceScout3(slippage_list_rep: List[ConceptMappingRep], s, t, o1) =>
      val obj1_descriptorRaw = slippage_list_rep.find(cm => {
        cm.descriptor1SlipNodeID == s.id
      }).map(_.descriptor2SlipNodeID)
      val obj1_descriptor = if (obj1_descriptorRaw.isEmpty) s.id else obj1_descriptorRaw.get

      val obj2_candidates = target.objects.toList.filter(wo => {
        wo.relevant_descriptions().find(d => {
          d.descriptor.isDefined && d.descriptor.get.id == obj1_descriptor
        }).isDefined
      })
      if (obj2_candidates.isEmpty) {
        println("no corresponding objects found: fizzle")
        sender() ! Finished
      } else {
        val obj2Opt = chooseObject(obj2_candidates, TemperatureAjustmentVariable.Inter_string_salience, t)

        obj2Opt match {
          case Some(obj2) =>
            var flip_obj2 = false;
            val obj1 = objectRefs()(o1.uuid)
            print("trying a correspondence between "+obj1+" and "+obj2);

            // if one object spans the string and the other doesn't - fizzle
            if (obj1.spans_string!=obj2.spans_string){
              // fizzle
              print("only one object spans the string: fizzle");
              sender() ! Finished
            } else {
              sender() ! GoWithImportantObjectCorrespondenceScout3Response(obj1.workspaceObjectRep())
            }

          case None =>
            println("Should never happen ? but obj2 is null: fizzle")
            sender() ! Finished
        }

      }


  }

  def build_rule(r: Rule): Unit = {
    // GUI workspace.Workspace_Rule.Change_Caption("rule : " +this.toString());
    if (rule.isDefined) {
      removeStructure(rule.get)
    }
    rule = Some(r);
    addStructure(r);

    r.build_rule()
  }

  def unreplaced_objects(): List[WorkspaceObject] = {
    // returns a list of all objects in the initial string that are not
    // replaced
    workspaceObjects().filter(wo => {
      val ok =((wo.wString.isDefined && wo.wString == initial) && (wo.isInstanceOf[Letter]))
      ok && wo.replacement.isEmpty
    })
  }

  val conditionalNeighbor = (from: WorkspaceObject, wo: WorkspaceObject) => (
    (wo.left_string_position == from.right_string_position + 1) ||
      (from.left_string_position== wo.right_string_position+1)
    )
  val conditionalRightNeighbor = (from: WorkspaceObject, wo: WorkspaceObject) => wo.left_string_position == (from.right_string_position+1)
  val conditionalLeftNeighbor = (from: WorkspaceObject, wo: WorkspaceObject) => from.left_string_position == (wo.right_string_position+1)


  def chooseObjectWith(slipNodeID: String,i_relevance: Double, t_relevance: Double, t: Double) : Option[WorkspaceObject] = {
    val i_unhappiness = initial.intra_string_unhappiness
    val t_unhappiness = target.intra_string_unhappiness

    log.debug("about to choose string:");
    log.debug("initial string: relevance="+i_relevance+", unhappiness="+i_unhappiness)
    log.debug("target string: relevance="+t_relevance+", unhappiness="+t_unhappiness)

    val str = workspaceStringBasedOnRelevanceAndUnhappiness(i_relevance, i_unhappiness, t_relevance, t_unhappiness)

    if (str == initial) print("initial string selected");
    else print("target string selected");

    chooseObject(str.objects.toList, TemperatureAjustmentVariable.Intra_string_salience, t)
  }

  def workspaceStringBasedOnRelevanceAndUnhappiness(
                                         i_relevance: Double,
                                         i_unhappiness: Double,
                                         t_relevance: Double,
                                         t_unhappiness:Double
                                       ): WorkspaceString = if (
    (r.nextDouble() * (i_relevance + i_unhappiness + t_relevance + t_unhappiness)) >
      (i_relevance + i_unhappiness)) target else initial

  def choose_bond_facet(fromob: WorkspaceObject, toob: WorkspaceObject) = {
    val fromdtypes = fromob.descriptions.toList.map(d => d.descriptionType)
    val todtypes = toob.descriptions.toList.map(d => d.descriptionType)
    (fromdtypes, todtypes)
  }



  def initialOrTargetText(ws: WorkspaceStructure) = if (ws.workspaceString().equals(initial)) "initial" else "target"

  def logTrying(wo: WorkspaceStructure, typeSource: WorkspaceStructure) = {
    val stringType = initialOrTargetText(typeSource)
    log.debug(s"trying to build ${wo.toString()} in ${stringType} string")
  }

  def total_description_type_support(description: SlipNodeRep, workspaceString: WorkspaceString): Double = {
    (description.activation+ local_description_type_support(description,workspaceString)) / 2.0
  }
  def local_description_type_support(description_type: SlipNodeRep, workspaceString: WorkspaceString): Double = {
    // returns the proportion of objects in the string that have
    // a description with this description_type
    val objectsWithSameString = objectRefs.values.filter(ob => ob.wString.isDefined && ob.wString.get == workspaceString)
    val number_of_objects = objectsWithSameString.size.toDouble

    val sameDescriptionTypeDescriptions: List[List[Description]] = objectsWithSameString.toList.map (ob => {
      ob.descriptions.toList.filter(d => d.descriptionType.id == description_type.id)
    })
    val total_number_of_objects: Double = sameDescriptionTypeDescriptions.map(_.size).sum
    number_of_objects/total_number_of_objects
  }

  def reset(initial_string: String, modified_string: String, target_string: String) = {
    initial = new WorkspaceString(initial_string,50,200,350,300)
    updateWorkspaceObjectRefs(initial)
    modified = new WorkspaceString(modified_string,650,200,950,300)
    updateWorkspaceObjectRefs(modified)
    target = new WorkspaceString(target_string,50,610,450,710)
    updateWorkspaceObjectRefs(target)
  }

  def updateWorkspaceObjectRefs(ws: WorkspaceString) = {
    for (l <- ws.objects) {
      //objectRefs += (l.uuid -> l)
      addStructure(l)
    }
  }

  // Add new description similar to those in argument
  def addDescriptionsToWorkspaceObject(wo: WorkspaceObject, descriptions: List[Description]) = {
    descriptions.filter(d => !wo.has_description(d)).map(d => {
      wo.add_description(d.descriptionType, d.descriptor)
    })
    build_descriptions(wo)
  }
  /*def add_descriptions(ds: List[Description]){
    ds.filter(d => !has_description(d)).map(d => add_description(d.descriptionTypeSlipNodeID, d.descriptorSlipNodeID, workspace))
    workspace.build_descriptions()
  }*/
/*
  def propose_description(ob: WorkspaceObject,
    slipnode description_type, slipnode descriptor, codelet orig){
    description d = addDescription(ob,description_type,descriptor);

    // TO BE DONE in slipnet
    descriptor.buffer=100.0;
    double urgency = description_type.activation;


    codelet ncd = new codelet("description-strength-tester",get_urgency_bin(urgency));
    ncd.arguments.addElement(d);
    ncd.Pressure_Type = orig.Pressure_Type;
    if (coderack.remove_terraced_scan) ncd.run();
    else coderack.Post(ncd);

  }*/
  // called by codelet "group-builder"
  def build_descriptions(wo: WorkspaceObject) = {
    log.debug("build_descriptions " + wo)
    for (description <- wo.descriptions) {
      build_description(description)
    }
    // GUI check_visibility()
  }
  def build_description(description: Description) = {
    if (description.descriptor.isEmpty) {
      log.info("oups d.descriptor is null");
    } else {
      val descriptor = description.descriptor.get
      slipnet ! SetSlipNodeBufferValue(description.descriptionType.id, 100.0)
      slipnet ! SetSlipNodeBufferValue(descriptor.id, 100.0)

      if (!description.wObject.has_slipnode_description(descriptor)) {
        if (!structures.contains(description)) {
          // GUI area.AddObject(d);
          //log.debug("add description " + description)

          addStructure(description)
        }
      }
    }
  }

  def build_group(group: Group) = {
    addStructure(group)
    group.build_group()

// GUI   workspace.WorkspaceArea.AddObject(this,3);
// GUI   workspace.WorkspaceSmall.AddObject(this);
    build_descriptions(group)
// GUI    workspace.check_visibility();
    group.activate_descriptions();
  }

  def build_bond(bond: Bond) = {
    addStructure(bond)
    bond.build_bond()
  }



  // GUI
  def check_visibility() = {
    // checks the visibility of all descriptions attached to objects in the
    // workspace.  if the object is part of a group - the description is invisible
    for (wo <- objectRefs.values) {
      val vis = wo.group.isEmpty

      for (d <- wo.descriptions) {
        if (d.visible!=vis) {
          d.visible = vis;
          // GUIWorkspaceArea.Redraw = true;
        }
      }
    }
  }


  def break_rule() = {
    rule = None
    // GUI ?
    // Workspace_Rule.Change_Caption("no rule");
  }

  def incompatible_rule_corr(r: Rule, c: Correspondence): Boolean = {
    if ((r==null)||(c==null)) return false;
    // find changed object
    val changedOpt = initial.objects.find(wo => wo.changed)

    changedOpt match {
      case Some(changed) =>
        if (c.obj1!=changed) return false;
        // it is incompatible if the rule descriptor is not in the mapping list

        c.concept_mapping_list.find(cm => {
          r.descriptor.isDefined && (cm.descriptor1SlipNodeID == r.descriptor.get)
        }).isEmpty

      case None => true
    }
  }




  /*def addDescription(
                      wObject: WorkspaceObject,
                      descriptionTypeSlipNodeID : String,
                      descriptorSlipNodeID : String): Unit = {
    val description = new Description(wObject,descriptionTypeSlipNodeID,descriptorSlipNodeID)
    description
  }*/
  def lettersOf(ws: WorkspaceString): List[Letter] = {
    ws.objects.toList.filter(s => s.isInstanceOf[Letter]).asInstanceOf[List[Letter]]
  }


  def workspaceObjects(): List[WorkspaceObject] = structures.toList.filter(s => s.isInstanceOf[WorkspaceObject]).asInstanceOf[List[WorkspaceObject]]

  def chooseObject(wos: List[WorkspaceObject], variable: String, temperature: Double) : Option[WorkspaceObject] = {
    //log.debug("workspaceObjects() " + workspaceObjects())
    val nonModifieds = wos.filter(wo =>
      {
        //println("chooseObject " + wo.workspaceString + " modified " + modified)
        wo.workspaceString != modified
      })
    //log.debug("nonModifieds " + nonModifieds)

    chooseObjectFromList(nonModifieds, variable)
  }
  def chooseNeighbor(from: WorkspaceObject, conditional: (WorkspaceObject,WorkspaceObject) => Boolean ) : Option[WorkspaceObject] = {
    val nonModifieds = workspaceObjects().filter(wo => (
      wo.workspaceString() == from.workspaceString()) && conditional(from, wo)
    )
    chooseObjectFromList(nonModifieds, TemperatureAjustmentVariable.Intra_string_salience)
  }



  object TemperatureAjustmentVariable {
    val Intra_string_salience = "intra_string_salience"
    val Inter_string_salience = "inter_string_salience"
    val Total_salience = "total_salience"
    val Relative_importance = "relative_importance"
  }

  def chooseObjectFromList(list: List[WorkspaceObject], variable: String): Option[WorkspaceObject] = {
    // chooses an object from the the list by a variable
    // eg "intra-string-salience" probabilistically adjusted for temperature
    if (list.isEmpty) {
      Option.empty[WorkspaceObject]
    } else {
      val adjustment: WorkspaceObject => Double = variable match {
        case TemperatureAjustmentVariable.Intra_string_salience => wo: WorkspaceObject => wo.intra_string_salience
        case TemperatureAjustmentVariable.Inter_string_salience => wo: WorkspaceObject => wo.inter_string_salience
        case TemperatureAjustmentVariable.Total_salience => wo: WorkspaceObject => wo.total_salience
        case TemperatureAjustmentVariable.Relative_importance => wo: WorkspaceObject => wo.relative_importance
      }
      val oProbs = list.map(wo => adjustment(wo))
      val index = Utilities.valueProportionalRandomIndexInValueList(oProbs)
      Option(list(index))
    }
  }


  def structure_vs_structure(s1: WorkspaceStructure,
                             w1: Double,
                             s2: WorkspaceStructure,
                             w2: Double): Boolean = {
    s1.update_strength_value();
    s2.update_strength_value();
    val vs1 = s1.total_strength*w1;
    val vs2 = s2.total_strength*w2;
    val v1 = Formulas.temperatureAdjustedValue(vs1, chaleur)
    val v2 = Formulas.temperatureAdjustedValue(vs2, chaleur)
    !(((v1+v2) * r.nextDouble())>v1)
  }

  def fight_it_out(wo: WorkspaceStructure, v1: Double, structs: List[WorkspaceStructure], v2: Double): Boolean = {
    if (structs.isEmpty) {
      true
    } else {
      !structs.find(ws => (!structure_vs_structure(wo,v1,ws,v2))).isDefined
    }
  }





  // From workspace_formulas.java.26
  def update_temperature() = {
    calculate_intra_string_unhappiness()
    calculate_inter_string_unhappiness()
    calculate_total_unhappiness()

    val rule_weakness = if (rule.isDefined){
      rule.get.update_strength_value();
      100.0-rule.get.total_strength
    } else 100.0

    actual_temperature = if (clamp_temperature) 100.0 else Formulas.weighted_average(total_unhappiness,0.8, rule_weakness,0.2)
    // Not useful Temperature.Update(actual_temperature);

    chaleur = if (!clamp_temperature) actual_temperature else chaleur;
    // GUI Temperature.Update(chaleur);
    total_happiness_values += 100.0 - total_unhappiness
    temperature_values += actual_temperature
  }

  def calculate_string_unhappiness(extractor:  WorkspaceObject => Double): Double = {
    val isu = (objectRefs.values.map(wo => wo.relative_importance * extractor(wo)).sum) / 2
    if (isu>100.0) 100.0 else isu
  }
  def calculate_intra_string_unhappiness() = {
    intra_string_unhappiness = calculate_string_unhappiness( (wo:WorkspaceObject) => wo.intra_string_unhappiness)
    log.debug("workspace intra string unhappiness = " + intra_string_unhappiness);
  }

  def calculate_inter_string_unhappiness() = {
    inter_string_unhappiness = calculate_string_unhappiness( (wo:WorkspaceObject) => wo.inter_string_unhappiness)
    log.debug("workspace inter string unhappiness = " + inter_string_unhappiness);
  }

  def calculate_total_unhappiness() = {
    total_unhappiness = calculate_string_unhappiness( (wo:WorkspaceObject) => wo.total_unhappiness)
    log.debug("workspace total string unhappiness = " + total_unhappiness);
  }


  def slippage_list(): SlippageListShell = {
    //Vector sl = new Vector();
    val sl = if (changed_object.isDefined && changed_object.get.correspondence.isDefined) {
      val c = changed_object.get.correspondence.get;

      c.concept_mapping_list
    } else List.empty[ConceptMappingRep]

    val sl2 = initial.objects.toList.map(wo => {
      if (wo.correspondence.isDefined){
        Some(wo.correspondence.get.slippageCandidates())
      } else None
    }).flatten.flatten

    SlippageListShell(sl,sl2)
  }


  // Also coded in slipnet world, in ConceptMapping. If possible use slipnet world
  def in_vector(cm: ConceptMappingRep, v: List[ConceptMappingRep]): Boolean = {
    // returns true in the concept mapping is in the vector
    v.find(c => {
      ((c.description_type1SlipNodeID == cm.description_type1SlipNodeID) &&
        (c.description_type2SlipNodeID == cm.description_type2SlipNodeID) &&
        (c.descriptor1SlipNodeID == cm.descriptor1SlipNodeID))
    }).isDefined
  }



  def number_of_bonds(): Int = {
    //returns the number of bonds in the workspace
    structures.filter(ws => ws.isInstanceOf[Bond]).size
  }

  def correspondence_present(c: Correspondence): Boolean = {
    val obj1Correspondence = c.obj1.correspondence
    obj1Correspondence.isDefined && obj1Correspondence.get.obj2 ==c.obj2
  }

}

