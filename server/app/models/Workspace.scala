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
import models.ConceptMapping.{ConceptMappingParameters, ConceptMappingRep}
import models.Correspondence.CorrespondenceRep
import models.Group.{FutureGroupRep, GroupRep}
import models.SlipNode.{GroupSlipnetInfo, SlipNodeRep}
import models.Slipnet.{CorrespondenceUpdateStrengthData, DescriptionTypeInstanceLinksToNodeInfo}
import models.Workspace.{DataForStrengthUpdateResponse, GoWithBondStrengthTester2, GoWithBottomUpCorrespondenceScout2Response, GoWithBottomUpCorrespondenceScout3, GoWithBottomUpCorrespondenceScout3Response, GoWithCorrespondenceBuilder, GoWithCorrespondenceBuilder2, GoWithCorrespondenceBuilder3, GoWithCorrespondenceBuilder4, GoWithCorrespondenceBuilder5, GoWithCorrespondenceBuilder6, GoWithCorrespondenceBuilder7, GoWithCorrespondenceBuilder8, GoWithCorrespondenceStrengthTester, GoWithCorrespondenceStrengthTester2, GoWithCorrespondenceStrengthTester3, GoWithDescriptionBuilder, GoWithGroupScoutWholeString, GoWithGroupStrengthTester, GoWithImportantObjectCorrespondenceScout, GoWithImportantObjectCorrespondenceScout2, GoWithImportantObjectCorrespondenceScout3, GoWithRuleBuilder, GoWithRuleScout, GoWithRuleScout2, GoWithRuleScout3, GoWithRuleStrengthTester, GoWithRuleTranslator, GoWithRuleTranslator2, GoWithTopDownBondScout2, GoWithTopDownBondScoutWithResponse, GoWithTopDownDescriptionScout2, GoWithTopDownGroupScoutCategory, GoWithTopDownGroupScoutDirection2, InitializeWorkspaceStringsResponse, PostTopBottomCodeletsGetInfoResponse, SlipnetLookAHeadForNewBondCreationResponse, SlippageListShell, StepX, UpdateEverything, UpdateEverythingFollowUp, WorkspaceProposeBondResponse, WorkspaceProposeRule, WorkspaceProposeRuleResponse}
import models.WorkspaceObject.WorkspaceObjectRep
import models.WorkspaceStructure.WorkspaceStructureRep
import models.codelet.BondStrengthTester.GoWithBondStrengthTesterResponse2
import models.codelet.BottomUpBondScout.{GoWithBottomUpBondScout2Response, GoWithBottomUpBondScoutResponse}
import models.codelet.BottomUpDescriptionScout.GoWithBottomUpDescriptionScoutResponse
import models.codelet.Codelet.{Finished, PrepareDescriptionResponse}
import models.codelet.CorrespondenceBuilder.{GoWithCorrespondenceBuilder2Response, GoWithCorrespondenceBuilder3Response, GoWithCorrespondenceBuilder4Response1, GoWithCorrespondenceBuilder4Response2, GoWithCorrespondenceBuilder6Response, GoWithCorrespondenceBuilder7Response, GoWithCorrespondenceBuilder8Response, GoWithCorrespondenceBuilder9Response}
import models.codelet.CorrespondenceStrengthTester.{GoWithCorrespondenceStrengthTesterResponse, GoWithCorrespondenceStrengthTesterResponse2, GoWithCorrespondenceStrengthTesterResponse3}
import models.codelet.DescriptionStrengthTester.GoWithDescriptionStrengthTesterResponse
import models.codelet.GroupBuilder.{GoWithGroupBuilderResponse, GoWithGroupBuilderResponse2, GroupBuilderNoGroupFighting, PrepareGroupFighting}
import models.codelet.GroupScoutWholeString.{GoWithGroupScoutWholeString3Response, GoWithGroupScoutWholeStringResponse, GroupScoutWholeString2Response, GroupScoutWholeString3Response}
import models.codelet.GroupStrengthTester.{GoWithGroupStrengthTesterResponse, GoWithGroupStrengthTesterResponse2}
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
  def props(temperature: ActorRef): Props = Props(new Workspace(temperature))
  def activationWithSlipNodeRep(activationBySlipNodeID: Map[String, Double], sr: SlipNodeRep): Double = {
    if (activationBySlipNodeID.contains(sr.id)) {
      activationBySlipNodeID(sr.id)
    } else 0.0
  }

  case class Run(executionRun: ActorRef, initialString: String, modifiedString: String, targetString: String)
  case class Initialize(coderack: ActorRef, initialS: String, modifiedS: String, targetS: String)
  case class InitializeWorkspaceStringsResponse(
                                                 initialDescriptions: List[WorkspaceObjectRep],
                                                 modifiedDescriptions: List[WorkspaceObjectRep],
                                                 targetDescriptions: List[WorkspaceObjectRep]
                                               )
  case class Step(temperature: Double)
  case class StepX(temperature: Double)

  case object Found
  case class GoWithBreaker(temperature: Double)
  case class BondWithNeighbor(temperatGoWithBottomUpBondScout2Debugure: Double)
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
                                    group_category: SlipNodeRep,
                                    direction_category: Option[SlipNodeRep],
                                    bond_facet: SlipNodeRep,
                                    bond_category: SlipNodeRep,
                                    groupSlipnetInfo: GroupSlipnetInfo,
                                    t: Double
                                  )
  case class WorkspaceProposeGroupResponse(groupID: String)

  case class WorkspaceProposeGroup2(
  object_rep_list: List[WorkspaceObjectRep],
  bls: List[BondRep],
  group_category: SlipNodeRep,
  direction_category: Option[SlipNodeRep],
    bond_facet: SlipNodeRep,
  bond_category: SlipNodeRep
                                   )


  case object GoWithReplacementFinder
  case class GoWithTopDownGroupScoutCategory(slipNodeID: String, bondFocus: String, t: Double, groupSlipnetInfo: GroupSlipnetInfo)
  case class GoWithTopDownGroupScoutCategory2(
                                              group_category: SlipNodeRep,
                                              mydirection: SlipNodeRep,
                                              fromob: WorkspaceObjectRep,
                                              bond_category: SlipNodeRep,
                                              temperature: Double,
                                              groupSlipnetInfo: GroupSlipnetInfo
                                             )


  case class GoWithBottomUpDescriptionScout(temperature: Double)
  case class GoWithTopDownDescriptionScout(descriptionTypeID: String, temperature: Double)
  case class GoWithTopDownDescriptionScout2(chosen_object: WorkspaceObjectRep, i: DescriptionTypeInstanceLinksToNodeInfo)

  case class PrepareDescription(chosen_object: WorkspaceObjectRep,
                                chosen_propertyRep: SlipNodeRep,
                                description_typeRep: SlipNodeRep)
  case class GoWithBottomUpCorrespondenceScout(temperature: Double)
  case class GoWithBottomUpCorrespondenceScout3(fg: FutureGroupRep, obj2: WorkspaceObjectRep, t: Double)
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
  case class GoWithTopDownBondScout2(fromob: WorkspaceObjectRep, toob: WorkspaceObjectRep, bond_facets: List[SlipNodeRep])
  case class GoWithTopDownBondScoutDirection(slipNodeID: String, temperature: Double)

  case class GoWithTopDownBondScoutWithResponse(from: WorkspaceObjectRep, to: WorkspaceObjectRep, fromdtypes: List[SlipNodeRep], todtypes: List[SlipNodeRep])

  case class GoWithDescriptionBuilder(descriptionID: String, temperature: Double)

  case class GoWithGroupBuilder(temperature: Double, groupID: String)
  case class GoWithGroupBuilder2(groupID: String, degree_of_association: Double, incompatibleBondList: List[BondRep])
  case class GoWithGroupBuilder3(groupID: String, bondReps: List[BondRep], degOfAssos: Map[String, Double])
  case class GoWithGroupBuilder4(groupID: String, incg: List[String],degree_of_association1: Double, degree_of_association2: Map[String, Double], incompatibleBondList: List[BondRep])
  case class GoWithGroupBuilder5(groupID: String, incompatibleBondList: List[BondRep], incg: List[String])

  case class GoWithDescriptionStrengthTester(temperature: Double, descriptionID: String)
  case class GoWithBondStrengthTester(temperature: Double, bondID: String)
  case class GoWithBondStrengthTester2(temperature: Double, bondID: String, bond_category_degree_of_association: Double)


  case class GoWithBondBuilder(temperature: Double, bondID: String, bond_category_degree_of_association: Double)

  case class GoWithTopDownGroupScoutDirection(slipNodeRep: SlipNodeRep, mydirection: SlipNodeRep, fromobrep: WorkspaceObjectRep, t:Double, groupSlipnetInfo: GroupSlipnetInfo)
  case class GoWithTopDownGroupScoutDirection2(group_category: Option[SlipNodeRep], fromob: WorkspaceObjectRep, firstBondUUID: String, bond_category: SlipNodeRep)
  case class CommonSubProcessing(group_category: SlipNodeRep, fromobUUID: String, firstBondUUID: String, bond_category: SlipNodeRep)

  case class GoWithGroupScoutWholeString(t: Double)
  case class GoWithGroupScoutWholeString2(left_most: WorkspaceObjectRep,slipnetLeft: SlipNodeRep,
                                          slipnetRight: SlipNodeRep)
  case class GoWithGroupScoutWholeString3(leftMostUUID: String)
  case class GoWithGroupStrengthTester(temperature: Double, groupID: String)
  case class GoWithGroupStrengthTester2(temperature: Double, groupID: String, degree_of_association: Double)
  case class LookAHeadForNewBondCreation(s: ActorRef, groupID: String, index: Int, incg: List[String], newBondList: List[BondRep])
  case class SlipnetLookAHeadForNewBondCreationResponse(
                                                         s: ActorRef,
                                                         g: String,
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
                                   facet: Option[SlipNodeRep],
                                  description: Option[SlipNodeRep],
                                  objectCategory: Option[SlipNodeRep],
                                  relation: Option[SlipNodeRep],
                                  lengthSlipNode: SlipNodeRep,
                                  predecessorSlipNode: SlipNodeRep,
                                  successorSlipNode: SlipNodeRep
                                 )
  case class WorkspaceProposeRuleResponse(ruleID: String)
  case class GoWithRuleScout2(changed: WorkspaceObjectRep, string_position_category: SlipNodeRep, letter_category: SlipNodeRep)
  case class GoWithRuleScout3(slippage_list_rep: List[ConceptMappingRep], object_list: List[SlipNodeRep],obj2: WorkspaceObjectRep, letterCategory: SlipNodeRep)

  case class SlippageListShell(
                                sl: List[ConceptMappingRep],
                                slippageCandidates: List[ConceptMappingRep]
                              )
  case class GoWithRuleStrengthTester(temperature: Double, ruleID: String)
  case class GoWithRuleBuilder(ruleID: String)
  case class GoWithRuleTranslator(t: Double)
  case class GoWithRuleTranslator2(slippage_list_rep: List[ConceptMappingRep])
  case class SlipnodeActivationChanged(id: String, activation: Double)
  case class GoWithImportantObjectCorrespondenceScout(t: Double)
  case object GoWithImportantObjectCorrespondenceScout2
  case class GoWithImportantObjectCorrespondenceScout3(slippage_list_rep: List[ConceptMappingRep], s: SlipNodeRep, t:Double, obj1: WorkspaceObjectRep)
  case class GoWithCorrespondenceBuilder(temperature: Double, correponsdenceID: String)
  case class GoWithCorrespondenceBuilderResponse(obj2: WorkspaceObjectRep)

  case class GoWithCorrespondenceBuilder2(correponsdenceID: String, futureGroupRep: FutureGroupRep, t:Double)
  case class GoWithCorrespondenceBuilder3(correponsdenceID: String,
                                          updatedCorrespondenceCMReps: List[ConceptMappingRep])
  case class GoWithCorrespondenceBuilder4(correponsdenceID: String, correspondenceReps: List[CorrespondenceRep],
                                          internal_strength: Double,
                                          supporting_correspondences:Map[String, Boolean]
                                         )
  case class GoWithCorrespondenceBuilder5(correponsdenceID: String, b: Option[BondRep],
                                          internal_strength: Double,
                                          supporting_correspondences:Map[String, Boolean],
                                          bond_category_degree_of_associationOpt: Option[Double]
                                         )
  case class GoWithCorrespondenceBuilder6(
                                           correponsdenceID: String,
                                           incc: List[CorrespondenceRep],
                                           incompatible_bond: Option[BondRep],
                                           incompatible_group: Option[GroupRep],
                                           internal_strength: Double,
                                           supporting_correspondences:Map[String, Boolean]
                                         )
  case class GoWithCorrespondenceBuilder7(
                                           correponsdenceID: String,
                                           accessory_concept_mapping_list: List[ConceptMappingRep]
                                         )
  case class GoWithCorrespondenceBuilder8(
                                           correponsdenceID: String,
                                           accessory_concept_mapping_list: List[ConceptMappingRep]
                                         )
  case class GoWithCorrespondenceBuilder9(
                                           correponsdenceID: String,
                                           temperature: Double
                                         )

  case class GoWithBottomUpCorrespondenceScout3Response(newObj2: WorkspaceObjectRep)
  case class GoWithBottomUpCorrespondenceScout2Response(
                                                         correspondenceID: String,
                                                         distiguishingConceptMappingSize: Int,
                                                         distiguishingConceptMappingTotalStrength: Double
                                                       )
  case class GoWithCorrespondenceStrengthTester(correponsdenceID: String, futureGroupRep: FutureGroupRep, t:Double)
  case class GoWithCorrespondenceStrengthTester2(correponsdenceID: String, t:Double)
  case class GoWithCorrespondenceStrengthTester3(correponsdenceID: String, internalStrength: Double,supporting_correspondences:Map[String, Boolean], t:Double)

  case class DataForStrengthUpdateResponse(bondData: Map[String,Double], correpondenceData: Map[String, CorrespondenceUpdateStrengthData], t: Double)
  case class UpdateEverything(codelets_run: Int, t: Double)
  case object UpdateEverythingFollowUp
  case class PostTopBottomCodeletsGetInfoResponse(codeletToPost: List[(String,Either[Double, Int], Option[String], Option[Double])])
  case class GetNumCodeletsResponse(codeletsSize: Int, t:Double)
  case class InitializeWorkspace(slipnet: ActorRef)

  case class GoWithBondStrengthTesterResponse(bond: BondRep)
  case class AfterFighting(groupID: String, bondReps: List[BondRep])
}

class Workspace(temperature: ActorRef) extends Actor with ActorLogging with InjectedActorSupport {
  import Workspace.{
    WorkspaceProposeGroup2,
    WorkspaceProposeGroupResponse,
    Initialize,
    Found,
    Step,
    GoWithBottomUpCorrespondenceScout,
    GoWithBottomUpCorrespondenceScout2,
    GoWithBottomUpDescriptionScout,
    GoWithTopDownDescriptionScout,
    GoWithTopDownBondScoutCategory,
    GoWithTopDownBondScoutDirection,
    GoWithTopDownGroupScoutCategory,
    GoWithTopDownGroupScoutCategory2,
    WorkspaceProposeGroup,
    PrepareDescription,
    GoWithReplacementFinder,
    GoWithBondBuilder,
    GoWithGroupBuilder,
    GoWithGroupBuilder2,
    GoWithGroupBuilder3,
    GoWithGroupBuilder4,
    GoWithGroupBuilder5,
    GoWithDescriptionStrengthTester,
    GoWithBondStrengthTester,
    BondWithNeighbor,
    GoWithBottomUpBondScout2,
    WorkspaceProposeBond,
    GoWithBreaker,
    GoWithTopDownGroupScoutDirection,
    CommonSubProcessing,
    GoWithGroupScoutWholeString2,
    LookAHeadForNewBondCreation,
    GoWithCorrespondenceBuilderResponse,
    GoWithCorrespondenceBuilder9,
    SlipnodeActivationChanged,
    InitializeWorkspace,
    GoWithBondStrengthTesterResponse,
    AfterFighting,
    GetNumCodeletsResponse,
    GoWithGroupScoutWholeString3,
    GoWithGroupStrengthTester2
  }

  import Slipnet._
  import Coderack._
  import models.codelet.BottomUpCorrespondenceScout.{
    GoWithBottomUpCorrespondenceScoutWorkspaceReponse
  }

  var executionRunActor: ActorRef = null
  var coderack: ActorRef = null
  var slipnet: ActorRef = null

  var found_answer = false;
  // var structures = ListBuffer.empty[WorkspaceStructure]
  // initial ---> target
  // modified ----> ?
  var initial: WorkspaceString = null
  var target: WorkspaceString = null
  var modified: WorkspaceString = null
  var changed_object = Option.empty[WorkspaceObject]
  var structureRefs = Map.empty[String, WorkspaceStructure]
  // Because a Map doesn't garantee the order, we need also an array
  var structures = ListBuffer.empty[WorkspaceStructure]
  var objectRefs = Map.empty[String, WorkspaceObject]
  var objects = ListBuffer.empty[WorkspaceObject]
  var wsRefs = Map.empty[String, WorkspaceStructure]

  var rule = Option.empty[Rule]

  var total_unhappiness = 0.0;
  var intra_string_unhappiness = 0.0;
  var inter_string_unhappiness = 0.0;

  var chaleur = 100.0;
  var actual_temperature = 100.0;
  var total_happiness_values = ListBuffer.empty[Double]
  var temperature_values = ListBuffer.empty[Double]
  var clamp_temperature = false;  // external clamp
  var codelets_run = 0

  var activationBySlipNodeID = Map.empty[String, Double]

  /*def objectRefs: Map[String, WorkspaceObject] = {
    val subset = structureRefs.filter { case (k,v) => v.isInstanceOf[WorkspaceObject] }
    subset.asInstanceOf[Map[String, WorkspaceObject]]
  }*/



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


  def addObject(wo: WorkspaceObject): Unit = {
    log.debug("workspace_objects add " + wo + " uuid " + wo.uuid);
    objectRefs += (wo.uuid -> wo)
    objects += wo
  }
  def removeObject(wo: WorkspaceObject): Unit = {
    log.debug("workspace_objects remove " + wo + " uuid " + wo.uuid);
    objectRefs -= wo.uuid
    objects -= wo
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
    log.debug("addBond " + b)
    addStructure(b)
    b.build_bond()
  }
  def break_bond(b: Bond) = {
    // GUI WorkspaceArea.DeleteObject(this);
    // GUI WorkspaceSmall.DeleteObject(this);
    System.out.println("bond.break_bond " + this);
    removeStructure(b)
    b.break_bond()
  }
  def break_group(gr: Group): Unit = {
    log.debug("breaking group "+this);
    for(d <- gr.descriptions) {
      break_description(d)
    }
    if (gr.group.isDefined) {
      break_group(gr.group.get)
    }
//   GUI  workspace.WorkspaceArea.DeleteObject(this);
//   GUI  workspace.WorkspaceSmall.DeleteObject(this);
    removeStructure(gr)
    removeObject(gr)

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
    System.out.println("break_correspondence. remove correspondence");

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


    case SlipnodeActivationChanged(slipNodeID, activation) =>
      activationBySlipNodeID += (slipNodeID -> activation)


    case Initialize(cr, initialS, modifiedS, targetS) =>
      log.debug("Workspace: Initialize")
      coderack = cr
      reset(initialS, modifiedS, targetS)
      slipnet ! InitializeWorkspaceStrings(
        initial.letterSlipnetComplements(),
        modified.letterSlipnetComplements(),
        target.letterSlipnetComplements()
      )

    case InitializeWorkspaceStringsResponse(initialDescriptions, modifiedDescriptions, targetDescriptions) =>
      log.debug("Workspace: InitializeWorkspaceStringsResponse")
      updateWorkspaceStringWithDescriptionReps(initial, initialDescriptions)
      updateWorkspaceStringWithDescriptionReps(modified, modifiedDescriptions)
      updateWorkspaceStringWithDescriptionReps(target, targetDescriptions)
      coderack ! FinishInitializingWorkspaceStrings(objects.toList.size)

    case InitializeWorkspace(sn) =>
      slipnet = sn

    case models.Workspace.UpdateEverything(cr, t) =>
      codelets_run = cr

      log.debug("update_Everything")
      val brs = bondReps()
      val crs = correspondenceReps()
      slipnet ! DataForStrengthUpdate(brs,crs,t)

    case DataForStrengthUpdateResponse(bondData, correspondenceData, t) =>
      log.debug("update_Everything " + structures.size)
      log.debug("1T: " + t)

      for (ws <- structures) {
        updateStructureStrengthValue(bondData, correspondenceData, ws)
      }
      log.debug("2T: " + t)

      log.debug("Workspace. update_Everything. activationBySlipNodeID " + (activationBySlipNodeID == null))
      // update the the object values of all objects in the workspace
      log.debug("update the the object values of all objects in the workspace")
      for (wo <- objects.toList) {
        wo.update_object_value(activationBySlipNodeID)
      }
      log.debug("3T: " + t)

      // update the relative importances of initial and target strings
      log.debug("update the relative importances of initial and target strings")
      initial.update_relative_importance();
      target.update_relative_importance();

      log.debug("4T: " + t)

      // update the intra string unhappiness of initial and target strings
      log.debug("update the intra string unhappiness of initial and target strings")

      initial.update_intra_string_unhappiness();
      target.update_intra_string_unhappiness();
      log.debug("5T: " + t)

      if (codelets_run>0) {
        coderack ! GetNumCodelets(t)
      } else {
        self ! UpdateEverythingFollowUp
      }

    case GetNumCodeletsResponse(codeletsSize: Int, t:Double) =>
      val ruleTotalWeaknessOpt = rule.map(_.total_weakness())
      slipnet ! PostTopBottomCodeletsGetInfo(
        t,
        intra_string_unhappiness,
        inter_string_unhappiness,
        ruleTotalWeaknessOpt,
        number_of_bonds,
        unrelated_objects().size,
        ungrouped_objects().size,
        unreplaced_objects().size,
        uncorresponding_objects().size,
        codeletsSize
      )

    case PostTopBottomCodeletsGetInfoResponse(codeletToPost) =>
      if (codelets_run>0){
        System.out.println("post_top_down_codelets");
        coderack ! PostCodelets(codeletToPost)
      } else {
        self ! UpdateEverythingFollowUp
      }



    case UpdateEverythingFollowUp =>
      val newT = update_temperature()
      println("Finish update_Everything T: " + newT);

      coderack ! UpdateEverythingResponse(newT)

    case Found =>
      found_answer = true


    case Step(temperature) =>
      log.debug("Step...")

      if (found_answer) {
        log.debug("Workspace Found answer")

        //executionRunActor ! ExecutionRun.Found
      } else {
        val dd = objects.toList
        log.debug(s"Step2 <$dd>")

        log.debug("Workspace Step")
        coderack ! ChooseAndRun(dd.size,temperature)
      }

    // GoWithBreaker, see Codelet.java.68
    case GoWithBreaker(t) =>
      val candidateStructures = structures.filter(s =>
        s.isInstanceOf[Group] ||
        s.isInstanceOf[Bond] ||
        s.isInstanceOf[Correspondence]
      )
      log.debug("Breaker candidateStructures " +candidateStructures)
      if (candidateStructures.isEmpty) {
        log.debug("There are no structures built: fizzle")
        sender() ! Finished
      } else {
        val wsize = candidateStructures.size
        // Codelet.java.84
        val p = (Random.rnd() * wsize).toInt
        val pos = if (p >= wsize) 0 else p
        val ws = candidateStructures(pos);
        log.debug("Breaker pos " + pos + " ws " + ws)



        val probability = 1.0 - WorkspaceFormulas.temperature_adjusted_probability(ws.total_strength / 100.0, t)

        // printing
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

        // try to break all objects
        val cant = break_objects.find(w => {
          val p = WorkspaceFormulas.temperature_adjusted_probability(w.total_strength/100.0, t);
          WorkspaceFormulas.flip_coin(p)
        }).isDefined
        if (cant) {
          log.debug("couldn't break structure: Fizzle!");
          sender() ! Finished
        } else {
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
      }

    // bottom-up-bond-scout in codelet.java.240
    case BondWithNeighbor(temperature) =>
      //          workspace_object fromob = workspace_formulas.choose_object("intra_string_salience",workspace.workspace_objects);
      val fromOpt = chooseObject(objects.toList, TemperatureAjustmentVariable.Intra_string_salience, temperature)
      fromOpt match {
        case None =>
          log.debug("BondWithNeighbor | failed with empty from")
          sender() ! Finished
        case Some(from) =>
          //log.debug(s"BondWithNeighbor | from ${from}")
          val toOpt = chooseNeighbor(from, conditionalNeighbor, temperature)
          toOpt match {
            case None =>
              log.debug("BondWithNeighbor | object has no neighbour - fizzle")
              sender() ! Finished

            case Some(to) =>
              log.debug(s"initial object chosen: $from in ${if (from.wString.isDefined && from.wString.get == initial) "initial" else "target"} string")
              log.debug(s"to object: $to")

              sender() ! GoWithBottomUpBondScoutResponse(from.workspaceObjectRep(), to.workspaceObjectRep())
          }
      }


    case GoWithBottomUpBondScout2(fromRep, toRep, fromFacets, toFacets) =>
      println("GoWithBottomUpBondScout2 in ws")
      // workspace_formulas.java.207
      if (fromFacets.isEmpty) {
        log.debug(s" no possible bond-facet - fizzle")
        sender() ! Finished

      } else {
        log.debug(s"fromRep.uuid ${fromRep.uuid} toRep.uuid ${toRep.uuid}")

        val from = objectRefs(fromRep.uuid)
        val to = objectRefs(toRep.uuid)
        log.debug(s"GoWithBottomUpBondScout2 $from $to")
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
      val bondFrom = objectRefs(bondFromRep.uuid)
      val bondTo = objectRefs(bondToRep.uuid)
      log.debug("WorkspaceProposeBond bondFacet " + bondFacet + " bondCategory " + bondCategory)
      val nb = new Bond(bondFrom, bondTo, bondCategory, bondFacet, fromDescriptor, toDescriptor, slipnetLeft, slipnetRight, slipnet)
      // if (!remove_terraced_scan) workspace.WorkspaceArea.AddObject(nb,1);
      wsRefs += (nb.uuid -> nb)

      sender ! WorkspaceProposeBondResponse(nb.uuid)

    // codelet.java.994
    case GoWithReplacementFinder =>
      val initialLetters = lettersOf(initial)
      val size = initialLetters.size
      val posBase = size * Random.rnd()
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
            log.debug("ReplacementFinder m_letter " + m_letter + " position " +position)

            val index = position - 1
            val modifiedChar: Char = modified.s(index)
            val relationOpt = initial.s(index) match {
              case mc if mc == modifiedChar => Some(Slipnet.RelationType.Sameness)
              case mc if mc == (modifiedChar - 1).toChar => Some(Slipnet.RelationType.Successor)
              case mc if mc == (modifiedChar + 1).toChar => Some(Slipnet.RelationType.Predecessor)
              case _ => None
            }
            if (relationOpt.isDefined) {
              log.debug(s"Workspace | ReplaceLetter | ${relationOpt.get} relation found")
            } else {
              log.debug(s"Workspace | ReplaceLetter | no relation found")
            }
            val replacement = new Replacement(i_letter, m_letter, relationOpt)
            i_letter.replacement = Some(replacement)
            wsRefs += (replacement.uuid -> replacement)

            log.debug(s"Workspace | ReplaceLetter | relation " + relationOpt)

            if (relationOpt.isDefined && relationOpt.get != Slipnet.RelationType.Sameness) {
              log.debug(s"Workspace | ReplaceLetter | i_letter " + i_letter + " changed to true")

              i_letter.changed = true;
              changed_object = Some(i_letter)
            }
            log.debug(s"building replacement")
            sender() ! Finished

        }
      }


    // codelet.java.127
    case GoWithBottomUpDescriptionScout(t) =>
      val chosen_objectOpt = chooseObject(objects.toList, TemperatureAjustmentVariable.Total_salience, t)
      chosen_objectOpt match {
        case None =>
          log.debug("GoWithBottomUpDescriptionScout | failed with empty chosen object")
          sender() ! Finished

        case Some(chosen_object) =>
          log.debug(s"chosen object: ${chosen_object} from ${initialOrTargetText(chosen_object)} string")

          val dOpt = WorkspaceFormulas.choose_relevant_description_by_activation(activationBySlipNodeID, chosen_object)
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
      val ob = objectRefs(chosen_object.uuid)

      val descriptor = chosen_propertyRep
      val d = new models.Description(ob, description_typeRep, Some(descriptor))
      slipnet ! SetSlipNodeBufferValue(descriptor.id, 100.0)

      val urgency = Workspace.activationWithSlipNodeRep(activationBySlipNodeID, description_typeRep)
      sender() ! PrepareDescriptionResponse(d.uuid, urgency)


    // codelet.java.165
    case GoWithTopDownDescriptionScout(descriptionTypeID, t) =>
      val chosen_objectOpt: Option[WorkspaceObject] = chooseObjectFromList(objects.toList, TemperatureAjustmentVariable.Total_salience,t)
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
      val chosen_object = objectRefs(chosen_objectRep.uuid)
      val v = chosen_object.get_possible_descriptions(i)
      if (v.isEmpty) {
        log.debug("couldn't find any descriptions");
        sender() ! Finished
      } else {
        val act = v.map(sn => Workspace.activationWithSlipNodeRep(activationBySlipNodeID, sn))
        val chosen_property = v(Utilities.valueProportionalRandomIndexInValueList(act))
        sender() ! GoWithTopDownDescriptionScoutResponse2(chosen_property)
      }

    case GoWithDescriptionBuilder(descriptionID, t) =>
      val d = structureRefs(descriptionID).asInstanceOf[Description]
      log.debug(d.toString());
      if (!objects.toList.contains(d.wObject)) {
        log.debug("object no longer exists: Fizzle!");
        sender() ! Finished
      } else {
        d.descriptor match {
          case Some(descriptor) =>
            if (d.wObject.has_slipnode_description(descriptor)) {
              print("description already exists: Fizzle!");

              slipnet ! SetSlipNodeBufferValue(d.description_type.id, 100.0)
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
      val obj1Opt: Option[WorkspaceObject] = chooseObjectFromList(initial.objects.toList, TemperatureAjustmentVariable.Inter_string_salience,t)
      obj1Opt match {
        case None =>
          log.debug("GoWithBottomUpCorrespondenceScout | failed with empty obj1")
          sender() ! Finished

        case Some(obj1) =>
          val obj2Opt = chooseObjectFromList(target.objects.toList, TemperatureAjustmentVariable.Inter_string_salience,t)
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
      val obj1 = objectRefs(obj1Rep.uuid)
      val obj2 = objectRefs(obj2Rep.uuid)

      val nc = new Correspondence(obj1, obj2, concept_mapping_list, flip_obj2);
      // TODO if (!remove_terraced_scan) WorkspaceArea.AddObject(nc,1);
      wsRefs += (nc.uuid -> nc)

      log.debug(s"BottomUpCorrespondenceScout. create new Correspondence ${nc.uuid} ")
      printCorrespondenceCMs(nc)

      sender ! GoWithBottomUpCorrespondenceScout2Response(
        nc.uuid,
        distiguishingConceptMappingSize,
        distiguishingConceptMappingTotalStrength
      )

      // flipped case
    case GoWithBottomUpCorrespondenceScout3(futureGroup: FutureGroupRep, obj2, t) =>
      val obj2Group = objectRefs(obj2.uuid).asInstanceOf[Group]

      val newObj2 = flippedGroupWithFutureGroup(obj2Group,futureGroup, t)
      structureRefs += (newObj2.uuid -> newObj2)
      sender() ! GoWithBottomUpCorrespondenceScout3Response(newObj2.workspaceObjectRep())

    // codelet.java.880
    case GoWithGroupBuilder(temperature, groupID) =>
      val g = objectRefs(groupID).asInstanceOf[Group]
      logTrying(g, g)

      if (WorkspaceFormulas.group_present(g)) {
        log.debug("already exists...activate descriptors & fizzle");
        g.activate_descriptions();
        val woOpt = WorkspaceFormulas.equivalent_group(g)
        woOpt match {
          case Some(wo) => addDescriptionsToWorkspaceObject(wo, g.descriptions.toList)
          case None =>
            log.debug("GoWithGroupBuilder stop with no equivalent group")
        }
        sender() ! Finished
      } else {
        log.debug("GoWithGroupBuilder. check to see if all objects are still there")
        // check to see if all objects are still there
        val objectNotThere = g.object_list.find(wo => !objects.toList.contains(wo))
        if (objectNotThere.isDefined) {
          log.debug("objects no longer exist! - fizzle");
          sender() ! Finished
        } else {
          log.debug("GoWithGroupBuilder. check to see if bonds are there of the same direction")

          // check to see if bonds are there of the same direction
          val gObjectList = g.object_list
          val incompatibleBondListRaw = for (i <- 1 to gObjectList.size-1) yield {
            val ol = gObjectList(i)
            val bOpt = ol.left_bond
            if (bOpt.isDefined) {
              val b = bOpt.get
              val ob2 = b.left_obj
              if ((ob2 != gObjectList(i - 1)) || (b.direction_category != g.direction_category)) {
                Some(b)
              } else None
            } else None
          }
          log.debug("GoWithGroupBuilder. calculate bondCandidate")

          val bondCandidate = if (gObjectList.size > 1) {
            val bOpt = gObjectList(0).right_bond
            if (bOpt.isDefined) {
              val b = bOpt.get
              val ob2 = b.right_obj
              if ((ob2 != gObjectList(1)) || (b.direction_category != g.direction_category)) Some(b) else None
            } else None
          } else None

          val incompatibleBondList = (bondCandidate :: incompatibleBondListRaw.toList).flatten

          // if incompatible bonds exist - fight
          log.debug("GoWithGroupBuilder. if incompatible bonds exist - fight")

          sender() ! GoWithGroupBuilderResponse(incompatibleBondList.map(b => b.bondRep()), g.group_category.id)
        }
      }

    case GoWithGroupBuilder2(groupID, degree_of_association, incompatibleBondList) =>
      val g = objectRefs(groupID).asInstanceOf[Group]
      g.update_strength_value(degree_of_association)

      val fightNeeded = !incompatibleBondList.isEmpty
      if (fightNeeded) {
        log.debug("GoWithGroupBuilder. fightNeeded")
        sender() ! GoWithGroupBuilderResponse2
      } else {
        log.debug("GoWithGroupBuilder. no fightNeeded")

        self.forward(AfterFighting(groupID, incompatibleBondList))
      }

    case GoWithGroupBuilder3(groupID, bondReps, degOfAssos: Map[String, Double]) =>
      log.debug("GoWithGroupBuilder3")
      val g = objectRefs(groupID).asInstanceOf[Group]
      val incompatibleBondList = bondReps.map(br => wsRefs(br.uuid).asInstanceOf[Bond])

      if (fight_it_out_group_bonds(g,1.0, incompatibleBondList,1.0, degOfAssos)){
        // beat all competing groups
        log.debug("won!")
        self.forward(AfterFighting(groupID, bondReps))
      }
      else {
        log.debug("couldn't break incompatible bonds: fizzle!");
        sender() ! Finished
      }


    case AfterFighting(groupID, bondReps) =>
      val g = objectRefs(groupID).asInstanceOf[Group]
      val incompatibleBondList = bondReps.map(br => wsRefs(br.uuid).asInstanceOf[Bond])

      // fight incompatible groups
      log.debug("fight incompatible groups");

      // fight all groups containing these objects
      val incg = WorkspaceFormulas.get_incompatible_groups(g);
      val incgIds = incg.map(gr => gr.uuid)
      if (!incg.isEmpty) {
        log.debug("fighting incompatible groups: " + incg);
        sender() ! PrepareGroupFighting(incgIds, incg.map(gr => (gr.uuid, gr.group_category.id)).toMap)
      } else {
        log.debug("Continue not group fighting");
        sender() ! GroupBuilderNoGroupFighting(incgIds)
      }

      // Group Fighting
    case GoWithGroupBuilder4(groupID, incgIds, degree_of_association1, degree_of_association2, incompatibleBondList) =>
      val g = objectRefs(groupID).asInstanceOf[Group]
      val incg = incgIds.map(gid => objectRefs(gid).asInstanceOf[Group])

      log.debug("fighting incompatible groups");
      // try to break all incompatible groups
      if (fight_it_out_group_groups(g,1.0, incg,1.0, degree_of_association1, degree_of_association2)){
          // beat all competing groups
          print("won");
          self.forward(GoWithGroupBuilder5(groupID, incompatibleBondList, incgIds))
      } else {
        log.debug("couldn't break incompatible groups: fizzle");
        sender() ! Finished
      }

    case GoWithGroupBuilder5(groupID, incompatibleBondList, incgroup) =>
      val g = objectRefs(groupID).asInstanceOf[Group]
      val incb = incompatibleBondList.map(br => wsRefs(br.uuid).asInstanceOf[Bond])
      val incg = incgroup.map(gid => objectRefs(gid).asInstanceOf[Group])

      log.debug("destroy incompatible bonds " + incb.size)
      // destroy incompatible bonds
      for (b <- incb) b.break_bond()

      // create new bonds
      g.bond_list = ListBuffer.empty[Bond]

      self ! LookAHeadForNewBondCreation(sender(), g.uuid, 1, incg.map(_.uuid), List.empty[BondRep])


    case LookAHeadForNewBondCreation(s: ActorRef, groupID, i, incg, newBondList) =>
      val g = objectRefs(groupID).asInstanceOf[Group]
      if (i < g.object_list.size) {
        val ob1 = g.object_list(i-1)
        val ob2 = g.object_list(i)

        if (ob1.right_bond.isEmpty){
          val isForward = g.direction_category.isDefined && g.direction_category.get.id == SlipNode.id.right
          val from_obj = if (isForward) ob1 else ob2
          val to_obj = if (isForward) ob2 else ob1

          slipnet ! SlipnetLookAHeadForNewBondCreation(s, g.groupRep(), i, incg, newBondList, from_obj.uuid, to_obj.uuid)

        } else {
          self ! LookAHeadForNewBondCreation(s, groupID, i + 1, incg, ob1.right_bond.get.bondRep() :: newBondList)
        }
      } else {
        // destroy incompatible groups
        val incgSet: Set[String] = incg.toSet
        for (gr <- incgSet) {
          val grp = objectRefs(gr).asInstanceOf[Group]
          break_group(grp)
        }

        build_group(g)
// already done in build_group        g.activate_descriptions();
        print("building group");
        s ! Finished
      }

    case SlipnetLookAHeadForNewBondCreationResponse(s, g_rep, i, incg, newBondList, bond_categoryOpt, from_obj_id, to_obj_id, bond_facet,slipnetLeft,
    slipnetRight) =>
      log.debug("SlipnetLookAHeadForNewBondCreationResponse bond_categoryOpt " + bond_categoryOpt)
      bond_categoryOpt match {
        case Some(bond_category) =>
          val from_obj = objectRefs(from_obj_id)
          val to_obj = objectRefs(to_obj_id)
          val g = objectRefs(g_rep).asInstanceOf[Group]
          val from_obj_descriptor = from_obj.get_description(bond_facet)
          val to_obj_descriptor = to_obj.get_description(bond_facet)

          val nb = new Bond(from_obj,to_obj,bond_category,bond_facet, from_obj_descriptor,to_obj_descriptor, slipnetLeft,slipnetRight, slipnet)
          addBond(nb)
          self ! LookAHeadForNewBondCreation(s, g.uuid, i + 1, incg, nb.bondRep() :: newBondList)

        case None =>
          println("Big mess")
          s ! Finished
      }

    // Codelet.java.425
    case GoWithBondBuilder(temperature, bondID, bond_category_degree_of_association) =>
      val b = wsRefs(bondID).asInstanceOf[Bond]
      logTrying(b, b.left_obj)

      b.update_strength_value(activationBySlipNodeID, bond_category_degree_of_association, objects.toList);
      log.debug("strength = " + b.total_strength);
      //val competitors = b.get_incompatible_bonds();

      if ((objects.toList.contains(b.from_obj)) &&
        (objects.toList.contains(b.to_obj))) {
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
          log.debug("check for incompatible structures")
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
            log.debug("no incompatible bonds!")
            true
          }
          if (!brokeIncb) {
            log.debug("not  brokeIncb")
            sender() ! Finished
          } else {
            // fight all groups containing these objects
            log.debug("fight all groups containing these objects")
            val incg = WorkspaceFormulas.get_common_groups(b.from_obj, b.to_obj);
            val failedFight = if (incg.isEmpty) {
              log.debug("no incompatible groups!")
              false
            } else {
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

            // inverted groups and correspondences fighting
            log.debug("fight all incompatible correspondences")
            // fight all incompatible correspondences
            var incc: List[Correspondence] = List.empty[Correspondence]
            val wonOrContinue = if (b.left_obj.leftmost || b.right_obj.rightmost) {
              log.debug("fight all incompatible correspondences 1")

              if (b.direction_category.isDefined) {
                log.debug("fight all incompatible correspondences 2")
                incc = b.get_incompatible_correspondences(initial)
                log.debug("fight all incompatible correspondences incc " + incc)

                if (!incc.isEmpty) {
                  log.debug("trying to break incompatible correspondences")
                  if (!fight_it_out(b, 2.0, incc, 3.0)) {
                    false
                  } else {
                    log.debug("won")
                    true
                  }
                } else true
                //System.out.println("looking for incompatible correspondences")
              } else true
            } else true


            if (!wonOrContinue) {
              log.debug("lost the fight: Fizzle!")
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
          }
        }

      } else {
        log.debug("objects do no longer exists: Fizzle!");
        sender() ! Finished
      }

    case GoWithDescriptionStrengthTester(temperature, descriptionID) =>
      slipnet ! SetSlipNodeBufferValue(descriptionID, 100.0)
      val d = structureRefs(descriptionID).asInstanceOf[Description]
      d.update_strength_value(activationBySlipNodeID, objects.toList)

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
      val b = wsRefs(bondID).asInstanceOf[Bond]
      log.debug("GoWithBondStrengthTester " + b)
      sender() ! GoWithBondStrengthTesterResponse(b.bondRep())

    case GoWithBondStrengthTester2(temperature, bondID, bond_category_degree_of_association) =>
      val b = wsRefs(bondID).asInstanceOf[Bond]
      b.update_strength_value(activationBySlipNodeID, bond_category_degree_of_association, objects.toList)
      val strength = b.total_strength;
      val leftStringOpt = b.left_obj.wString
      val workingString = if (leftStringOpt.isDefined && leftStringOpt.get == initial) "initial" else "target"
      log.info(s"bond = ${b.toString} in ${workingString} string")

      val prob = WorkspaceFormulas.temperature_adjusted_probability(strength / 100.0, temperature)
      log.info("bond strength = " + strength)
      if (!WorkspaceFormulas.flip_coin(prob)) {
        log.debug("not strong enough: Fizzle!");
        sender() ! Finished
      } else {
        // it is strong enough - post builder  & activate nodes
        slipnet ! SetSlipNodeBufferValue(b.bond_facet.id, 100.0)
        if (b.from_obj_descriptor.isDefined)
          slipnet ! SetSlipNodeBufferValue(b.from_obj_descriptor.get.id, 100.0)
        if (b.to_obj_descriptor.isDefined)
          slipnet ! SetSlipNodeBufferValue(b.to_obj_descriptor.get.id, 100.0)

        log.info("succeeded: will post bond-builder");
        sender() ! GoWithBondStrengthTesterResponse2(strength)
      }


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

          val toOpt = chooseNeighbor(fromob, conditionalNeighbor, temperature)
          toOpt match {
            case None =>
              log.debug("GoWithTopDownBondScoutCategory | object has no neighbour - fizzle")
              sender() ! Finished

            case Some(toob) =>
              //log.debug(s"initial object chosen: $from in ${if (from.workspaceString() == initial) "initial" else "target"} string")
              // log.debug(s"ito object: $to")
              log.debug("to object : " + toob);

              // workspace_formulas.choose_bond_facet
              val fromdtypes = fromob.descriptions.toList.map(d => d.description_type)
              val todtypes = toob.descriptions.toList.map(d => d.description_type)

              sender() ! GoWithTopDownBondScoutWithResponse(
                fromob.workspaceObjectRep(),
                toob.workspaceObjectRep(),
                fromdtypes,
                todtypes
              )
          }
      }

    case GoWithTopDownBondScout2(fromobrep, toobrep, bond_facets) =>
      val fromob = objectRefs(fromobrep.uuid)
      fromob.wString match {
        case Some(fromobString) =>
          val object_probs = bond_facets.map(ob => {
            val t = total_description_type_support(ob, fromobString)
            log.debug("object_probs " + ob.id + " t " + t);
            t
          })
          val bond_facet = bond_facets(Utilities.valueProportionalRandomIndexInValueList(object_probs))
          log.debug("chosen bond facet :" + bond_facet.id)
          val toob = objectRefs(toobrep.uuid)

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
          log.debug("initial object: " + fromob);
          val conditional = if (directionID == "lf") conditionalLeftNeighbor else conditionalRightNeighbor
          val toOpt = chooseNeighbor(fromob, conditional, temperature)
          toOpt match {
            case None =>
              log.debug("GoWithTopDownBondScoutCategory | object has no neighbour - fizzle")
              sender() ! Finished

            case Some(toob) =>
              //log.debug(s"initial object chosen: $from in ${if (from.workspaceString() == initial) "initial" else "target"} string")
              // log.debug(s"ito object: $to")
              log.debug("to object : " + toob);

              // workspace_formulas.choose_bond_facet
              val fromdtypes = fromob.descriptions.toList.map(d => d.description_type)
              val todtypes = toob.descriptions.toList.map(d => d.description_type)

              sender() ! GoWithTopDownBondScoutWithResponse(
                fromob.workspaceObjectRep(),
                toob.workspaceObjectRep(),
                fromdtypes,
                todtypes
              )
          }
      }
    case GoWithTopDownGroupScoutCategory(slipNodeID: String, bondFocus: String, t, groupSlipnetInfo) =>
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
            log.debug("define direction");

            val mydirection = if (fromob.leftmost) Some(groupSlipnetInfo.right) else
              if (fromob.rightmost) Some(groupSlipnetInfo.left) else None
            log.debug("mydirection " + mydirection)
            sender() ! GoWithTopDownGroupScoutCategoryResponse(mydirection, fromob.workspaceObjectRep())
          }
      }
    case GoWithTopDownGroupScoutCategory2(group_category, mydirection, fromobrep, bond_category, t, groupSlipnetInfo) =>
      log.debug("GoWithTopDownGroupScoutCategory2")
      log.debug("check possible group");

      var fromob = objectRefs(fromobrep.uuid)
      val first_bondOpt = if (mydirection == groupSlipnetInfo.left) fromob.left_bond else fromob.right_bond
      if ((first_bondOpt.isEmpty) || (first_bondOpt.get.bond_category != bond_category)) {
        // check the other side of object
        val newFirst_bondOpt = if (mydirection == groupSlipnetInfo.right) fromob.left_bond else fromob.right_bond
        if ((newFirst_bondOpt.isEmpty) || (newFirst_bondOpt.get.bond_category != bond_category)) {
          // this is a single letter group
          if ((bond_category.id != "sm") || (!(fromob.isInstanceOf[Letter]))) {
            log.debug("no bonds of this type found: fizzle!");
            sender() ! Finished
          }
          else {
            log.debug("thinking about a single letter group");
            val oblist = ListBuffer(fromob)
            val letter_category = "lc"

            // This group is fake. It is just for prob
            val g = new Group(
              fromob.wString.get,
              group_category = groupSlipnetInfo.samegrp,
              direction_category = None,
              bond_facet = groupSlipnetInfo.letter,
              object_list = oblist,
              ListBuffer.empty[Bond],
              bond_category,
              groupSlipnetInfo,
              t,
              slipnet,
              activationBySlipNodeID
            )

            val prob = g.single_letter_group_probability(Workspace.activationWithSlipNodeRep(activationBySlipNodeID, groupSlipnetInfo.length), t)
            if (Random.rnd() < prob) {
              // propose single letter group
              log.debug("single letter group proposed");


              val slipnetGroup_cat = groupSlipnetInfo.samegrp
              val direction_category = Option.empty[SlipNodeRep]
              val bond_facet = groupSlipnetInfo.letter_category
              val object_list = oblist.toList.map(_.workspaceObjectRep())
              val bond_list = List.empty[BondRep]

              sender() ! GoWithTopDownGroupScoutCategory2Response(
                slipnetGroup_cat,
                direction_category,
                bond_facet,
                object_list,
                bond_list
              )
            } else {
              log.debug("failed")
              sender() ! Finished
            }
          }
        }
      } else {
        self.forward(CommonSubProcessing(group_category, fromob.uuid,first_bondOpt.get.uuid, bond_category))
      }

    case CommonSubProcessing(group_category, fromobUUID, firstBondUUID, bond_category: SlipNodeRep) =>
      var fromob = objectRefs(fromobUUID)
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
            group_category,
            direction,
            bond_facet,
            object_rep_list,
            bond_rep_list
          )
        }
      }


    case  WorkspaceProposeGroup2(
          object_rep_list,
          bls,
          group_category,
          direction_category,
            bond_facet,
    bond_category
    ) =>
      log.debug("WorkspaceProposeGroup2")





    // Corerack.java.298, propose_group
    case WorkspaceProposeGroup(
      object_rep_list,
      bls,
      group_category,
      direction_category,
      bond_facet,
      bond_category,
      groupSlipnetInfo: GroupSlipnetInfo,
      t: Double
    ) =>
      log.debug("WorkspaceProposeGroup")
      val wStringFromWo = object_rep_list.head
      val wString = objectRefs(wStringFromWo.uuid).wString.get
      val object_list = object_rep_list.map(ol => objectRefs(ol.uuid)).to[ListBuffer]
      val bond_list = bls.map(ol => wsRefs(ol.uuid).asInstanceOf[Bond]).to[ListBuffer]


      val ng = new Group(
        wString,
        group_category,
        direction_category,
        bond_facet,
        object_list,
        bond_list,
        bond_category,
        groupSlipnetInfo,
        t,
        slipnet,
        activationBySlipNodeID
      )
      log.debug("WorkspaceProposeGroup. Register group " + ng.uuid)
      objectRefs += (ng.uuid -> ng)
      sender() ! WorkspaceProposeGroupResponse(ng.uuid)

    case GoWithTopDownGroupScoutDirection(direction, mydirection: SlipNodeRep, fromobrep: WorkspaceObjectRep, t, gsi) =>
      var fromob = objectRefs(fromobrep.uuid)

      log.debug("check possible group " + fromob);

      val first_bondOpt = if (mydirection.id.equals(gsi.left.id)) fromob.left_bond else fromob.right_bond
      log.debug("check possible group. first_bond " + first_bondOpt);
      log.debug("check possible group. first_bond " + fromob.right_bond);

      val newDirection = if ((first_bondOpt.isDefined) && (first_bondOpt.get.direction_category.isEmpty)) None else Some(direction)
      log.debug("check possible group. direction " + newDirection);

      val (fizzle, fb, dir) = if ((first_bondOpt.isEmpty) || (first_bondOpt.isDefined && !first_bondOpt.get.direction_category.equals(newDirection))) {
        val newFirst_bondOpt = if (mydirection.id.equals(gsi.right.id)) fromob.left_bond else fromob.right_bond
        log.debug("check possible group. new first_bond " + newFirst_bondOpt + " new first_bond direction_category " + newFirst_bondOpt.map(nfb => nfb.direction_category));
        log.debug("check possible group. first_bond " + fromob.left_bond);

        val newDirection2 = if ((newFirst_bondOpt.isDefined) && (newFirst_bondOpt.get.direction_category.isEmpty)) None else newDirection
        log.debug("check possible group. newDirection2 " + newDirection2);

        if ((newFirst_bondOpt.isEmpty) || (newFirst_bondOpt.isDefined && !newFirst_bondOpt.get.direction_category.equals(newDirection2))) {
          (true, newFirst_bondOpt, newDirection2)
        } else (false, newFirst_bondOpt, newDirection2)
      } else (false, first_bondOpt, newDirection)

      log.debug("Check fizzle")
      if (fizzle) {
        log.debug("no possible group: fizzle!")
        sender() ! Finished
      } else {
        log.debug("Check fb " + fb)

        fb match {
          case Some(first_bond) =>
            val bond_category = first_bond.bond_category;
            log.debug("Check bond_category " + bond_category + " first_bond.uuid " + first_bond.uuid)

            sender() ! GoWithTopDownGroupScoutDirectionResponse(bond_category, first_bond.uuid)

          case None =>
            log.debug("no bond in the "+ dir +" direction was found: fizzle.");
            sender() ! Finished
        }
      }

    case GoWithTopDownGroupScoutDirection2(group_categoryOpt, fromob, firstBondUUID, bond_category: SlipNodeRep) =>
      group_categoryOpt match {
        case Some(group_category) =>
          self.forward(CommonSubProcessing(group_category, fromob.uuid, firstBondUUID, bond_category))
        case None =>
          print("Look's like we can't go this way ? : fizzle!")
          sender() ! Finished
      }

    case GoWithGroupScoutWholeString(t: Double) =>
      log.debug("GoWithGroupScoutWholeString t:" + t);
      log.debug("about to choose string");
      val wString = if (Random.rnd() > 0.5) target else initial
      val wStringText = if (wString == initial) "initial" else "target"
      log.debug(s"${wStringText} string selected")

      // find leftmost object & the highest group to which it belongs
      var leftmost: WorkspaceObject = null
      for (w <- wString.objects) {
        log.debug("leftmost iterate " + w + " leftmost " + w.leftmost);

        if (w.leftmost) leftmost = w;
      }
      log.debug("leftmost " + leftmost);

      while ((leftmost.group.isDefined)&&(leftmost.group.get.bond_category.id==SlipNode.id.sameness))
        leftmost=leftmost.group.get
      log.debug("leftmost2 " + leftmost);

      sender() ! GoWithGroupScoutWholeStringResponse(leftmost.workspaceObjectRep())


    case GoWithGroupScoutWholeString3(leftMostUUID) =>
      var leftmostgroup = objectRefs(leftMostUUID)
      sender() ! GoWithGroupScoutWholeString3Response(leftmostgroup.workspaceObjectRep())

      // leftmost.group.map(_.groupRep())
    //   Codelet.java.807
    case GoWithGroupScoutWholeString2(left_mostRep, slipnetLeft, slipnetRight) =>
      log.debug("GoWithGroupScoutWholeString2")
      var leftmost = objectRefs(left_mostRep.uuid)
      if (leftmost.spans_string){
        // the object already spans the string - propose this object
        val g = leftmost.asInstanceOf[Group]

        log.debug("selected object already spans string: propose");

        sender() ! GroupScoutWholeString2Response(
          g.group_category,
          g.direction_category,
          g.bond_facet,
          g.object_list.toList.map(_.workspaceObjectRep()),
          g.bond_list.toList.map(_.bondRep())
        )
      } else {
        log.debug("GoWithGroupScoutWholeString2. leftmost.spans_string is false")

        var bond_list = ListBuffer.empty[Bond]
        var object_list = ListBuffer(leftmost)

        while (leftmost.right_bond.isDefined){
          val right_bond = leftmost.right_bond.get
          bond_list += right_bond
          leftmost = right_bond.right_obj
          object_list += leftmost
        }
        log.debug("leftmost.rightmost " + leftmost.rightmost);
        if (!(leftmost.rightmost)){
          log.debug("no spanning bonds - fizzle");
          sender() ! Finished
        } else {
          // choose a random bond from list
          val posRaw = (Random.rnd() * bond_list.size).toInt
          val pos = if (posRaw >= bond_list.size) 0 else posRaw

          val chosen_bond = bond_list(pos)
          val bond_category = chosen_bond.bond_category
          val direction_category = chosen_bond.direction_category;
          val bond_facet = chosen_bond.bond_facet;
          val bond_listOpt = WorkspaceFormulas.possible_group_bond_list(bond_category,
            direction_category, bond_facet, bond_list.toList, slipnetLeft, slipnetRight, slipnet)
          if (bond_listOpt.isEmpty){
            log.debug("no possible group - fizzle");
            sender() ! Finished
          } else {
            log.debug("proposing "+bond_category.id+" group");

            sender() ! GroupScoutWholeString3Response(
              bond_category,
              direction_category,
              bond_facet,
              object_list.toList.map(_.workspaceObjectRep()),
              bond_list.toList.map(_.bondRep())
            )
          }


        }

      }

    case GoWithGroupStrengthTester(temperature: Double, groupID: String) =>
      log.debug(s"GoWithGroupStrengthTester groupID $groupID")
      val g = objectRefs(groupID).asInstanceOf[Group]
      log.debug(s"GoWithGroupStrengthTester groupID ${g.group_category}")

      sender() ! GoWithGroupStrengthTesterResponse(g.group_category.id)

    case GoWithGroupStrengthTester2(temperature: Double, groupID: String, degree_of_association: Double) =>
      val g = objectRefs(groupID).asInstanceOf[Group]

      g.update_strength_value(degree_of_association)
      val strength = g.total_strength;
      val workingString = if (g.wString == initial) "initial" else "target"
      log.info(s"evaluating group = ${groupID} in ${workingString} string")

      val prob = WorkspaceFormulas.temperature_adjusted_probability(strength / 100.0, temperature)
      log.info(s"strength = $strength, adjusted prob.= $prob")
      if (Random.rnd() > prob){
        println("not strong enough: fizzled!");
        sender() ! Finished
      } else {
        // it is strong enough - post builder  & activate nodes
        sender() ! GoWithGroupStrengthTesterResponse2(g.groupRep(), strength)

      }
    case GoWithRuleScout =>
      log.debug("GoWithRuleScout unreplaced_objects() " + unreplaced_objects())
      if (!unreplaced_objects().isEmpty) {
        log.debug("not all replacements have been found. Fizzle");
        sender() ! Finished
      } else {
        log.debug("all replacements found");

        var changedOpt: Option[WorkspaceObject] = None
        // find changed object;
        for (wo <- initial.objects) {
          log.debug("rule-scout " + wo + " changed " + wo.changed)
          if (wo.changed) changedOpt = Some(wo)
        }
        log.debug("rule-scout changed " + changedOpt);

        // if there are no changed objects, propose a rule with no changes
        if (changedOpt.isEmpty) {
          log.debug("there are no changed objects!");
          log.debug("proposing null rule");
          val facet = Option.empty[String]
          val description = Option.empty[String]
          val objectCategory = Option.empty[String]
          val relation = Option.empty[String]

          sender() ! RuleScoutProposeRule(None, None, None, None)
        } else {
          // Codelet.java.1080
          log.debug("changed not empty");

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
      log.debug("GoWithRuleScout2")
      val changed = objectRefs(changedRep.uuid)
      var object_list = ListBuffer.empty[SlipNodeRep]
      val positionOpt = changed.get_description(string_position_category);
      if (positionOpt.isDefined) {
        log.debug("GoWithRuleScout2 positionOpt " + positionOpt)

        object_list += positionOpt.get
      };
      val letterOpt = changed.get_description(letter_category)
      val noLetterFound = initial.objects.find(wo => (wo.get_description_type(letterOpt).isDefined)&&(wo!=changed))
      var only_letter = noLetterFound.isEmpty;  // if this is true, the letter can be thought of as a distinguishing feature
      if (only_letter) object_list += letterOpt.get

      log.debug("GoWithRuleScout2 changed.correspondence.isDefined " + changed.correspondence.isDefined)


      if (changed.replacement.isEmpty) {
        log.debug("Changed replacement is empty. Fizzle")
        sender() ! Finished
      } else {
        val replacement = changed.replacement.get
        val letterCategory = replacement.to.get_description_with_id(SlipNode.id.letter_category)
        if (letterCategory.isEmpty) {
          log.debug("Changed replacement get_description_with_id letter_category is empty. Fizzle")
          sender() ! Finished
        } else {


          // if this object corresponds to another object in the workspace
          // object_list = the union of this and the distingushing descriptors
          if (changed.correspondence.isDefined) {

            log.debug("letterCategory " + letterCategory.get)

            val obj2 = changed.correspondence.get.obj2
            val slippagesShell = slippage_list()

            sender() ! GoWithRuleScout2Response(
              obj2.workspaceObjectRep(),
              slippagesShell,
              object_list.toList,
              replacement.relation,
              letterCategory.get
            )
          } else {
            sender() ! GoWithRuleScout3Response(object_list.toList, letterCategory.get)
          }
        }

      }

    case GoWithRuleScout3(slippage_list_rep, object_list,obj2_rep, letterCategory) =>
      val obj2 = objectRefs(obj2_rep.uuid)
      val new_object_list = object_list.map(s => {
        val sID = Rule.apply_slippages(Some(s), slippage_list_rep)
        if (sID.isDefined && obj2.has_slipnode_description(sID.get) && obj2.distinguishing_descriptor(sID.get.id))
            Some(sID.get)
        else None
      }).flatten
      sender() ! GoWithRuleScout3Response(new_object_list, letterCategory)

    case GoWithRuleStrengthTester(temperature: Double, ruleID) =>
      val rule = structureRefs(ruleID).asInstanceOf[Rule]
      log.debug("testing: "+rule.toString())

      rule.update_strength_value(initial.objects.toList, slippage_list())
      val strength = rule.total_strength
      val prob = WorkspaceFormulas.temperature_adjusted_probability(strength / 100.0, temperature)
      log.info(s"strength = $strength, adjusted prob.= $prob")
      if (Random.rnd() > prob){
        log.debug("not strong enough: fizzled!");
        sender() ! Finished
      } else {
        // it is strong enough - post builder  & activate nodes
        sender() ! GoWithRuleStrengthTesterResponse(rule.workspaceStructureRep(), strength)
      }

    case GoWithRuleBuilder(ruleID) =>
      val myrule = structureRefs(ruleID).asInstanceOf[Rule]
      log.debug("trying to build "+myrule.toString());
      if (myrule.rule_equal(rule)){
        // rule already exists: fizzle, but activate concepts
        log.debug("already exists - activate concepts");
        myrule.activate_rule_descriptions()
        sender() ! Finished
      } else {
        log.debug("GoWithRuleBuilder. does not exists")
        myrule.update_strength_value(initial.objects.toList, slippage_list())
        val strength = myrule.total_strength
        if (strength == 0.0) {
          log.debug("the rule is incompatible with correspondences: Fizzle");
          sender() ! Finished
        } else {
          // fight against other rules
          val anyLost = if (rule.isDefined){
            log.debug("Fighting against existing rule");
            log.debug("existing rule strength: "+ rule.get.total_strength);
            log.debug("this rule strength: "+ rule.get.total_strength);
            if (!rule_vs_rule(
              myrule,1.0,rule.get,1.0)){
              // lost the fight
              true
            }
            else {
              println("won!")
              false
            };
          } else false

          if (anyLost) {
            println("lost the fight: fizzle!");
            sender() ! Finished
          } else {
            build_rule(myrule)
            log.debug("building rule");
            sender() ! Finished
          }

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
      log.debug("GoWithImportantObjectCorrespondenceScout")
      val obj1Opt = chooseObject(initial.objects.toList, TemperatureAjustmentVariable.Relative_importance, t)

      obj1Opt match {
        case Some(obj1) =>
          println("object chosen from initial string: "+obj1);
          val v = obj1.relevant_distinguishing_descriptors(activationBySlipNodeID)
          println("relevant_distinguishing_descriptors: "+v);

          sender() ! GoWithImportantObjectCorrespondenceScoutResponse(obj1.workspaceObjectRep(), v)

        case None =>
          println("Popopo obj1 is null: Fizzle")
          sender() ! Finished
      }

      // Codelet.java.1333
    case GoWithImportantObjectCorrespondenceScout2 =>
      log.debug("GoWithImportantObjectCorrespondenceScout2")
      log.debug("will slippage_list:")
      val sShell = slippage_list()
      log.debug("did slippage_list: " + sShell)
      sender() ! GoWithImportantObjectCorrespondenceScout2Response(sShell)

    // Codelet.java.1334 - 1369
    case GoWithImportantObjectCorrespondenceScout3(slippage_list_rep: List[ConceptMappingRep], s, t, o1) =>
      log.debug("GoWithImportantObjectCorrespondenceScout3")
      val obj1_descriptorRaw = slippage_list_rep.find(cm => {
        cm.descriptor1.id.equals(s.id)
      }).map(_.descriptor2)
      val obj1_descriptor = if (obj1_descriptorRaw.isEmpty) s else obj1_descriptorRaw.get
      log.debug("obj1_descriptor: " + obj1_descriptor);

      val obj2_candidates = target.objects.toList.filter(wo => {
        wo.relevant_descriptions(activationBySlipNodeID).find(d => {
          d.descriptor.isDefined && d.descriptor.get.id.equals(obj1_descriptor.id)
        }).isDefined
      })
      log.debug("Test obj2 candidate size " + obj2_candidates.size);
      if (obj2_candidates.isEmpty) {
        log.debug("no corresponding objects found: fizzle")
        sender() ! Finished
      } else {
        val obj2Opt = chooseObject(obj2_candidates, TemperatureAjustmentVariable.Inter_string_salience, t)
        log.debug("obj2 " + obj2Opt)

        obj2Opt match {
          case Some(obj2) =>
            var flip_obj2 = false;
            val obj1 = objectRefs(o1.uuid)
            log.debug("trying a correspondence between "+obj1+" and "+obj2);

            // if one object spans the string and the other doesn't - fizzle
            if (obj1.spans_string!=obj2.spans_string){
              // fizzle
              log.debug("only one object spans the string: fizzle");
              sender() ! Finished
            } else {
              log.debug("get the posible concept-mappings");

              sender() ! GoWithImportantObjectCorrespondenceScout3Response(obj2.workspaceObjectRep(), obj2.workspaceObjectRep())
            }

          case None =>
            log.debug("Should never happen ? but obj2 is null: fizzle")
            sender() ! Finished
        }

      }

    // Codelet.java.1478
    // Codelet.java.1438
    case GoWithCorrespondenceBuilder(temperature, correponsdenceID) =>
      val c = wsRefs(correponsdenceID).asInstanceOf[Correspondence]
      val obj1 = c.obj1
      val obj2 = c.obj2
      log.debug("trying correspondence from "+obj1+" to "+obj2)
      sender() ! GoWithCorrespondenceBuilderResponse(obj2.workspaceObjectRep())

      // Codelet.java.1482
    case GoWithCorrespondenceBuilder2(correponsdenceID, futureGroupRep,t) =>
      val c = wsRefs(correponsdenceID).asInstanceOf[Correspondence]
      val obj1 = c.obj1
      val obj2 = c.obj2

      val obj2Group = obj2.asInstanceOf[Group]
      val flippedGroup = flippedGroupWithFutureGroup(obj2Group,futureGroupRep,t)


      if (( !objects.toList.contains(obj1) || !objects.toList.contains(obj2)) &&
            !c.flip_obj2 && target.group_present(flippedGroup).isEmpty
      ) {
        println("objects no longer exist");
        sender() ! Finished
      } else {
        sender() ! GoWithCorrespondenceBuilder9Response
      }

    case GoWithCorrespondenceBuilder9(correponsdenceID,t) =>
      val c = wsRefs(correponsdenceID).asInstanceOf[Correspondence]
      // if this correspondence is present, add any new concept mappings
      if (correspondence_present(c)) {
        // if the correspondence exists, activate concept mappings
        // and add new ones to the existing corr.

        //val existing = (c.obj1).correspondence.get.asInstanceOf[Correspondence]
        log.debug("GoWithCorrespondenceBuilder9. present")
        sender() ! GoWithCorrespondenceBuilder2Response(c.concept_mapping_list)
      } else {
        log.debug("GoWithCorrespondenceBuilder9. not present")

        val correspondences = initial.objects.map(w => w.correspondence).flatten
        val correspondenceReps = correspondences.toList.map(_.correspondenceRep)
        val correspondence = c.correspondenceRep()

        val wcreps = workspaceCorrespondences().map(_.correspondenceRep())

        sender() ! GoWithCorrespondenceBuilder3Response(correspondence, correspondenceReps, wcreps)
      }


    case GoWithCorrespondenceBuilder3(correponsdenceID, updatedCorrespondenceCMReps) =>
      log.debug("Workspace. GoWithCorrespondenceBuilder3")
      val c = wsRefs(correponsdenceID).asInstanceOf[Correspondence]
      val obj1 = c.obj1
      val existing = obj1.correspondence.get
      existing.addConceptMappings(updatedCorrespondenceCMReps)
      log.debug("correspondence is already present");
      log.debug("activate concept mappings & fizzle");
      sender() ! Finished



    case GoWithCorrespondenceBuilder4(correponsdenceID, correspondenceReps, internal_strength, correpondenceData) =>
      log.debug("Workspace. GoWithCorrespondenceBuilder4")
      val c = wsRefs(correponsdenceID).asInstanceOf[Correspondence]
      log.debug("Workspace. GoWithCorrespondenceBuilder4 - 2")

      val incc = correspondenceReps.map(co => structureRefs(co.uuid).asInstanceOf[Correspondence])
      log.debug(s"Workspace. GoWithCorrespondenceBuilder4 - 3 ${incc.isEmpty}")

      // fight against all correspondences
      val anyFightLost = if (!incc.isEmpty){
        log.debug("fighting incompatible correspondences");
        val won = incc.find(comp => {
          val csize = (c.obj1).letter_span() + (c.obj2).letter_span();
          val compsize = (comp.obj1).letter_span() + (comp.obj2).letter_span();
          !(correspondence_vs_correspondence(
            c, csize, comp, compsize, internal_strength, correpondenceData))
        }).isEmpty
        if (won) log.debug("won!")
        !won
      } else false

      if (anyFightLost) {
        log.debug("not strong enough: fizzle");
        sender() ! Finished
      } else {
        log.debug("Workspace. GoWithCorrespondenceBuilder4: WON");

        var incompatible_bond = Option.empty[Bond]
        var incompatible_group = Option.empty[Group]

        // if there is an incompatible bond then fight against it
        if (((c.obj1).leftmost) || ((c.obj1).rightmost) &&
          ((c.obj2).leftmost) || ((c.obj2).rightmost)) {
          // search for the incompatible bond
          log.debug("Workspace. GoWithCorrespondenceBuilder4: search for the incompatible bond");

          val incompatible_bond_base = c.get_incompatible_bond();
          val wcreps = workspaceCorrespondences().map(_.correspondenceRep())

          sender ! GoWithCorrespondenceBuilder4Response1(c.correspondenceRep(),incompatible_bond_base, wcreps)
        } else {
          sender ! GoWithCorrespondenceBuilder4Response2(None, internal_strength, correpondenceData)
        }
      }

    case GoWithCorrespondenceBuilder5(correponsdenceID: String, incompatible_bondOpt,
    internal_strength: Double,
    supporting_correspondences:Map[String, Boolean],
    bond_category_degree_of_associationOpt
    ) =>
      val c = wsRefs(correponsdenceID).asInstanceOf[Correspondence]
      var incompatible_group = Option.empty[Group]
      val anyLostFight = if (incompatible_bondOpt.isDefined){
        val incompatible_bond = wsRefs(incompatible_bondOpt.get.uuid).asInstanceOf[Bond]
        print("fighting incompatible bond");
        // bond found - fight against it
        if (!(correspondence_vs_bond(
          c,3.0,incompatible_bond,2.0, internal_strength, supporting_correspondences, bond_category_degree_of_associationOpt.get))){
          print("lost: fizzle!");
          true  // fizzle as it has lost
        } else {
          print("won");
          // won against incompatible bond
          incompatible_group = (c.obj2).group;
          if (incompatible_group.isDefined){
            print("fighting incompatible group");
            if (!(correspondence_vs_group(
              c,1.0,incompatible_group.get,1.0, internal_strength, supporting_correspondences))){
              print("lost: fizzle!");
              true  // fizzle as it has lost
            }
            print("won");
            false
          } else false
        }
      } else false
      if (anyLostFight) {
        sender() ! Finished
      } else {
        sender() ! GoWithCorrespondenceBuilder4Response2(incompatible_group.map(_.groupRep()),internal_strength, supporting_correspondences)
      }

    case GoWithCorrespondenceBuilder6(correponsdenceID, inccRep, incompatible_bondOpt, incompatible_group, internal_strength: Double,
    supporting_correspondences:Map[String, Boolean]) =>
      val c = wsRefs(correponsdenceID).asInstanceOf[Correspondence]
      // if there is an incompatible rule, fight against it
      val incompat_ruleOpt: Option[Rule] = if (rule.isDefined && incompatible_rule_corr(rule.get,c)) {
        rule
      } else None

      val anyLostFight = incompat_ruleOpt match {
        case Some(r) =>
          print("Fighting against incompatible Rule")
          if (!(correspondence_vs_rule(
            c,1.0,rule.get,1.0, internal_strength, supporting_correspondences))){
            print("lost: fizzle!");
            true  // fizzle as it has lost
          } else {
            print("won");
            false
          }
        case None => false
      }
      if (anyLostFight) {
        sender() ! Finished
      } else {
        val incc = inccRep.map(crep => wsRefs(crep.uuid).asInstanceOf[Correspondence])
        for (comp <- incc){
          break_correspondence(comp)
        }
        // break incompatible group and bond if they exist
        if (incompatible_bondOpt.isDefined) {
          val incompatible_bond = wsRefs(incompatible_bondOpt.get.uuid).asInstanceOf[Bond]
          break_bond(incompatible_bond)
        }
        if (incompatible_group.isDefined) {
          val g = objectRefs(incompatible_group.get.uuid).asInstanceOf[Group]
          break_group(g)
        }
        if (incompat_ruleOpt.isDefined) {
          break_rule(incompat_ruleOpt.get);
        }
        log.debug("building correspondence");
        addStructure(c)
        c.build_correspondenceStep1()
        sender() ! GoWithCorrespondenceBuilder6Response(c.correspondenceRep())
      }

    // Second part of buid_correpondence
    case GoWithCorrespondenceBuilder7(correponsdenceID,accessory_concept_mapping_list) =>
      log.debug("Workspace. GoWithCorrespondenceBuilder7")
      val c = wsRefs(correponsdenceID).asInstanceOf[Correspondence]
      c.accessory_concept_mapping_list ++ accessory_concept_mapping_list
      val obj1 = c.obj1
      val obj2 = c.obj2

      // we need to use the slipnet again so let's create a structure for it
      val groupObjs = if (obj1.isInstanceOf[Group] && obj2.isInstanceOf[Group]) {
        val group1 = obj1.asInstanceOf[Group]
        val group2 = obj2.asInstanceOf[Group]

        Some(ConceptMappingParameters(
            obj1.workspaceObjectRep(),
            obj2.workspaceObjectRep(),
            group1.bond_descriptions.toList.map(d => d.descriptionRep),
            group2.bond_descriptions.toList.map(d => d.descriptionRep)
          ))
      } else None
      sender() ! GoWithCorrespondenceBuilder7Response(groupObjs)


    case GoWithCorrespondenceBuilder8(correspondenceID,accessory_concept_mapping_list) =>
      val c = wsRefs(correspondenceID).asInstanceOf[Correspondence]
      c.addAccessoryConceptMappings(accessory_concept_mapping_list)
      sender()! GoWithCorrespondenceBuilder8Response(c.concept_mapping_list)

    case GoWithCorrespondenceStrengthTester(correponsdenceID: String, futureGroupRep: FutureGroupRep, t:Double) =>
      val c = wsRefs(correponsdenceID).asInstanceOf[Correspondence]
      val obj1 = c.obj1
      val obj2 = c.obj2

      val obj2Group = obj2.asInstanceOf[Group]
      val flippedGroup = flippedGroupWithFutureGroup(obj2Group,futureGroupRep,t)


      if (( !objects.toList.contains(obj1) || !objects.toList.contains(obj2)) &&
        !c.flip_obj2 && target.group_present(flippedGroup).isEmpty
      ) {
        println("objects no longer exist");
        sender() ! Finished
      } else {
        sender() ! GoWithCorrespondenceStrengthTesterResponse
      }

    case GoWithCorrespondenceStrengthTester2(correponsdenceID: String, t:Double) =>
      log.debug("GoWithCorrespondenceStrengthTester2. start update_strength_value")
      val c = wsRefs(correponsdenceID).asInstanceOf[Correspondence]
      val wcreps = workspaceCorrespondences().map(_.correspondenceRep())

      sender() ! GoWithCorrespondenceStrengthTesterResponse2(c.correspondenceRep(), wcreps)

    case GoWithCorrespondenceStrengthTester3(correponsdenceID: String, internalStrength: Double, supporting_correspondences:Map[String, Boolean], t:Double) =>
      log.debug("GoWithCorrespondenceStrengthTester3")
      val c = wsRefs(correponsdenceID).asInstanceOf[Correspondence]
      c.update_strength_value(internalStrength, workspaceCorrespondences(), supporting_correspondences)

      val strength = c.total_strength
      val prob = WorkspaceFormulas.temperature_adjusted_probability(strength / 100.0, t)
      log.info(s"strength = $strength, adjusted prob.= $prob")
      if (Random.rnd() > prob){
        print("not strong enough: fizzled!");
        sender() ! Finished
      } else {
        // it is strong enough - post builder  & activate nodes
        sender() ! GoWithCorrespondenceStrengthTesterResponse3(c.correspondenceRep(), strength)
      }

  }

  private def updateStructureStrengthValue(bondData: Map[String, Double], correspondenceData: Map[String, CorrespondenceUpdateStrengthData], ws: WorkspaceStructure) = {
    if (ws.isInstanceOf[Bond]) {
      val b = ws.asInstanceOf[Bond]
      val bond_category_degree_of_association = bondData(b.uuid)
      b.update_strength_value(activationBySlipNodeID, bond_category_degree_of_association, objects.toList)
    } else if (ws.isInstanceOf[Correspondence]) {
      val c = ws.asInstanceOf[Correspondence]
      val cData = correspondenceData(c.uuid)
      log.debug("cData.internal_strength " + cData.internal_strength)
      c.update_strength_value(cData.internal_strength, workspaceCorrespondences(), cData.supporting_correspondences)
    } else if (ws.isInstanceOf[Rule]) {
      val r = ws.asInstanceOf[Rule]
      r.update_strength_value(initial.objects.toList, slippage_list())
    } else {
      ws.update_strength_value(activationBySlipNodeID, objects.toList)
    }
  }

  def printCorrespondenceCMs(c: Correspondence) = {
    for (cm <- c.concept_mapping_list) {
      log.debug(s"Correspondence ${c.uuid} cm ${cm}")
    }
  }



  def workspaceCorrespondences(): List[Correspondence] = {
    structures.toList.filter(ws => ws.isInstanceOf[Correspondence]).asInstanceOf[List[Correspondence]]
  }


  def flippedGroupWithFutureGroup(group: Group, futureGroupRep: FutureGroupRep, t: Double) = {
    val bond_list = futureGroupRep.bond_list.map(bondRep => structureRefs(bondRep.uuid).asInstanceOf[Bond]).to[ListBuffer]

    new Group(
      group.wString.get,
      futureGroupRep.group_category,
      futureGroupRep.direction_category,
      futureGroupRep.bond_facet,
      group.object_list,
      bond_list,
      futureGroupRep.bond_category,
      futureGroupRep.groupSlipnetInfo,
      t,
      slipnet,
      activationBySlipNodeID
    )
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


  def unrelated_objects(): List[WorkspaceObject] = {
    // returns a list of all objects in the workspace that have at least
    // one bond slot open
    log.debug("unrelated_objects " + objects.size)
    val result = objects.toList.filter(wo => {
      val ok = ((wo.wString.isDefined && wo.wString.get == initial) || (wo.wString.isDefined && wo.wString.get == target));
      val left = ((wo.left_bond.isEmpty)&&(!wo.leftmost));
      val right = ((wo.right_bond.isEmpty)&&(!wo.rightmost));
      log.debug("unrelated_objects " + wo + " ok " + ok + " wo.spans_string " + wo.spans_string + " right " + right + " left " + left + " wo.left_bond.isEmpty " + wo.left_bond.isEmpty + " wo.leftmost " +wo.leftmost + " wo.right_bond.isEmpty " + wo.right_bond.isEmpty + " wo.rightmost " +wo.rightmost );
      ((ok)&&(!wo.spans_string) && ((right)||(left)))
    })
    log.debug("unrelated_objects result " + result.size)

    result
  }

  def ungrouped_objects(): List[WorkspaceObject] = {
    // returns a list of all objects in the workspace that are not
    // in a group
    objects.toList.filter(wo => {
      val ok = ((wo.wString.isDefined && wo.wString.get == initial) || (wo.wString.isDefined && wo.wString.get == target));
      ((ok) && (!wo.spans_string) && wo.group.isEmpty)
    })
  }

  def unreplaced_objects(): List[WorkspaceObject] = {
    // returns a list of all objects in the initial string that are not
    // replaced
    objects.toList.filter(wo => {
      val ok =((wo.wString.isDefined && wo.wString.get == initial) && (wo.isInstanceOf[Letter]))
      ok && wo.replacement.isEmpty
    })
  }

  def uncorresponding_objects(): List[WorkspaceObject] = {
    // returns a list of all objects in the initial string that are not
    // replaced
    objects.toList.filter(wo => {
      val ok =((wo.wString.isDefined && wo.wString.get == initial) || (wo.wString.isDefined && wo.wString.get ==target));
      ((ok)&&(wo.correspondence.isEmpty))
    })
  }


  def bonds(): List[Bond] = structures.toList.filter(s => s.isInstanceOf[Bond]).asInstanceOf[List[Bond]]
  def bondReps() = bonds().map(b => b.bondRep())
  def correspondences(): List[Correspondence] = structures.toList.filter(s => s.isInstanceOf[Correspondence]).asInstanceOf[List[Correspondence]]
  def correspondenceReps() = correspondences().map(c => c.correspondenceRep())

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

    if (str == initial) log.debug("initial string selected");
    else log.debug("target string selected");

    chooseObject(str.objects.toList, TemperatureAjustmentVariable.Intra_string_salience, t)
  }

  def workspaceStringBasedOnRelevanceAndUnhappiness(
                                         i_relevance: Double,
                                         i_unhappiness: Double,
                                         t_relevance: Double,
                                         t_unhappiness:Double
                                       ): WorkspaceString = if (
    (Random.rnd() * (i_relevance + i_unhappiness + t_relevance + t_unhappiness)) >
      (i_relevance + i_unhappiness)) target else initial

  def choose_bond_facet(fromob: WorkspaceObject, toob: WorkspaceObject) = {
    val fromdtypes = fromob.descriptions.toList.map(d => d.description_type)
    val todtypes = toob.descriptions.toList.map(d => d.description_type)
    (fromdtypes, todtypes)
  }



  def initialOrTargetText(ws: WorkspaceStructure) = if (ws.workspaceString().equals(initial)) "initial" else "target"

  def logTrying(wo: WorkspaceStructure, typeSource: WorkspaceStructure) = {
    val stringType = initialOrTargetText(typeSource)
    log.debug(s"trying to build ${wo.toString()} in ${stringType} string")
  }

  def total_description_type_support(description: SlipNodeRep, workspaceString: WorkspaceString): Double = {
    (Workspace.activationWithSlipNodeRep(activationBySlipNodeID,description) + local_description_type_support(description,workspaceString)) / 2.0
  }
  def local_description_type_support(description_type: SlipNodeRep, workspaceString: WorkspaceString): Double = {
    // returns the proportion of objects in the string that have
    // a description with this description_type
    val objectsWithSameString = objectRefs.values.filter(ob => ob.wString.isDefined && ob.wString.get == workspaceString)
    val number_of_objects = objectsWithSameString.size.toDouble

    val sameDescriptionTypeDescriptions: List[List[Description]] = objectsWithSameString.toList.map (ob => {
      ob.descriptions.toList.filter(d => d.description_type.id == description_type.id)
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
      addObject(l)
    }
  }

  // Add new description similar to those in argument
  def addDescriptionsToWorkspaceObject(wo: WorkspaceObject, descriptions: List[Description]) = {
    descriptions.filter(d => !wo.has_description(d)).map(d => {
      wo.add_description(d.description_type, d.descriptor)
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
      slipnet ! SetSlipNodeBufferValue(description.description_type.id, 100.0)
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

//  def build_correspondence(c: Correspondence) = {
//    addStructure(c)
//    c.build_correspondence()
//
//  }

  def build_group(group: Group) = {
    addObject(group)
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


  def break_rule(r: Rule) = {
    System.out.println("breaking rule ");
    rule = None
//    workspace.Workspace_Rule.Change_Caption("no rule");
  }

  def incompatible_rule_corr(r: Rule, c: Correspondence): Boolean = {
    if ((r==null)||(c==null)) return false;
    // find changed object
    val changed = initial.objects.find(wo => wo.changed)
    if (changed.isDefined && c.obj1!=changed.get) return false;
    c.concept_mapping_list.find(cm => {
      r.descriptor.isDefined && cm.descriptor1.id.equals(r.descriptor.get.id)
    }).isEmpty
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


  //def objects.toList: List[WorkspaceObject] = structures.toList.filter(s => s.isInstanceOf[WorkspaceObject]).asInstanceOf[List[WorkspaceObject]]

  def chooseObject(wos: List[WorkspaceObject], variable: String, temperature: Double) : Option[WorkspaceObject] = {
    //log.debug("objects.toList " + objects.toList)
    val nonModifieds = wos.filter(wo =>
      {
        println("chooseObject " + wo.workspaceString + " modified " + modified)
        wo.wString.isDefined && wo.wString.get != modified
      })
    log.debug("nonModifieds " + nonModifieds + " variable " + variable)

    chooseObjectFromList(nonModifieds, variable, temperature)
  }
  def chooseNeighbor(from: WorkspaceObject, conditional: (WorkspaceObject,WorkspaceObject) => Boolean , t: Double) : Option[WorkspaceObject] = {
    //println("workspace_objects " + objects.toList);

    val nonModifieds = objects.toList.filter(wo =>
      wo.workspaceString() == from.workspaceString() && conditional(from, wo)
    )
    //println("nonModifieds " + nonModifieds)
    chooseObjectFromList(nonModifieds, TemperatureAjustmentVariable.Intra_string_salience,t)
  }

  object TemperatureAjustmentVariable {
    val Intra_string_salience = "intra_string_salience"
    val Inter_string_salience = "inter_string_salience"
    val Total_salience = "total_salience"
    val Relative_importance = "relative_importance"
  }

  def chooseObjectFromList(list: List[WorkspaceObject], variable: String, t: Double): Option[WorkspaceObject] = {
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
      val oProbs = list.map(wo => Formulas.temperatureAdjustedValue(adjustment(wo), t))
      val index = Utilities.valueProportionalRandomIndexInValueList(oProbs)
      Option(list(index))
    }
  }

  def rule_vs_rule(s1: Rule,
                   w1: Double,
                   s2: Rule,
                   w2: Double): Boolean = {

    s1.update_strength_value(initial.objects.toList, slippage_list())
    s2.update_strength_value(initial.objects.toList, slippage_list())
    complete_structure_vs_structure(s1,w1,s2,w2)
  }

  def correspondence_vs_correspondence(s1: Correspondence,
                                       w1: Double,
                                       s2: Correspondence,
                                       w2: Double,
                                       internal_strength: Double,
                                       supporting_correspondences:Map[String, Boolean]
                                      ): Boolean = {
    s1.update_strength_value(internal_strength, workspaceCorrespondences(), supporting_correspondences)
    s2.update_strength_value(internal_strength, workspaceCorrespondences(), supporting_correspondences)
    complete_structure_vs_structure(s1,w1,s2,w2)
  }

  def correspondence_vs_bond(s1: Correspondence,
                                       w1: Double,
                                       s2: Bond,
                                       w2: Double,
                                       internal_strength: Double,
                                       supporting_correspondences:Map[String, Boolean],
                                       bond_category_degree_of_association: Double
                                      ): Boolean = {
    s1.update_strength_value(internal_strength, workspaceCorrespondences(), supporting_correspondences)
    s2.update_strength_value(activationBySlipNodeID, bond_category_degree_of_association, objects.toList)
    complete_structure_vs_structure(s1,w1,s2,w2)
  }

  def correspondence_vs_group(s1: Correspondence,
                                       w1: Double,
                                       s2: Group,
                                       w2: Double,
                                       internal_strength: Double,
                                       supporting_correspondences:Map[String, Boolean]
                                      ): Boolean = {
    s1.update_strength_value(internal_strength, workspaceCorrespondences(), supporting_correspondences)
    s2.update_strength_value(activationBySlipNodeID, objects.toList)
    complete_structure_vs_structure(s1,w1,s2,w2)
  }

  def correspondence_vs_rule(s1: Correspondence,
                             w1: Double,
                             s2: Rule,
                             w2: Double,
                             internal_strength: Double,
                             supporting_correspondences:Map[String, Boolean]
                            ): Boolean = {
    s1.update_strength_value(internal_strength, workspaceCorrespondences(), supporting_correspondences)
    s2.update_strength_value(initial.objects.toList, slippage_list())
    complete_structure_vs_structure(s1,w1,s2,w2)
  }
  def group_vs_bond(s1: Group,
                             w1: Double,
                             s2: Bond,
                             w2: Double,
                             bond_category_degree_of_association: Double
                            ): Boolean = {
    s1.update_strength_value(activationBySlipNodeID, objects.toList)
    s2.update_strength_value(activationBySlipNodeID, bond_category_degree_of_association, objects.toList)
    complete_structure_vs_structure(s1,w1,s2,w2)
  }


  def structure_vs_structure(s1: WorkspaceStructure,
                             w1: Double,
                             s2: WorkspaceStructure,
                             w2: Double): Boolean = {
    s1.update_strength_value(activationBySlipNodeID, objects.toList)
    s2.update_strength_value(activationBySlipNodeID, objects.toList)
    complete_structure_vs_structure(s1,w1,s2,w2)
  }

//    s1.update_strength_value(activationBySlipNodeID,objects.toList);
//    s2.update_strength_value(activationBySlipNodeID,objects.toList);
  def complete_structure_vs_structure(s1: WorkspaceStructure,
                                      w1: Double,
                                      s2: WorkspaceStructure,
                                      w2: Double) = {
    val vs1 = s1.total_strength*w1;
    val vs2 = s2.total_strength*w2;
    val v1 = Formulas.temperatureAdjustedValue(vs1, chaleur)
    val v2 = Formulas.temperatureAdjustedValue(vs2, chaleur)
    !(((v1+v2) * Random.rnd())>v1)
  }

  def group_vs_group(s1: Group,
                    w1: Double,
                    s2: Group,
                    w2: Double,
                     degree_of_association1: Double,
                       degree_of_association2: Double

                   ): Boolean = {
    s1.update_strength_value(degree_of_association1)
    s2.update_strength_value(degree_of_association2)
    complete_structure_vs_structure(s1,w1,s2,w2)
  }

  def fight_it_out_group_bonds(wo: Group, v1: Double, structs: List[Bond], v2: Double, degOfAssos: Map[String, Double]): Boolean = {
    if (structs.isEmpty) {
      true
    } else {
      !structs.find(ws => {
        val bond_category_degree_of_association = degOfAssos(ws.uuid)
        !group_vs_bond(wo,v1,ws,v2,bond_category_degree_of_association)
      }).isDefined
    }
  }


  def fight_it_out_group_groups(wo: Group, v1: Double, structs: List[Group], v2: Double, degree_of_association1: Double, degOfAssos: Map[String, Double]): Boolean = {
    if (structs.isEmpty) {
      true
    } else {
      !structs.find(ws => {
        val degree_of_association2 = degOfAssos(ws.uuid)
        !group_vs_group(wo,v1,ws,v2,degree_of_association1, degree_of_association2)
      }).isDefined
    }
  }


  def fight_it_out(wo: WorkspaceStructure, v1: Double, structs: List[WorkspaceStructure], v2: Double): Boolean = {
    if (structs.isEmpty) {
      true
    } else {
      !structs.find(ws => (!structure_vs_structure(wo,v1,ws,v2))).isDefined
    }
  }





  // From workspace_formulas.java.26
  def update_temperature(): Double = {
    calculate_intra_string_unhappiness()
    calculate_inter_string_unhappiness()
    calculate_total_unhappiness()
    System.out.println("intra_string_unhappiness " + intra_string_unhappiness + " inter_string_unhappiness " + inter_string_unhappiness + " total_unhappiness " + total_unhappiness);

    val rule_weakness = if (rule.isDefined){
      rule.get.update_strength_value(initial.objects.toList, slippage_list());
      100.0-rule.get.total_strength
    } else 100.0
    System.out.println("rule_weakness " +rule_weakness);

    actual_temperature = if (clamp_temperature) 100.0 else Formulas.weighted_average(total_unhappiness,0.8, rule_weakness,0.2)
    // Not useful Temperature.Update(actual_temperature);
    System.out.println("formulas.actual_temperature " + actual_temperature);

    System.out.println("clamp_temperature " + clamp_temperature);

    chaleur = if (!clamp_temperature) actual_temperature else chaleur;
    System.out.println("formulas.actual_temperature " + actual_temperature);

    // GUI Temperature.Update(chaleur);
    total_happiness_values += 100.0 - total_unhappiness
    temperature_values += actual_temperature
    chaleur
  }

  def calculate_string_unhappiness(extractor:  WorkspaceObject => Double): Double = {
    val isu = (objects.toList.map(wo => wo.relative_importance * extractor(wo)).sum) / 2
    log.debug("workspace calculate_string_unhappiness isu " + isu)
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
      ((c.description_type1.id.equals(cm.description_type1.id) &&
        (c.description_type2.id.equals(cm.description_type2.id)) &&
        (c.descriptor1.id.equals(cm.descriptor1.id))))
    }).isDefined
  }



  def number_of_bonds(): Int = {
    //returns the number of bonds in the workspace
    structures.filter(ws => ws.isInstanceOf[Bond]).size
  }

  def correspondence_present(c: Correspondence): Boolean = {
    val obj1Correspondence = c.obj1.correspondence
    obj1Correspondence.isDefined && obj1Correspondence.get.obj2 == c.obj2
  }

}

