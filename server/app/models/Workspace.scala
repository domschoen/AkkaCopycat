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
import models.ConceptMapping.ConceptMappingRep
import models.SlipNode.SlipNodeRep
import models.Slipnet.{GroupRep, WorkspaceStructureRep}
import models.Workspace.InitializeWorkspaceStringsResponse
import models.codelet.BottomUpBondScout.{GoWithBottomUpBondScout2Response, GoWithBottomUpBondScoutResponse}
import models.codelet.Codelet.Finished
import models.codelet.{Codelet, CodeletType}
import play.api.Configuration
import play.api.libs.concurrent.InjectedActorSupport

import scala.collection.mutable.ListBuffer






object Workspace {
  def props(slipnet: ActorRef, temperature: ActorRef): Props = Props(new Workspace(slipnet, temperature))

  case class Run(executionRun: ActorRef, initialString: String, modifiedString: String, targetString: String)
  case class Initialize(initialS: String, modifiedS: String, targetS: String)
  case class InitializeWorkspaceStringsResponse(
                                                 initialDescriptions: List[WorkspaceStructureRep],
                                                 modifiedDescriptions: List[WorkspaceStructureRep],
                                                 targetDescriptions: List[WorkspaceStructureRep])
  case object Step
  case object Found
  case object ChooseRandomStructure
  case class BondWithNeighbor(temperature: Double)
  case class GoWithBottomUpBondScout2(from: WorkspaceStructureRep, to:WorkspaceStructureRep, fromFacets: List[SlipNodeRep], toFacets: List [SlipNodeRep])
  case class GoWithBottomUpBondScout3(bondFrom: WorkspaceStructureRep,
                                      bondTo: WorkspaceStructureRep,
                                      bondCategory: SlipNodeRep,
                                      bondFacet: SlipNodeRep,
                                      fromDescriptor: SlipNodeRep,
                                      toDescriptor: SlipNodeRep,
                                      bondCategoryDegreeOfAssociation: Double,
                                      slipnetLeft: SlipNodeRep,
                                      slipnetRight: SlipNodeRep
                                     )

  case object GoWithReplacementFinder
  case class GoWithBottomUpCorrespondenceScout(temperature: Double, codelet: ActorRef)
  case class GoWithBottomUpCorrespondenceScout2(
                                                 obj1: WorkspaceStructureRep,
                                                 obj2: WorkspaceStructureRep,
                                                 concept_mapping_list : List[ConceptMappingRep],
                                                 flip_obj2: Boolean,
                                                 distiguishingConceptMappingSize: Int,
                                                 distiguishingConceptMappingTotalStrength: Double,
                                                 temperature: Double
                                               )

  case class GoWithGroupBuilder(temperature: Double, groupID: String)
  case class GoWithDescriptionStrengthTester(temperature: Double, correspondenceID: String)
  case class GoWithBondStrengthTester(temperature: Double, bondID: String)


  case class GoWithBondBuilder(temperature: Double)
}

class Workspace(slipnet: ActorRef, temperature: ActorRef) extends Actor with ActorLogging with InjectedActorSupport {
  import Workspace.{
    Initialize,
    Found,
    GoWithBottomUpCorrespondenceScout,
    GoWithBottomUpCorrespondenceScout2,
    GoWithReplacementFinder,
    GoWithBondBuilder,
    GoWithGroupBuilder,
    GoWithDescriptionStrengthTester,
    GoWithBondStrengthTester,
    BondWithNeighbor,
    GoWithBottomUpBondScout2,
    GoWithBottomUpBondScout3,
    ChooseRandomStructure
  }

  import Slipnet._
  import Coderack._
  import models.codelet.BottomUpCorrespondenceScout.{
    GoWithBottomUpCorrespondenceScoutWorkspaceReponse,
    GoWithBottomUpCorrespondenceScout2Response
  }
  import models.codelet.BottomUpBondScout.{
    GoWithBottomUpBondScout3Response
  }
  import models.codelet.BondStrengthTester.GoWithBondStrengthTesterResponse

  var executionRunActor: ActorRef = null
  var coderack: ActorRef = null

  var found_answer = false;
  var structures = ListBuffer.empty[WorkspaceStructure]
  // initial ---> target
  // modified ----> ?
  var initial: WorkspaceString = null
  var target: WorkspaceString = null
  var modified: WorkspaceString = null
  val r = scala.util.Random
  var changed_object = Option.empty[WorkspaceObject]
  var objectRefs = Map.empty[String, WorkspaceObject]
  var rule = Option.empty[Rule]

  var total_unhappiness = 0.0;
  var intra_string_unhappiness = 0.0;
  var inter_string_unhappiness = 0.0;



  var chaleur = 100.0;
  var actual_temperature = 100.0;
  var total_happiness_values = ListBuffer.empty[Double]
  var temperature_values = ListBuffer.empty[Double]
  var clamp_temperature = false;  // external clamp

  def updateWorkspaceStringWithDescriptionReps(ws: WorkspaceString, wosToUpdate: List[WorkspaceStructureRep]) = {
    val letterRefs = ws.objects.map(_.uuid).zip(ws.objects).toMap
    for(woRep <- wosToUpdate) {
      val letter = letterRefs(woRep.uuid)
      for(d <- woRep.descriptions) {
        letter.add_description(d.descriptionType, d.descriptor)
      }
      build_descriptions(letter)
    }
  }


  def receive = LoggingReceive {

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

    case Found =>
      found_answer = true

    case Step =>
      if (found_answer) {
        executionRunActor ! ExecutionRun.Found
      } else {
        self ! Step
      }
    case ChooseRandomStructure =>
      val candidateStructures = structures.filter(s => s.isInstanceOf[Group] ||
        s.isInstanceOf[Bond] ||
        s.isInstanceOf[Correspondence]
      )
      if (candidateStructures.isEmpty) {
        log.debug("There are no structures built: fizzle")

      }
    // bottom-up-bond-scout in codelet.java.240
    case BondWithNeighbor(temperature) =>
      //          workspace_object fromob = workspace_formulas.choose_object("intra_string_salience",workspace.workspace_objects);
      val fromOpt = chooseObject(TemperatureAjustmentVariable.Intra_string_salience, temperature)
      fromOpt match {
        case None =>
          log.debug("BondWithNeighbor | failed with empty from")
          sender() ! Finished
        case Some(from) =>
          val toOpt = chooseNeighbor(from, temperature)
          toOpt match {
            case None =>
              log.debug("BondWithNeighbor | object has no neighbour - fizzle")
              sender() ! Finished

            case Some(to) =>
              log.debug(s"initial object chosen: $from in ${if (from.workspaceString() == initial) "initial" else "target"} string")
              log.debug(s"ito object: $to")

              sender() ! GoWithBottomUpBondScoutResponse(from.workspaceStructureRep(), to.workspaceStructureRep())
          }
      }
      sender() ! Finished


    case GoWithBottomUpBondScout2(fromRep: WorkspaceStructureRep, toRep: WorkspaceStructureRep, fromFacets: List[SlipNodeRep], toFacets: List [SlipNodeRep]) =>
      // workspace_formulas.java.207
      if (fromFacets.isEmpty) {
        log.debug(s" no possible bond-facet - fizzle")
        sender() ! Finished

      } else {
        val from = objectRefs(fromRep.uuid)
        val to = objectRefs(toRep.uuid)
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
          val fromDescriptor = fromDescriptorOpt.get
          val toDescriptor = toDescriptorOpt.get
          log.debug(s"from object descriptor: ${fromDescriptor.id}")
          log.debug(s"to object descriptor: ${toDescriptor.id}")

          // Here it goes to slipnet
          sender() ! GoWithBottomUpBondScout2Response(bondFacet, fromDescriptor, toDescriptor)
        }
      }
    case GoWithBottomUpBondScout3(bondFromRep, bondToRep, bondCategory, bondFacet, fromDescriptor, toDescriptor, bondCategoryDegreeOfAssociation, slipnetLeft, slipnetRight) =>
      val bondFrom = objectRefs(bondFromRep.uuid)
      val bondTo = objectRefs(bondToRep.uuid)
      val nb = new Bond(bondFrom,bondTo,bondCategory,bondFacet,fromDescriptor,toDescriptor, slipnetLeft, slipnetRight)
      // if (!remove_terraced_scan) workspace.WorkspaceArea.AddObject(nb,1);

      sender ! GoWithBottomUpBondScout3Response(nb.uuid)

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
            sender()  ! Finished

        }
      }
    // codelet.java.1233
    case GoWithBottomUpCorrespondenceScout(
      temperature,
      codelet
    ) =>
      val obj1Opt: Option[WorkspaceObject] = chooseObjectFromList(initial.objects.toList, TemperatureAjustmentVariable.Inter_string_salience)
      obj1Opt match {
        case None =>
          log.debug("GoWithBottomUpCorrespondenceScout | failed with empty obj1")
          codelet ! Finished

        case Some(obj1) =>
          val obj2Opt = chooseObjectFromList(target.objects.toList,TemperatureAjustmentVariable.Inter_string_salience)
          obj2Opt match {
            case None =>
              log.debug("GoWithBottomUpCorrespondenceScout | failed with empty obj2")
              codelet ! Finished

            case Some(obj2) =>
              log.debug(s"GoWithBottomUpCorrespondenceScout | trying a correspondence between $obj1 and $obj2")
              // if one object spans the string and the other doesn't - fizzle
              if (obj1.spans_string != obj2.spans_string) {
                // fizzle
                log.debug("GoWithBottomUpCorrespondenceScout | only one object spans the string: fizzle");
                codelet ! Finished

              } else {
                // He we continue in the slipnet
                // then the slipnet will potentially tell the workspace to create a Correspondence
                // then workspace tell the coderack to post a "correspondence-strength-tester" codelet

                codelet ! GoWithBottomUpCorrespondenceScoutWorkspaceReponse(
                  obj1.workspaceStructureRep(),
                  obj2.workspaceStructureRep(),
                  codelet
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

      val nc = new Correspondence(obj1,obj2,concept_mapping_list,flip_obj2);
      // TODO if (!remove_terraced_scan) WorkspaceArea.AddObject(nc,1);

      sender ! GoWithBottomUpCorrespondenceScout2Response(
        nc.uuid,
        distiguishingConceptMappingSize,
        distiguishingConceptMappingTotalStrength
        )


    case GoWithGroupBuilder(temperature, groupID) =>
      /*val group = objectRefs(groupID).asInstanceOf[Group]
      val st = group.toString()
      val stringType = if (group.workspaceString().equals(initial)) "initial" else "target"
      log.debug(s"trying to build ${group.toString()} in ${stringType} string")
      if (WorkspaceFormulas.group_present(group)){
        print("already exists...activate descriptors & fizzle");
        group.activate_descriptions();
        val wo = WorkspaceFormulas.equivalent_group(group)
        addDescriptionsToWorkspaceObject(wo, group.descriptions)
        return false;

      }*/
      sender() ! Finished

    case GoWithDescriptionStrengthTester(temperature, correspondenceID) =>
      sender() ! Finished

    // codelet.java.395
    case GoWithBondStrengthTester(temperature, bondID) =>
      val b = objectRefs(bondID).asInstanceOf[Bond]
      b.update_strength_value()
      val strength = b.total_strength;
      val workingString = if (b.left_obj.wString == initial) "initial" else "target"
      log.info(s"bond = ${bondID} in ${workingString} string")

      val prob = WorkspaceFormulas.temperature_adjusted_probability(strength/100.0, temperature)
      log.info("bond strength = "+strength)
      if (!WorkspaceFormulas.flip_coin(prob)){
        print("not strong enough: Fizzle!");
        sender() ! Finished
      }
      // it is strong enough - post builder  & activate nodes
      slipnet ! SetSlipNodeBufferValue(b.bond_facet.id, 100.0)
      slipnet ! SetSlipNodeBufferValue(b.from_obj_descriptor.id, 100.0)
      slipnet ! SetSlipNodeBufferValue(b.to_obj_descriptor.id, 100.0)

      log.info("succeeded: will post bond-builder");
      sender() ! GoWithBondStrengthTesterResponse(strength)





    case GoWithBondBuilder(temperature) =>
      sender() ! Finished

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
      objectRefs += (l.uuid -> l)
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
    for (description <- wo.descriptions) {
      slipnet ! SetSlipNodeBufferValue(description.descriptionType.id, 100.0)
      if (description.descriptor.isEmpty) {
        log.info("oups d.descriptor is null");
      } else {
        slipnet ! SetSlipNodeBufferValue(description.descriptor.get.id, 100.0)
        if (!structures.contains(description)) {
          // GUI area.AddObject(d);
          structures += description
        }
      }
    }
    // GUI check_visibility()
  }

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

  def build_rule(r: Rule){
    // GUI workspace.Workspace_Rule.Change_Caption("rule : " +this.toString());
    if (rule.isDefined) { structures -= rule.asInstanceOf[WorkspaceStructure] }
    rule= Some(r);
    structures += r
    r.activate_rule_descriptions()
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

  def chooseObject(variable: String, temperature: Double) : Option[WorkspaceObject] = {
    val nonModifieds = workspaceObjects().filter(wo =>
      {
        println("chooseObject " + wo.workspaceString + " modified " + modified)
        wo.workspaceString != modified
      })
    chooseObjectFromList(nonModifieds, variable)
  }
  def chooseNeighbor(from: WorkspaceObject, temperature: Double) : Option[WorkspaceObject] = {
    val nonModifieds = workspaceObjects().filter(wo => (
      wo.workspaceString() == from.workspaceString()) &&
      (
        (wo.left_string_position == from.right_string_position + 1) ||
        (from.left_string_position== wo.right_string_position+1)
      )
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


  def temperatureAdjustedValue(value: Double, temperature: Double) = Math.pow(value,((100.0-temperature)/30.0)+0.5)


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

  /* slip !
  def slippage_list(): List[ConceptMapping] = {
    Vector sl = new Vector();
    if ((changed_object!=null)&&(changed_object.correspondence!=null)){
      Correspondence c = changed_object.correspondence;

      val slip_list =
      Vector slip_list = c.concept_mapping_list;
      for (int y=0; y<slip_list.size(); y++){
        concept_mapping cm = (concept_mapping)slip_list.elementAt(y);
        sl.addElement(cm);
      }
    }
    for (int x=0; x<workspace.initial.objects.size(); x++){
      workspace_object wo =(workspace_object)workspace.initial.objects.elementAt(x);
      if (wo.correspondence!=null){
        Correspondence c = wo.correspondence;
        Vector slip_list = c.slippage_list();
        for (int y=0; y<slip_list.size(); y++){
          concept_mapping cm = (concept_mapping)slip_list.elementAt(y);
          if (!(cm.in_vector(sl))) sl.addElement(cm);
        }
      }
    }
    return sl;
  }*/

  def number_of_bonds(): Int = {
    //returns the number of bonds in the workspace
    structures.filter(ws => ws.isInstanceOf[Bond]).size
  }

  def correspondence_present(c: Correspondence): Boolean = {
    val obj1Correspondence = c.obj1.correspondence
    obj1Correspondence.isDefined && obj1Correspondence.get.obj2 ==c.obj2
  }

}

