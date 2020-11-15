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
import models.Slipnet.{BondRep, DescriptionRep, GroupRep, WorkspaceStructureRep}
import models.codelet.Codelet.Finished
import models.codelet.{Codelet, CodeletType}
import play.api.Configuration
import play.api.libs.concurrent.InjectedActorSupport

import scala.collection.mutable.ListBuffer




class WorkspaceString (val s: String, x1: Int, y1: Int, x2: Int, y2: Int) {
  def length() = s.length
  // Graphics var ratio = 100.0;  // every letter is 100 long unless >(x2-x1)/len

  var objects: ListBuffer[WorkspaceObject] = (for (i <- 0 to s.length -1) yield {
    new Letter(this, i+1, i+1).asInstanceOf[WorkspaceObject]
  }).to[ListBuffer]
}


object Workspace {
  def props(slipnet: ActorRef, temperature: ActorRef): Props = Props(new Workspace(slipnet, temperature))

  case class Run(executionRun: ActorRef, initialString: String, modifiedString: String, targetString: String)
  case class Initialize(coderack: ActorRef)
  case object Step
  case object Found
  case object ChooseRandomStructure
  case class BondWithNeighbor(temperature: Double, codelet: ActorRef)
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
  case class GoWithDescriptionStrengthTester(temperature: Double, correspondenceID: String, codelet: ActorRef)
  case class GoWithBondBuilder(temperature: Double, codelet: ActorRef)
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
    //BondWithNeighbor,
    ChooseRandomStructure
  }

  import Slipnet._
  import Coderack._
  import models.codelet.BottomUpCorrespondenceScout.{ GoWithBottomUpCorrespondenceScoutWorkspaceReponse, GoWithBottomUpCorrespondenceScout2Response }

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

  def receive = LoggingReceive {

    case Initialize(cr) =>
      coderack = cr

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
    /*case BondWithNeighbor(temperature, codelet) =>
      //          workspace_object fromob = workspace_formulas.choose_object("intra_string_salience",workspace.workspace_objects);
      val fromOpt = chooseObject(TemperatureAjustmentVariable.Intra_string_salience, temperature)
      fromOpt match {
        case None =>
          log.debug("BondWithNeighbor | failed with empty from")
          codelet ! Finished
        case Some(from) =>
          val toOpt = chooseNeighbor(from, temperature)
          toOpt match {
            case None =>
              log.debug("BondWithNeighbor | object has no neighbour - fizzle")
              codelet ! Finished

            case Some(to) =>
              log.debug(s"initial object chosen: $from in ${if (from.workspaceString() == initial) "initial" else "target"} string")
              log.debug(s"ito object: $to")

              slipnet ! BondFromTo(from, to, codelet)
          }
      }*/
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

    case GoWithDescriptionStrengthTester(temperature, correspondenceID, codelet) =>
      codelet ! Finished

    case GoWithBondBuilder(temperature, codelet) =>
      codelet ! Finished

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
  def lettersOf(ws: WorkspaceString): List[Letter] = ws.objects.filter(s => s.isInstanceOf[Letter]).asInstanceOf[List[Letter]]


  def workspaceObjects(): List[WorkspaceObject] = structures.filter(s => s.isInstanceOf[WorkspaceObject]).asInstanceOf[List[WorkspaceObject]]

  def chooseObject(variable: String, temperature: Double) : Option[WorkspaceObject] = {
    val nonModifieds = workspaceObjects().filter(wo => wo.workspaceString != modified)
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

