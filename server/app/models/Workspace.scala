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
import models.codelet.BottomUpDescriptionScout.{GoWithBottomUpDescriptionScoutResponse, PrepareDescriptionResponse}
import models.codelet.Codelet.Finished
import models.codelet.DescriptionStrengthTester.GoWithDescriptionStrengthTesterResponse
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
  case class GoWithBreaker(temperature: Double)
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

  case class GoWithBottomUpDescriptionScout(temperature: Double)
  case class PrepareDescription(chosen_object: WorkspaceStructureRep,
                                chosen_propertyRep: SlipNodeRep,
                                description_typeRep: SlipNodeRep)
  case class GoWithBottomUpCorrespondenceScout(temperature: Double)
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
  case class GoWithDescriptionStrengthTester(temperature: Double, descriptionID: String)
  case class GoWithBondStrengthTester(temperature: Double, bondID: String)


  case class GoWithBondBuilder(temperature: Double, bondID: String)
}

class Workspace(slipnet: ActorRef, temperature: ActorRef) extends Actor with ActorLogging with InjectedActorSupport {
  import Workspace.{
    Initialize,
    Found,
    GoWithBottomUpCorrespondenceScout,
    GoWithBottomUpCorrespondenceScout2,
    GoWithBottomUpDescriptionScout,
    PrepareDescription,
    GoWithReplacementFinder,
    GoWithBondBuilder,
    GoWithGroupBuilder,
    GoWithDescriptionStrengthTester,
    GoWithBondStrengthTester,
    BondWithNeighbor,
    GoWithBottomUpBondScout2,
    GoWithBottomUpBondScout3,
    GoWithBreaker
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

    case Found =>
      found_answer = true

    case Step =>
      if (found_answer) {
        executionRunActor ! ExecutionRun.Found
      } else {
        self ! Step
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
        val p = (r.nextDouble()*wsize).toInt
        val pos =  if (p >= wsize) 0 else p
        val ws = structures(pos);

        val probability = 1.0 - WorkspaceFormulas.temperature_adjusted_probability(ws.total_strength/100.0, t)
        val st = if (ws.isInstanceOf[WorkspaceObject]) {
          val wo = ws.asInstanceOf[WorkspaceObject]
          if (wo.workspaceString().equals(initial)) " from initial string" else " from target string"
        } else ""
        log.debug(s"object chosen = ${ws}${st} break probability = ${probability}")


        val grOpt: Option[Group] = if (ws.isInstanceOf[Bond]){
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
          val fromDescriptor = fromDescriptorOpt.get
          val toDescriptor = toDescriptorOpt.get
          log.debug(s"from object descriptor: ${fromDescriptor.id}")
          log.debug(s"to object descriptor: ${toDescriptor.id}")

          // Here it goes to slipnet
          sender() ! GoWithBottomUpBondScout2Response(bondFacet, fromDescriptor, toDescriptor)
        }
      }
    case GoWithBottomUpBondScout3(bondFromRep, bondToRep, bondCategory, bondFacet, fromDescriptor, toDescriptor, bondCategoryDegreeOfAssociation, slipnetLeft, slipnetRight) =>
      val bondFrom = objectRefs()(bondFromRep.uuid)
      val bondTo = objectRefs()(bondToRep.uuid)
      val nb = new Bond(bondFrom,bondTo,bondCategory,bondFacet,fromDescriptor,toDescriptor, slipnetLeft, slipnetRight, slipnet)
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
                  log.debug("chosen descriptor = "+ chosen_descriptor.id);
                  sender() ! GoWithBottomUpDescriptionScoutResponse(chosen_object.workspaceStructureRep(), chosen_descriptor)

                case None =>
                  log.debug(s"GoWithBottomUpDescriptionScout | Oups choosen description is not defined")
                  sender() ! Finished
              }

            case None =>
              log.debug("no relevant descriptions: Fizzle");
              sender()  ! Finished
          }
      }
    case PrepareDescription(chosen_object, chosen_propertyRep, description_typeRep) =>
      val ob = objectRefs()(chosen_object.uuid)

      val descriptor = chosen_propertyRep
      val d = new models.Description(ob,description_typeRep, Some(descriptor))
      slipnet ! SetSlipNodeBufferValue(descriptor.id, 100.0)
      val urgency = description_typeRep.activation
      sender() ! PrepareDescriptionResponse(d.uuid, urgency)


    // codelet.java.1233
    case GoWithBottomUpCorrespondenceScout(t) =>
      val obj1Opt: Option[WorkspaceObject] = chooseObjectFromList(initial.objects.toList, TemperatureAjustmentVariable.Inter_string_salience)
      obj1Opt match {
        case None =>
          log.debug("GoWithBottomUpCorrespondenceScout | failed with empty obj1")
          sender() ! Finished

        case Some(obj1) =>
          val obj2Opt = chooseObjectFromList(target.objects.toList,TemperatureAjustmentVariable.Inter_string_salience)
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
                  obj1.workspaceStructureRep(),
                  obj2.workspaceStructureRep()
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

      val nc = new Correspondence(obj1,obj2,concept_mapping_list,flip_obj2);
      // TODO if (!remove_terraced_scan) WorkspaceArea.AddObject(nc,1);

      sender ! GoWithBottomUpCorrespondenceScout2Response(
        nc.uuid,
        distiguishingConceptMappingSize,
        distiguishingConceptMappingTotalStrength
        )


    // codelet.java.880
    case GoWithGroupBuilder(temperature, groupID) =>
      val group = objectRefs()(groupID).asInstanceOf[Group]
      logTrying(group, group)

      if (WorkspaceFormulas.group_present(group)){
        print("already exists...activate descriptors & fizzle");
        group.activate_descriptions();
        val woOpt = WorkspaceFormulas.equivalent_group(group)
        woOpt match {
          case Some(wo) => addDescriptionsToWorkspaceObject(wo, group.descriptions.toList)
          case None =>
            log.debug("GoWithGroupBuilder stop with no equivalent group")
        }
      }


      // TODO to be completed !!


      sender() ! Finished


      // Codelet.java.425
    case GoWithBondBuilder(temperature, bondID) =>
      val b = objectRefs()(bondID).asInstanceOf[Bond]
      logTrying(b, b.left_obj)

      b.update_strength_value();
      print("strength = "+b.total_strength);
      //val competitors = b.get_incompatible_bonds();

      if ((workspaceObjects().contains(b.from_obj))&&
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
            if (fight_it_out(b,1.0,incb,1.0)) {
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
                val incg = WorkspaceFormulas.get_common_groups(b.from_obj,b.to_obj);
                if (incg.isEmpty) {
                  log.debug("no incompatible groups!")
                }
                val failedFight = if (incg.isEmpty) false else {
                  log.debug("trying to break incompatible groups")
                  // try to break all incompatible groups
                  if (fight_it_out(b,1.0,incg,1.0)){
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

      val prob = WorkspaceFormulas.temperature_adjusted_probability(strength/100.0, temperature)
      log.debug("GoWithDescriptionStrengthTester description strength = " + strength);

      if (!WorkspaceFormulas.flip_coin(prob)){
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

      val prob = WorkspaceFormulas.temperature_adjusted_probability(strength/100.0, temperature)
      log.info("bond strength = "+strength)
      if (!WorkspaceFormulas.flip_coin(prob)){
        log.debug("not strong enough: Fizzle!");
        sender() ! Finished
      }
      // it is strong enough - post builder  & activate nodes
      slipnet ! SetSlipNodeBufferValue(b.bond_facet.id, 100.0)
      slipnet ! SetSlipNodeBufferValue(b.from_obj_descriptor.id, 100.0)
      slipnet ! SetSlipNodeBufferValue(b.to_obj_descriptor.id, 100.0)

      log.info("succeeded: will post bond-builder");
      sender() ! GoWithBondStrengthTesterResponse(strength)



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
    //log.debug("build_descriptions " + wo)
    for (description <- wo.descriptions) {
      slipnet ! SetSlipNodeBufferValue(description.descriptionType.id, 100.0)
      if (description.descriptor.isEmpty) {
        log.info("oups d.descriptor is null");
      } else {
        slipnet ! SetSlipNodeBufferValue(description.descriptor.get.id, 100.0)
        if (!structures.contains(description)) {
          // GUI area.AddObject(d);
          //log.debug("add description " + description)

          addStructure(description)
        }
      }
    }
    // GUI check_visibility()
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

  def build_rule(r: Rule){
    // GUI workspace.Workspace_Rule.Change_Caption("rule : " +this.toString());
    if (rule.isDefined) {
      removeStructure(rule.get)
    }
    rule= Some(r)
    addStructure(r)
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
    //log.debug("workspaceObjects() " + workspaceObjects())
    val nonModifieds = workspaceObjects().filter(wo =>
      {
        //println("chooseObject " + wo.workspaceString + " modified " + modified)
        wo.workspaceString != modified
      })
    //log.debug("nonModifieds " + nonModifieds)

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


  def structure_vs_structure(s1: WorkspaceStructure,
                             w1: Double,
                             s2: WorkspaceStructure,
                             w2: Double): Boolean = {
    s1.update_strength_value();
    s2.update_strength_value();
    val vs1 = s1.total_strength*w1;
    val vs2 = s2.total_strength*w2;
    val v1 = temperatureAdjustedValue(vs1, chaleur)
    val v2 = temperatureAdjustedValue(vs2, chaleur)
    !(((v1+v2) * r.nextDouble())>v1)
  }

  def fight_it_out(wo: WorkspaceStructure, v1: Double, structs: List[WorkspaceStructure], v2: Double): Boolean = {
    if (structs.isEmpty) {
      true
    } else {
      !structs.find(ws => (!structure_vs_structure(wo,v1,ws,v2))).isDefined
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

