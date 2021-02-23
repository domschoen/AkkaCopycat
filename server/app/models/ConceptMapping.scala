package models

import akka.event.LoggingAdapter

import java.util.UUID
import models.SlipNode.{SlipNodeRep, SlipnetInfo}
import models.Slipnet.InflatedDescriptionRep
import models.WorkspaceObject.WorkspaceObjectRep
import models.Description.DescriptionRep

import java.io.{PrintWriter, StringWriter}

// It is more a object living in Slipnet
// w1 and w2 are going to be references, just an ID that could be then used in the workspace

// A description has description type = slipnode
object ConceptMapping {

  case class ConceptMappingParameters(
                                       w1 : WorkspaceObjectRep,
                                       w2 : WorkspaceObjectRep,
                                       ds1: List[DescriptionRep],
                                       ds2: List[DescriptionRep]
                                     )

  case class ConceptMappingRep2(
                                 description_type1: SlipNode,
                                 description_type2: SlipNode,
                                 descriptor1: SlipNode,
                                 descriptor2: SlipNode,
                                 label: Option[SlipNode]
  )
  case class ConceptMappingRep(
                                uuid: String,
//                                description_type1SlipNodeID: String,
//                               description_type2SlipNodeID: String,
//                               descriptor1SlipNodeID: String,
//                               descriptor2SlipNodeID: String,
                               description_type1: SlipNodeRep,
                               description_type2: SlipNodeRep,
                               descriptor1: SlipNodeRep,
                               descriptor2: SlipNodeRep,
                               obj1: WorkspaceObjectRep,
                               obj2: WorkspaceObjectRep
                         ) {

    //    def updatedConceptMappingRepForDescriptionType1(slipNodeID: String, newActivation: Double) =
//      if (description_type1.id == slipNodeID) this.copy(description_type1.copy(activation = newActivation)) else this
//
//    def updatedConceptMappingRepForDescriptionType2(slipNodeID: String, newActivation: Double) =
//      if (description_type2.id == slipNodeID) this.copy(description_type2.copy(activation = newActivation)) else this
//
//    def updatedConceptMappingRepForDescriptor1(slipNodeID: String, newActivation: Double) =
//      if (descriptor1.id == slipNodeID) this.copy(descriptor1.copy(activation = newActivation)) else this
//
//    def updatedConceptMappingRepForDescriptor2(slipNodeID: String, newActivation: Double) =
//      if (descriptor2.id == slipNodeID) this.copy(descriptor2.copy(activation = newActivation)) else this
//
//    def updatedConceptMappingRep(slipNodeID: String, newActivation: Double) = {
//      updatedConceptMappingRepForDescriptionType1(slipNodeID, newActivation)
//        .updatedConceptMappingRepForDescriptionType2(slipNodeID, newActivation)
//        .updatedConceptMappingRepForDescriptor1(slipNodeID, newActivation)
//        .updatedConceptMappingRepForDescriptor2(slipNodeID, newActivation)
//    }

//    // Duplicated code (also defined in ConceptMapping)
//    def relevant(): Boolean ={
//      description_type1.activation == 100.0 && description_type2.activation == 100.0
//    }
     override def toString(): String = descriptor1.id + " -> " + descriptor2.id

  }
  var conceptMappingRefs = Map.empty[String, ConceptMapping]

  def conceptMappingsWithReps(conceptMappingReps: List[ConceptMappingRep]) =
    conceptMappingReps.map(cm => ConceptMapping.conceptMappingRefs(cm.uuid))


  def get_concept_mapping_list(
                                w1 : WorkspaceObjectRep,
                                w2 : WorkspaceObjectRep,
                                ds1: List[InflatedDescriptionRep],
                                ds2: List[InflatedDescriptionRep],
                                slipnetInfo: SlipnetInfo
                              ) = {
    val couples = for (
      d1 <- ds1;
      d2 <- ds2) yield (d1, d2)

    val goodCouples = couples.filter(c => {
      val d1 = c._1
      val d2 = c._2
//      println(s"get_concept_mapping_list ${d1.descriptorSlipNode.map(_.id())} -> ${d2.descriptorSlipNode.map(_.id())}")
      d1.descriptionTypeSlipNode.equals(d2.descriptionTypeSlipNode) &&
        (
          d1.descriptorSlipNode.equals(d2.descriptorSlipNode) ||
            (
              d1.descriptorSlipNode.isDefined &&
                d2.descriptorSlipNode.isDefined &&
                SlipnetFormulas.slip_linked(d1.descriptorSlipNode.get,d2.descriptorSlipNode.get)
              )
          )
    })
    goodCouples.map(c => {
      val d1 = c._1
      val d2 = c._2
      val cm = new ConceptMapping(
        d1.descriptionTypeSlipNode,
        d2.descriptionTypeSlipNode,
        d1.descriptorSlipNode.get,
        d2.descriptorSlipNode.get,
        w1,
        w2,
        slipnetInfo
      )
      addNewConceptMapping(cm)
    })
  }

  def all_opposite_mappings(cms: List[ConceptMapping], opposite : SlipNode): Boolean = {
    // returns true if all mappings are opposite
    !cms.find(cm => cm.label.isDefined && cm.label.get != opposite).isDefined
  }
  def addNewConceptMapping(cm: ConceptMapping): ConceptMapping  = {
    conceptMappingRefs += (cm.uuid -> cm)
    cm
  }


}


// Not so good because a correspondence (wo) has a list of concept mapping
class ConceptMapping(val description_type1: SlipNode,
                     val description_type2: SlipNode,
                     val descriptor1: SlipNode,
                     val descriptor2: SlipNode,
                     val obj1: WorkspaceObjectRep,
                     val obj2: WorkspaceObjectRep,
                     slipnetInfo: SlipnetInfo
                    ) {
  import ConceptMapping.{
    ConceptMappingRep,
    ConceptMappingRep2
  }

  val uuid = generateID()

  if (descriptor1 == null || descriptor2 == null) {
    try {
      throw new Exception("toto")
    } catch {
      case e: Exception =>
        val sw = new StringWriter()
        e.printStackTrace(new PrintWriter(sw))
        println("ConceptMapping initialization exception " + descriptor1 + descriptor2 + " stack: " + sw.toString)
    }
  }





  def generateID(): String = UUID.randomUUID().toString()

  var label: Option[SlipNode] = SlipnetFormulas.get_bond_category(descriptor1,descriptor2, slipnetInfo.slipnetIdentity)  // if the concept_mapping has a linking concept

  def conceptMappingRep(): ConceptMappingRep = ConceptMappingRep(
      uuid,
      description_type1.slipNodeRep(),
      description_type2.slipNodeRep(),
      descriptor1.slipNodeRep(),
      descriptor2.slipNodeRep(),
      obj1,
      obj2
    )
  def conceptMappingRep2(): ConceptMappingRep2 = ConceptMappingRep2(
    description_type1,
    description_type2,
    descriptor1,
    descriptor2,
    label
  )


  override def toString(): String = {
    descriptor1.name + " -> " + descriptor2.name + " label:"+ label.map(_.id()) + " label activation: " + (if (label.isDefined) label.get.activation else "None")
  }


  def in_vector(cms: List[ConceptMapping]): Boolean = {
    // returns true in the concept mapping is in the vector
    cms.find(cm =>
      (cm.description_type1 == description_type1) &&
        (cm.description_type2 == description_type2) &&
        (cm.descriptor1 == descriptor1)).isDefined
  }

  def slipability(): Double = {
    val d_o_a = this.degree_of_association();
    if (d_o_a==100.0) {
      100.0
    } else {
      val v = (conceptual_depth()/100.0);
      d_o_a*(1-(v*v))
    }
  }

  def strength(): Double = {
    if (degree_of_association()==100.0) {
      100.0
    } else {
      val cd= conceptual_depth()/100.0
      degree_of_association()*(1+(cd*cd))
    }
  }

  def slippage(): Boolean = {
    //if (label==null) return false;
    label match {
      case x if x == slipnetInfo.slipnetSameness => false
      case x if x == slipnetInfo.slipnetIdentity => false
      case _ => true
    }
  }
  def relevant(): Boolean ={
    return (((description_type1.activation)==100.0)&&
      ((description_type2.activation)==100.0));
  }
  def conceptual_depth(): Double = {
    (descriptor1.conceptual_depth+descriptor2.conceptual_depth)/2.0
  }

  def degree_of_association(): Double = {
    // assumes the 2 descriptors are connected in the
    // slipnet by at most 1 link
    if (descriptor1==descriptor2)  {
      100.0
    } else {
      val found = descriptor1.lateral_slip_links.find(l => l.to_node==descriptor2)
      found match {
        case Some(l) => l.degree_of_association()
        case None => 0.0
      }
    }
  }

  def distinguishing(log: LoggingAdapter): Boolean = {
    if ((descriptor1== slipnetInfo.slipnetWhole)&&(descriptor2== slipnetInfo.slipnetWhole)) {
      log.debug("distinguishing whole " + this)
      false
    } else {
      val distinguishing_descriptor1 = distinguishing_descriptor(log, obj1, descriptor1)
      val distinguishing_descriptor2 = distinguishing_descriptor(log, obj2, descriptor2)
      log.debug("distinguishing else distinguishing_descriptor1 " + distinguishing_descriptor1 + " distinguishing_descriptor2 "+  distinguishing_descriptor2 + " " + this)

      distinguishing_descriptor1 && distinguishing_descriptor2
    }
  }

  def distinguishing_descriptor(log: LoggingAdapter, obj: WorkspaceObjectRep, descriptor: SlipNode): Boolean = {
//    log.debug("distinguishing_descriptor " + descriptor);

    if (descriptor== slipnetInfo.slipnetLetter) return false;
    if (descriptor== slipnetInfo.slipnetGroup) return false;

    if (slipnetInfo.slipnet_numbers.find(sn => sn == descriptor).isDefined) {
      false
    } else {
//      log.debug("distinguishing_descriptor obs size " + obj.letterOrGroupCompanionReps.size);

      !obj.letterOrGroupCompanionReps.find(lgc => {
//        log.debug("distinguishing_descriptor wo: " + lgc);

        lgc.descriptions.find(d => {
//          log.debug("distinguishing_descriptor d descriptor: " + d.descriptor);
          d.descriptor.isDefined &&
            d.descriptor.get.id.equals(descriptor.id)
        }).isDefined
      }).isDefined
    }
  }


  def symmetric_version(): ConceptMapping ={
    if ((label == slipnetInfo.slipnetIdentity) || (label == slipnetInfo.slipnetSameness))
      return this;
    if (!(SlipnetFormulas.get_bond_category(descriptor2,descriptor1,slipnetInfo.slipnetIdentity) == label))
      return this;
    val cm = new ConceptMapping(description_type2,description_type1,
      descriptor2,descriptor1,obj1,obj2,slipnetInfo)
    ConceptMapping.addNewConceptMapping(cm)
  }

  def concept_mapping_present(concept_mapping_list: List[ConceptMapping]): Boolean = {
    // returns true if a concept mapping of the same sort is
    // present in the workspace
    concept_mapping_list.find(c =>
      ((c.description_type1 == description_type1)&&
        (c.description_type2 == description_type2)&&
        (c.descriptor1 == descriptor1)&&
        (c.descriptor2 == descriptor2))
    ).isDefined
  }

}