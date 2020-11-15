package models

import java.util.UUID

import models.SlipNode.SlipnetInfo
import models.Slipnet.{InflatedDescriptionRep, WorkspaceStructureRep}

// It is more a object living in Slipnet
// w1 and w2 are going to be references, just an ID that could be then used in the workspace

// A description has description type = slipnode
object ConceptMapping {
  case class ConceptMappingRep(description_type1SlipNodeID: String,
                          description_type2SlipNodeID: String,
                          descriptor1SlipNodeID: String,
                          descriptor2SlipNodeID: String,
                          obj1: WorkspaceStructureRep,
                          obj2: WorkspaceStructureRep
                         )

  def get_concept_mapping_list(
                                w1 : WorkspaceStructureRep,
                                w2 : WorkspaceStructureRep,
                                ds1: List[InflatedDescriptionRep],
                                ds2: List[InflatedDescriptionRep],
                                slipnetInfo: SlipnetInfo
                              ) = {
    for (
      d1 <- ds1;
      d2 <- ds2 if
      d1.descriptionTypeSlipNode.equals(d2.descriptionTypeSlipNode) &&
        (
          d1.descriptorSlipNode.equals(d2.descriptorSlipNode) ||
            (
              d1.descriptorSlipNode.isDefined &&
                d2.descriptorSlipNode.isDefined &&
                SlipnetFormulas.slip_linked(d1.descriptorSlipNode.get,d2.descriptorSlipNode.get)
              )
        )
    ) yield new ConceptMapping(
      d1.descriptionTypeSlipNode,
      d2.descriptionTypeSlipNode,
      d1.descriptorSlipNode.get,
      d2.descriptorSlipNode.get,
      w1,
      w2,
      slipnetInfo
    )
  }

  def all_opposite_mappings(cms: List[ConceptMapping], opposite : SlipNode): Boolean = {
    // returns true if all mappings are opposite
    !cms.find(cm => cm.label != opposite).isDefined
  }

}


// Not so good because a correspondence (wo) has a list of concept mapping
class ConceptMapping(val description_type1: SlipNode,
                     val description_type2: SlipNode,
                     val descriptor1: SlipNode,
                     val descriptor2: SlipNode,
                     val obj1: WorkspaceStructureRep,
                     val obj2: WorkspaceStructureRep,
                     slipnetInfo: SlipnetInfo
                    ) {
  import ConceptMapping.ConceptMappingRep

  val uuid = generateID()
  def generateID(): String = UUID.randomUUID().toString()

  var label: Option[SlipNode] = SlipnetFormulas.get_bond_category(descriptor1,descriptor2, slipnetInfo.slipnetOpposite)  // if the concept_mapping has a linking concept

  def conceptMappingRep(): ConceptMappingRep = ConceptMappingRep(
      description_type1.id(),
      description_type2.id(),
      descriptor1.id(),
      descriptor2.id(),
      obj1,
      obj2
    )


  override def toString(): String = descriptor1.name + " -> " + descriptor2.name


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

  def distinguishing(): Boolean = {
    if ((descriptor1== slipnetInfo.slipnetWhole)&&(descriptor2== slipnetInfo.slipnetWhole)) {
      false
    } else {
      (distinguishing_descriptor(obj1, descriptor1)) &&
        (distinguishing_descriptor(obj2, descriptor2))
    }
  }

  def distinguishing_descriptor(obj: WorkspaceStructureRep, descriptor: SlipNode): Boolean = {
    if (descriptor== slipnetInfo.slipnetLetter) return false;
    if (descriptor== slipnetInfo.slipnetGroup) return false;

    if (slipnetInfo.slipnet_numbers.find(sn => sn == descriptor).isDefined) {
      false
    } else {
      !obj.letterOrGroupCompanionReps.find(lgc => lgc.descriptions.find(d => d.descriptorSlipNodeID.equals(descriptor.id)).isDefined).isDefined
    }
  }


  def concept_mapping_symmetric_version(): ConceptMapping ={
    if ((label == slipnetInfo.slipnetIdentity) || (label == slipnetInfo.slipnetSameness))
      return this;
    if (!(SlipnetFormulas.get_bond_category(descriptor2,descriptor1,slipnetInfo.slipnetIdentity) == label))
      return this;
    new ConceptMapping(description_type2,description_type1,
      descriptor2,descriptor1,obj1,obj2,slipnetInfo)
  }
}