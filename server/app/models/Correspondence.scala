package models

import models.ConceptMapping.ConceptMappingRep

import scala.collection.mutable.ListBuffer

case class Correspondence (
                            val obj1: WorkspaceObject,
                            val obj2: WorkspaceObject,
                            var concept_mapping_list : List[ConceptMappingRep],
                            val flip_obj2: Boolean
                          ) extends WorkspaceStructure {

  var accessory_concept_mapping_list = ListBuffer.empty[ConceptMappingRep]


  // This is cumbersome ! a list of concept mapping which is not just references ...
  // Vector concept_mapping_list = new Vector();



  /* def concept_mapping_present(cm: ConceptMapping): Boolean = {
     // returns true if a concept mapping of the same sort is
     // present in the workspace
     concept_mapping_list.find(c =>
       ((c.description_type1==cm.description_type1)&&
         (c.description_type2==cm.description_type2)&&
         (c.descriptor1==cm.descriptor1)&&
         (c.descriptor2==cm.descriptor2))
     ).isDefined
   }*/

//  def slipNodeActivationChanged(slipNodeID: String, newActivation: Double) = {
//    concept_mapping_list = concept_mapping_list.map(cm => {
//      cm.updatedConceptMappingRep(slipNodeID, newActivation)
//    })
//  }


  // partially moved to Workspace.break_correspondence
  def break_correspondence() = {
    obj1.correspondence = None
    obj2.correspondence = None
  }

  // To be completed see below remark
  def build_correspondence() = {
    if (obj1.correspondence.isDefined) obj1.correspondence.get.break_correspondence()
    if (obj2.correspondence.isDefined) obj2.correspondence.get.break_correspondence()
    obj1.correspondence = Some(this)
    obj2.correspondence = Some(this)

    // add mappings to accessory-concept-mapping-list


    // Needs to call slipnet at this point
    // To be done in Correspondence-builder
//    Vector v = relevant_distinguishing_cms();
//    for (int x=0; x<v.size(); x++){
//      concept_mapping cm = (concept_mapping) v.elementAt(x);
//      if (cm.slippage())
//        accessory_concept_mapping_list.addElement(cm.symmetric_version());
//    }
//    if ((obj1 instanceof group)&&(obj2 instanceof group)){
//      Vector cmv = concept_mapping.get_concept_mapping_list(obj1,obj2,
//        obj1.bond_descriptions,obj2.bond_descriptions);
//      for (int x=0; x<cmv.size(); x++){
//        concept_mapping cm = (concept_mapping) cmv.elementAt(x);
//        accessory_concept_mapping_list.addElement(cm);
//        if (cm.slippage())
//          accessory_concept_mapping_list.addElement(cm.symmetric_version());
//      }
//    }
//
//
  }
//
//  def relevant_distinguishing_cms() = {
//    concept_mapping_list.filter(cm => cm.relevant() && cm.distinguishing())
//  }

  def slippageCandidates() = {
    concept_mapping_list ::: accessory_concept_mapping_list.toList
  }

}


