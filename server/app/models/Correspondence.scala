package models

import models.ConceptMapping.ConceptMappingRep

case class Correspondence (
                            val obj1: WorkspaceObject,
                            val obj2: WorkspaceObject,
                            val concept_mapping_list : List[ConceptMappingRep],
                            val flip_obj2: Boolean
                          ) extends WorkspaceStructure {

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

}


