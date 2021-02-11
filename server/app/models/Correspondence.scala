package models

import models.Bond.BondRep
import models.ConceptMapping.ConceptMappingRep
import models.WorkspaceObject.WorkspaceObjectRep

import scala.collection.mutable.ListBuffer

object Correspondence {
  case class CorrespondenceRep(
                                uuid: String,
                                obj1: WorkspaceObjectRep,
                                obj2: WorkspaceObjectRep,
                                concept_mapping_list: List[ConceptMappingRep]
                              ) {
    override def toString(): String = {
      s"Correspondence between ${obj1} and ${obj2}"
    }
  }

}

case class Correspondence (
                            val obj1: WorkspaceObject,
                            val obj2: WorkspaceObject,
                            var concept_mapping_list : List[ConceptMappingRep],
                            val flip_obj2: Boolean
                          ) extends WorkspaceStructure {
  def addConceptMappings(cCMReps: List[ConceptMappingRep]) = {
    System.out.println("Correspondence. addConceptMapping concept_mapping_list " + concept_mapping_list);
    System.out.println("Correspondence. addConceptMapping " + cCMReps);
    concept_mapping_list = cCMReps ::: concept_mapping_list
  }
  def addAccessoryConceptMappings(cCMReps: List[ConceptMappingRep]) = {
    System.out.println("Correspondence. addAccessoryConceptMappings " + cCMReps);
    accessory_concept_mapping_list =  accessory_concept_mapping_list ++ cCMReps
  }


  import Correspondence.CorrespondenceRep
  var accessory_concept_mapping_list = ListBuffer.empty[ConceptMappingRep]


  override def toString(): String = {
    s"Correspondence between ${obj1} and ${obj2}"
  }
  def correspondenceRep() = CorrespondenceRep(
    uuid,
    obj1.workspaceObjectRep(),
    obj2.workspaceObjectRep(),
    concept_mapping_list
  )

  // This is cumbersome ! a list of concept mapping which is not just references ...
  // Vector concept_mapping_list = new Vector();


//  see ConceptMapping
//  def concept_mapping_present(cm: ConceptMapping): Boolean = {
//     // returns true if a concept mapping of the same sort is
//     // present in the workspace
//     concept_mapping_list.find(c =>
//       ((c.description_type1 == cm.description_type1)&&
//         (c.description_type2 == cm.description_type2)&&
//         (c.descriptor1 == cm.descriptor1)&&
//         (c.descriptor2 == cm.descriptor2))
//     ).isDefined
//   }

//  def slipNodeActivationChanged(slipNodeID: String, newActivation: Double) = {
//    concept_mapping_list = concept_mapping_list.map(cm => {
//      cm.updatedConceptMappingRep(slipNodeID, newActivation)
//    })
//  }

/* see slipnet
  public static boolean supporting_concept_mappings(
         concept_mapping cm1, concept_mapping cm2){
   // Concept-mappings (a -> b) and (c -> d) support each other if a is related
    // to c and if b is related to d and the a -> b relationship is the same as the
    // c -> d relationship.  E.g., rightmost -> rightmost supports right -> right
    // and leftmost -> leftmost.  Notice that slipnet distances are not looked
    // at, only slipnet links.  This should be changed eventually.

    // If the two concept-mappings are the same, then return t.  This
    // means that letter->group supports letter->group, even though these
    // concept-mappings have no label.

    if ((cm1.descriptor1==cm2.descriptor1)&&(cm1.descriptor2==cm2.descriptor2)) return true;
    // if the descriptors are not related return false
    if (!(slipnet_formulas.related(cm1.descriptor1,cm2.descriptor1)||
         slipnet_formulas.related(cm1.descriptor2,cm2.descriptor2))) return false;
   if ((cm1.label==null)||(cm2.label==null)) return false;
   if ((cm1.label).equals(cm2.label)) return true;
   return false;
  }

  public boolean internally_coherent(){
    // returns true if there is any pair of relevant_distinguish
    // cms that support each other
    Vector cm_list = this.relevant_distinguishing_cms();
    for (int x=0; x<cm_list.size(); x++)
    for (int y=0; y<cm_list.size(); y++)
    if (x!=y){
      if (supporting_concept_mappings(
        (concept_mapping)cm_list.elementAt(x),
      (concept_mapping)cm_list.elementAt(y))) return true;
    }
    return false;
  }



  def relevant_distinguishing_cms() = {
    Vector v = new Vector();
    for (int x=0; x<concept_mapping_list.size(); x++){
      concept_mapping cm = (concept_mapping)concept_mapping_list.elementAt(x);
      if ((cm.relevant())&&(cm.distinguishing())) v.addElement(cm);
    }
    return v;
  }

*/

  // partially moved to Workspace.break_correspondence
  def break_correspondence() = {
    obj1.correspondence = None
    obj2.correspondence = None
  }

  // To be completed see below remark
  def build_correspondenceStep1() = {
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




/* see ...
  def incompatible_concept_mappings() : Boolean = (
    concept_mapping cm1, concept_mapping cm2){
    // Concept-mappings (a -> b) and (c -> d) are incompatible if a is
    // related to c or if b is related to d, and the a -> b relationship is
    // different from the c -> d relationship. E.g., rightmost -> leftmost
    // is incompatible with right -> right, since rightmost is linked
    // to right, but the relationships (opposite and identity) are different.
    // Notice that slipnet distances are not looked at, only slipnet links. This
    // should be changed eventually.
    if (!((slipnet_formulas.related(cm1.descriptor1,cm2.descriptor1))||
      (slipnet_formulas.related(cm1.descriptor2,cm2.descriptor2))))
      return false;
    if ((cm1.label==null)||(cm2.label==null)) return false;
    if (!(cm1.label==cm2.label)) return true;
    return false;
  }
*/
  def get_incompatible_bond(): Option[(BondRep,BondRep)] = {
    var bond1 = Option.empty[Bond]
    if (obj1.leftmost) bond1=obj1.right_bond
    if (obj1.rightmost) bond1=obj1.left_bond
    var bond2 = Option.empty[Bond]
    if (obj2.leftmost) bond2=obj2.right_bond;
    if (obj2.rightmost) bond2=obj2.left_bond;
    if ((bond1.isDefined)&&(bond2.isDefined)){
      if ((bond1.get.direction_category!=null)&&
        (bond2.get.direction_category!=null)) {
        Some((bond1.get.bondRep(),bond2.get.bondRep()))
      }
    }
    return None
  }


  def update_strength_value(internal_strength: Double, cs: List[Correspondence], supporting_correspondences:Map[String, Boolean]) = {
    calculate_internal_strength(internal_strength)
    calculate_external_strength(cs, supporting_correspondences)
    calculate_total_strength()
  };

  def calculate_internal_strength(internalStrength: Double) = {
    internal_strength = internalStrength
  }

/* see Slipnet
  def incompatible_correspondences(c1: Correspondence, c2: Correspondence) : Boolean = {
    if (c1.obj1==c2.obj1) return true;
    if (c1.obj2==c2.obj2) return true;
    c1.concept_mapping_list.find(c1cm => {
      c2.concept_mapping_list.find(c2cm => {
        incompatible_concept_mappings(
          (concept_mapping)c1.concept_mapping_list.elementAt(x),
        (concept_mapping)c2.concept_mapping_list.elementAt(y))
      })
    }).isDefined
    for (int x=0; x<c1.concept_mapping_list.size(); x++)
    for (int y=0; y<c2.concept_mapping_list.size(); y++){
      if (incompatible_concept_mappings(
        (concept_mapping)c1.concept_mapping_list.elementAt(x),
      (concept_mapping)c2.concept_mapping_list.elementAt(y))) return true;
    }
    return false;
  }
*/

  /* see workspace
  def get_incompatible_correspondences() = {
    // returns a list of all existing correspondences that are incompatible
    // with this proposed correspondence
    Vector incc = new Vector();
    for (int x=0; x<workspace.initial.objects.size(); x++){
      workspace_object w  = (workspace_object)workspace.initial.objects.elementAt(x);
      if (w.correspondence!=null){
        if (incompatible_correspondences(this,w.correspondence))
          incc.addElement(w.correspondence);
      }
    }

    return incc;
  }
*/


  def slippageCandidates() = {
    concept_mapping_list ::: accessory_concept_mapping_list.toList
  }

  def support(cs: List[Correspondence], supporting_correspondences:Map[String, Boolean]): Double = {
    // For now there are three levels of compatibility:
    // supporting, not incompatible but not supporting, and incompatible.
    // This returns the sum of the strengths of other correspondences that
    // support this one (or 100, whichever is lower).  If one of the objects is the
    // single letter in its string, then the support is 100.
    //System.out.println("support obj1 " + obj1);
    //System.out.println("support obj1.spans_string " + obj1.spans_string);
    //System.out.println("support obj2 " + obj1);
    //System.out.println("support obj2.spans_string " + obj2.spans_string);
    if (obj1.isInstanceOf[Letter] && obj1.spans_string) return 100.0
    if (obj2.isInstanceOf[Letter] && obj2.spans_string) return 100.0
    var support_sum = 0.0;
    for (c <- cs) {
      val supCorrs = supporting_correspondences(c.uuid)
      //System.out.println("support ws!=this " + (c !=this));
      //System.out.println("support supporting_correspondences(this,(Correspondence)ws)) " + supCorrs);

      if ((c != this) && supCorrs)
          support_sum += c.total_strength;
    }
    //System.out.println("support support_sum " + support_sum);

    if (support_sum>100.0) return 100.0;
    else return support_sum;
  }

  def calculate_external_strength(cs: List[Correspondence], supporting_correspondences:Map[String, Boolean]) = {
    //System.out.println("calculate_external_strength " + cs)
    //System.out.println("calculate_external_strength " + supporting_correspondences)
    external_strength = support(cs, supporting_correspondences)
  }

}


