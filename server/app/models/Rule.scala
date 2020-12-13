package models

import akka.actor.ActorRef
import models.ConceptMapping.ConceptMappingRep
import models.SlipNode.SlipNodeRep
import models.Slipnet.SetSlipNodeBufferValue

object Rule {

    def apply_slippages(sn: Option[String], slippages: List[ConceptMappingRep]): Option[String] = {
      val found = slippages.find(cm => sn.isDefined && sn.get == cm.descriptor1SlipNodeID)
      found match {
        case Some(cm) => Some(cm.descriptor2SlipNodeID)
        case None => sn
      }
    }
}

/*case class SlipNodeRep(id: String, descriptorFacetSlipNodeID: String,
                        descriptorSlipNodeID: String,
                        objectCategorySlipNodeID: String,
                        relationSlipNodeID: String)*/
case class Rule (
                  var descriptorFacet: Option[String],
                  var descriptor: Option[String],
                  var objectCategory: Option[String],
                  var relation: Option[String],
                  slipnet: ActorRef,
                  lengthSlipNode: SlipNodeRep,
                  predecessorSlipNode: SlipNodeRep,
                  successorSlipNode: SlipNodeRep

                ) extends WorkspaceStructure {

  def rule_equal(rOpt: Option[Rule]): Boolean = {
    if (rOpt.isEmpty) {
      false
    } else {
      val r = rOpt.get
      (
        (relation == r.relation) &&
        (descriptorFacet == r.descriptorFacet) &&
        (objectCategory == r.objectCategory) &&
        (descriptor==r.descriptor)
      )
    }
  }

  def activate_rule_descriptions() = {
    (relation :: List(descriptorFacet,objectCategory,descriptor)).flatten.map(
      sn => slipnet ! SetSlipNodeBufferValue(sn,100.0)
    )
  }

  override def toString(): String = {
    s"replace ${descriptorFacet} of ${descriptor} ${objectCategory} by ${relation}"
  }

  def build_rule() = {
    // GUI workspace.Workspace_Rule.Change_Caption("rule : " +this.toString());
//    if (workspace.rule!=null) workspace.workspace_structures.removeElement(workspace.rule);
//    workspace.rule= this;
//    workspace.workspace_structures.addElement(this);
    activate_rule_descriptions();
  }

  // Rule.java.81
  def change_string(s: String): String = {
    if (relation.isEmpty) {
      // Does not look like it is supported
      // throw exeception
      throw new Exception("Rule change_string when relation is empty is not supported")
    }
    val relationVal = relation.get
    // applies the changes to this string ie. successor
    var stringok = true;
    if (descriptorFacet == lengthSlipNode.id) {
      if (relationVal == predecessorSlipNode.id)
        return s.substring(0,s.length()-1);
      if (relationVal == successorSlipNode.id) {
        val addon = s.substring(0,1);
        return s+addon;
      }
      return s;
    }
    // apply character changes
    val st = s.toList
    val ch = 36.toChar
    val charname = relationVal.toList


    val biteTheLine = st.find(c => {
      val lowerBite = c-1 < 97 && relationVal==predecessorSlipNode.id
      val higherBite = c+1 > 122 && relationVal==successorSlipNode.id
      lowerBite || higherBite
    })
    if (biteTheLine.isDefined) "NULL" else {
      val listOfChars: List[Char] = st.map(c => {
        (relationVal match {
          case r if r==predecessorSlipNode.id => c-1
          case r if r==successorSlipNode.id => c+1
          case _ => relationVal.toList.head + 32
        }).toChar
      })
      listOfChars.mkString
    }
  }
    def build_translated_rule(slippage_list_rep: List[ConceptMappingRep], target_string: String, target_object: List[WorkspaceObject]): Option[String] = {
      objectCategory = Rule.apply_slippages(objectCategory,slippage_list_rep)
      descriptorFacet = Rule.apply_slippages(descriptorFacet, slippage_list_rep)
      descriptor = Rule.apply_slippages(descriptor, slippage_list_rep)
      relation = Rule.apply_slippages(relation, slippage_list_rep)

      // generate the final string
      var final_answer = target_string;
      var changed_obOpt = target_object.find(wo => {
        descriptor.isDefined && wo.has_slipnode_description_with_id(descriptor.get) &&
        objectCategory.isDefined && wo.has_slipnode_description_with_id(objectCategory.get)
      })
      if (changed_obOpt.isDefined) {
        val changed_ob = changed_obOpt.get
        //System.out.println("changed object = "+changed_ob);
        val string_length = final_answer.length();
        val start_pos = changed_ob.left_string_position
        val end_pos = changed_ob.right_string_position
        var start_string = ""
        var middle_string = ""
        var end_string = ""
        if (start_pos>1) start_string = final_answer.substring(0,start_pos-1);
        middle_string =
          change_string(final_answer.substring(start_pos-1,end_pos));
        if (end_pos<string_length) end_string =
          final_answer.substring(end_pos);
        final_answer = start_string + middle_string + end_string;
        if (middle_string.equals("NULL")) return None
      }

      // HARD Coded
      val codelets_run = 0
      val formulas_actual_temperature = 100.0
      System.out.println(final_answer+" "+codelets_run + " " + formulas_actual_temperature)
//   GUI   workspace.Workspace_Answer.Change_Caption(final_answer);
//   GUI   workspace.Workspace_Comments.Change_Caption("translated rule : "+this.toString());
      Some(final_answer)
    }
/* for slipnet
    def calculate_internal_strength(){
      double cdd = descriptor.conceptual_depth-relation.conceptual_depth;
      if (cdd<0.0) cdd=-cdd;
      cdd=100.0-cdd;
      double av = (descriptor.conceptual_depth+relation.conceptual_depth)/2.0;
      av=Math.pow(av,1.1);

      double shared_descriptor_term = 0.0;

      // see if the object corresponds to an object
      // if so, see if the descriptor is present (modulo slippages) in the
      // corresponding object

      workspace_object changed = null;
      // find changed object;
      for (int i=0; i<workspace.initial.objects.size(); i++){
        workspace_object wo=(workspace_object)workspace.initial.objects.elementAt(i);
        if (wo.changed) changed=wo;
      }

      if ((changed!=null)&&(changed.correspondence!=null)){

        workspace_object obj2 = changed.correspondence.obj2;
        Vector slippages = workspace.slippage_list();
        slipnode s = descriptor;
        s=Rule.apply_slippages(s,slippages);
        if (obj2.has_description(s))
          shared_descriptor_term = 100.0;
        else {
          internal_strength = 0.0; return;
        }
      }

      double shared_descriptor_weight = Math.pow(((100.0-descriptor.conceptual_depth)/10.0),1.4);

      internal_strength = formulas.weighted_average(cdd,12,av,18,
        shared_descriptor_term, shared_descriptor_weight);
      if (internal_strength>100.0) internal_strength=100.0;
    }

    def calculate_external_strength(){
      external_strength=internal_strength;
    }*/
}
