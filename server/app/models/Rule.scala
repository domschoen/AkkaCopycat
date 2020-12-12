package models

import akka.actor.ActorRef
import models.SlipNode.SlipNodeRep
import models.Slipnet.SetSlipNodeBufferValue

object Rule {

  /*
    public static slipnode apply_slippages(slipnode sn,Vector slippages){
      for (int x=0; x<slippages.size(); x++){
        concept_mapping cm = (concept_mapping)slippages.elementAt(x);
        if (sn==cm.descriptor1) return cm.descriptor2;
      }
      return sn;
    }*/
}

/*case class SlipNodeRep(id: String, descriptorFacetSlipNodeID: String,
                        descriptorSlipNodeID: String,
                        objectCategorySlipNodeID: String,
                        relationSlipNodeID: String)*/
case class Rule (
                  descriptorFacet: Option[String],
                  descriptor: Option[String],
                  objectCategory: Option[String],
                  var relation: Option[String],
                  slipnet: ActorRef,
                  lengthSlipNode: SlipNodeRep,
                  predecessorSlipNode: SlipNodeRep,
                  successorSlipNode: SlipNodeRep

                ) extends WorkspaceStructure {

  def rule_equal(r: Rule): Boolean = {
    if (r==null) return false;
    if ((relation == r.relation)&&
      (descriptorFacet == r.descriptorFacet)&&
      (objectCategory == r.objectCategory)&&
      (descriptor==r.descriptor)) return true;
    return false;
  }

  def activate_rule_descriptions() = {
    (relation :: List(descriptorFacet,objectCategory,descriptor)).flatten.map(
      sn => slipnet ! SetSlipNodeBufferValue(sn,100.0)
    )
  }

  override def toString(): String = {
    s"replace ${descriptorFacet} of ${descriptor} ${objectCategory} by ${relation}"
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
  /* for slipnet
    def build_translated_rule(codelets_run: Int): Boolean = {
      Vector v = slippage_list();
      object_category = apply_slippages(object_category,v);
      descriptor_facet = apply_slippages(descriptor_facet,v);
      descriptor = apply_slippages(descriptor,v);
      relation = apply_slippages(relation,v);
      //System.out.println(this);

      // generate the final string
      final_answer = workspace.target_string;
      workspace_object changed_ob = null;
      for (int x=0; x<workspace.target.objects.size(); x++){
        workspace_object wo = (workspace_object)workspace.target.objects.elementAt(x);
        if ((wo.has_description(descriptor))&&
          (wo.has_description(object_category))) changed_ob = wo;
      }
      if (changed_ob!=null){
        //System.out.println("changed object = "+changed_ob);
        int string_length = final_answer.length();
        int start_pos = changed_ob.left_string_position;
        int end_pos = changed_ob.right_string_position;
        String start_string = "";
        String middle_string = "";
        String end_string = "";
        if (start_pos>1) start_string = final_answer.substring(0,start_pos-1);
        middle_string =
          change_string(final_answer.substring(start_pos-1,end_pos));
        if (end_pos<string_length) end_string =
          final_answer.substring(end_pos);
        final_answer = start_string + middle_string + end_string;
        if (middle_string.equals("NULL")) return false;
      }
      System.out.println(final_answer+" "+codelets_run+" "+
        formulas.actual_temperature);
      workspace.Workspace_Answer.Change_Caption(final_answer);
      workspace.Workspace_Comments.Change_Caption("translated rule : "+this.toString());
      return true;
    }

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
