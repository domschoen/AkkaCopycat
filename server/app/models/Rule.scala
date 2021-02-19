package models

import akka.actor.ActorRef
import akka.event.LoggingAdapter
import models.ConceptMapping.ConceptMappingRep
import models.SlipNode.SlipNodeRep
import models.Slipnet.SetSlipNodeBufferValue
import models.Workspace.SlippageListShell

object Rule {

    def apply_slippages(sn: Option[SlipNodeRep], slippages: List[ConceptMappingRep]): Option[SlipNodeRep] = {
      val found = slippages.find(cm => sn.isDefined && sn.get.id.equals(cm.descriptor1.id))
      found match {
        case Some(cm) => Some(cm.descriptor2)
        case None => sn
      }
    }

}

/*case class SlipNodeRep(id: String, descriptorFacetSlipNodeID: String,
                        descriptorSlipNodeID: String,
                        objectCategorySlipNodeID: String,
                        relationSlipNodeID: String)*/
case class Rule (log: LoggingAdapter,
                  var descriptorFacet: Option[SlipNodeRep],
                  var descriptor: Option[SlipNodeRep],
                  var objectCategory: Option[SlipNodeRep],
                  var relation: Option[SlipNodeRep],
                  slipnet: ActorRef,
                  lengthSlipNode: SlipNodeRep,
                  predecessorSlipNode: SlipNodeRep,
                  successorSlipNode: SlipNodeRep

                ) extends WorkspaceStructure(log) {

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
      sn => slipnet ! SetSlipNodeBufferValue(sn.id,100.0)
    )
  }

  override def toString(): String = {
    val descriptorFacetString = SlipNode.displayStringWithOptionalSlipNodeRep(descriptorFacet)
    val descriptorString = SlipNode.displayStringWithOptionalSlipNodeRep(descriptor)
    val objectCategoryString = SlipNode.displayStringWithOptionalSlipNodeRep(objectCategory)
    val relationString = SlipNode.displayStringWithOptionalSlipNodeRep(relation)

    s"replace ${descriptorFacetString} of ${descriptorString} ${objectCategoryString} by ${relationString}"
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
    if (descriptorFacet.isDefined && descriptorFacet.get.id == lengthSlipNode.id) {
      if (relationVal.id == predecessorSlipNode.id)
        return s.substring(0,s.length()-1);
      if (relationVal.id == successorSlipNode.id) {
        val addon = s.substring(0,1);
        return s+addon;
      }
      return s;
    }
    // apply character changes
    val st = s.toList
    val ch = 36.toChar
    val charname = relationVal.id.toList


    val biteTheLine = st.find(c => {
      val lowerBite = c-1 < 97 && relationVal.id==predecessorSlipNode.id
      val higherBite = c+1 > 122 && relationVal.id==successorSlipNode.id
      lowerBite || higherBite
    })
    if (biteTheLine.isDefined) "NULL" else {
      val listOfChars: List[Char] = st.map(c => {
        (relationVal.id match {
          case r if r==predecessorSlipNode.id => c-1
          case r if r==successorSlipNode.id => c+1
          case _ => relationVal.id.toList.head + 32
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

      log.debug("generate the final string " + target_string + " objectCategory " + objectCategory + " descriptorFacet "
        + descriptorFacet + " descriptor " + descriptor + " relation " + relation)
      // generate the final string
      var final_answer = target_string;
      var changed_obOpt = target_object.find(wo => {
        log.debug("changed_obOpt " + wo)
        descriptor.isDefined && wo.has_slipnode_description(descriptor.get) &&
        objectCategory.isDefined && wo.has_slipnode_description(objectCategory.get)
      })
      if (changed_obOpt.isDefined) {
        log.debug("changed_obOpt is defined " + changed_obOpt)

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
        log.debug("middle_string " + middle_string)

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

  def update_strength_value(wInitialObjects: List[WorkspaceObject], slippage_list: List[ConceptMappingRep]) = {
    //log.debug("update_strength_value: " + this.uuid)
    calculate_internal_strength(wInitialObjects, slippage_list)
    calculate_external_strength()
    calculate_total_strength(log)
  };


  def calculate_internal_strength(wInitialObjects: List[WorkspaceObject], slippage_list: List[ConceptMappingRep]) = {
//    log.debug("calculate_internal_strength wInitialObjects " + wInitialObjects)
//    log.debug("calculate_internal_strength slippage_list " + slippage_list)
      val cdd0 = descriptor.get.conceptual_depth - relation.get.conceptual_depth;
      val cdd1 = if (cdd0 < 0.0) - cdd0 else cdd0
      val cdd= 100.0 - cdd1
      val av0 = (descriptor.get.conceptual_depth + relation.get.conceptual_depth) / 2.0
      val av = Math.pow(av0,1.1)

      var shared_descriptor_term = 0.0;

      // see if the object corresponds to an object
      // if so, see if the descriptor is present (modulo slippages) in the
      // corresponding object

      // find changed object;
      val changed = wInitialObjects.find(wo => wo.changed)
//    log.debug("calculate_internal_strength cdd: " + cdd + " av " + av + " changed " + changed);

      val cont = if ((changed.isDefined)&&(changed.get.correspondence.isDefined)) {
//        log.debug("calculate_internal_strength correspondence: " +changed.get.correspondence);

        val obj2 = changed.get.correspondence.get.obj2
        val s = Rule.apply_slippages(descriptor, slippage_list);
//        log.debug("calculate_internal_strength s: " +s);
        if (s.isDefined && obj2.has_slipnode_description(s.get)) {
//          log.debug("calculate_internal_strength has_slipnode_description");

          shared_descriptor_term = 100.0
          true
        } else {
          internal_strength = 0.0;
          false
        }
      } else true

      if (cont) {
        val shared_descriptor_weight = Math.pow(((100.0-descriptor.get.conceptual_depth)/10.0),1.4);
//        log.debug("calculate_internal_strength shared_descriptor_weight: " +shared_descriptor_weight);

        internal_strength = Formulas.weighted_average(cdd,12,av,18,
          shared_descriptor_term, shared_descriptor_weight);
        if (internal_strength>100.0) internal_strength=100.0;
      }
    }

   def calculate_external_strength() = {
      external_strength=internal_strength;
    }

  override def calculate_total_strength(log: LoggingAdapter) = {
    super.calculate_total_strength(log)
    log.debug("Rule calculate_total_strength "+ uuid + " rule: " + this + " total_strength " + total_strength)
  }
}
