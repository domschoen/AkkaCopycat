package models

import akka.actor.ActorRef
import akka.event.LoggingAdapter
import models.SlipNode.SlipNodeRep

object WorkspaceFormulas {

  val very_low_distribution = List(5.0,150.0,5.0,2.0,1.0,1.0,1.0,1.0,1.0,1.0)
  val low_distribution = List(2.0,5.0,150.0,5.0,2.0,1.0,1.0,1.0,1.0,1.0)
  val medium_distribution = List(1.0,2.0,5.0,150.0,5.0,2.0,1.0,1.0,1.0,1.0)
  val high_distribution = List(1.0,1.0,2.0,5.0,150.0,5.0,2.0,1.0,1.0,1.0)
  val very_high_distribution = List(1.0,1.0,1.0,2.0,5.0,150.0,5.0,2.0,1.0,1.0)



  def choose(vals: List[Double]): Int = {
    Utilities.valueProportionalRandomIndexInValueList(vals) + 1
  }

/* See Workspace
  static void update_temperature(){
    workspace.calculate_intra_string_unhappiness();
    workspace.calculate_inter_string_unhappiness();
    workspace.calculate_total_unhappiness();

    double rule_weakness=100.0;
    if (workspace.rule!=null){
      (workspace.rule).update_strength_value();
      rule_weakness=100.0-(workspace.rule).total_strength;
    }

    formulas.actual_temperature = formulas.weighted_average(workspace.total_unhappiness,0.8, rule_weakness,0.2);
    if (Temperature.clamped) formulas.actual_temperature = 100.0;
    Temperature.Update(formulas.actual_temperature);
    if (!clamp_temperature) formulas.temperature =formulas.actual_temperature;
    Temperature.Update(formulas.temperature);
    workspace.tvh.Values.addElement(new Double(100.0-workspace.total_unhappiness));
    workspace.temp.Values.addElement(new Double(formulas.actual_temperature));
  }
*/

  def temperature_adjusted_probability(value: Double, temperature: Double): Double = {
    Formulas.temperature_adjusted_probability(value, temperature)
  }

  def flip_coin(value: Double): Boolean = {
    Random.rnd(null) < value
  }

  /* slip
  def similar_has_property_links(s: SlipNode): List[SlipnetLink] ={
    Vector v = new Vector();
    for (int x=0; x<s.has_property_links.size(); x++){
      slipnet_link sl = (slipnet_link)s.has_property_links.elementAt(x);
      if (flip_coin(temperature_adjusted_probability(
        sl.degree_of_association()/100.0)))
        v.addElement(sl);
    }
    return v;
  }*/

  // See workspace.temperatureAdjustedValue
//  def temperature_adjusted_value(value: Double): Double = {
//    return Math.pow(value,((100.0-formulas.temperature)/30.0)+0.5);
//  }

  /* see
    static int select_list_position(Vector list){
     double totalval=0.0;
     for (int i=0; i<list.size(); i++){
        totalval+=((double)list.elementAt(i));
     }

     double pos = totalval*Random.rnd(null);
     int listpos = -1;
     totalval=0.0;
     for (int i=0; i<list.size(); i++){
        totalval+=((double)list.elementAt(i));
        if ((listpos==-1)&&(totalval>=pos))
          listpos=i;
     }
     if (listpos==-1) return 0;
     return listpos;
  }

   */


/* t of ws or t
  def choose_object_from_list(wos: List[WorkspaceObject], variable: String): Option[WorkspaceObject] = {
    // chooses an object from the the list by a variable
    // eg "intra-string-salience" probabilistically adjusted for temperature
    if (wos.isEmpty) {
      None
    } else {
      val object_probs = wos.map(wo => {
        if (variable.equals("intra_string_salience")) {
          temperature_adjusted_value(wo.intra_string_salience)

          if (variable.equals("inter_string_salience"))
        val=temperature_adjusted_value(ob.inter_string_salience);
        if (variable.equals("total_salience"))
        val=temperature_adjusted_value(ob.total_salience);
        if (variable.equals("relative_importance"))
        val=temperature_adjusted_value(ob.relative_importance);
        object_probs.addElement(new Double(val));
      }
      return wos(Utilities.valueProportionalRandomIndexInValueList(object_probs))

    })
    }

  }*/

  def choose_relevant_description_by_activation(activationBySlipNodeID: Map[String, Double],wo: WorkspaceObject): Option[Description] = {
    val v = wo.relevant_descriptions(activationBySlipNodeID);
    if (v.isEmpty) {
      None
    } else {
      val vals = v.map(d => if (d.descriptor.isEmpty) 0.0 else Workspace.activationWithSlipNodeRep(activationBySlipNodeID, d.descriptor.get))
      Some(v(Utilities.valueProportionalRandomIndexInValueList(vals)))
    }
  }

/*
  def choose_object(String variable,Vector oblist): WorkspaceObject = {
    // chooses an object from oblist by a variable
    // eg "intra-string-salience" probabilistically adjusted for temperature
    Vector objects = new Vector();

    for (int i=0; i<oblist.size(); i++){
      workspace_object ob = (workspace_object)oblist.elementAt(i);
      if (ob.string!=workspace.modified) objects.addElement(ob);
    }

    return choose_object_from_list(objects,variable);
  }*/
/* see slipnet
  static slipnode choose_slipnode_by_conceptual_depth(Vector slist): SlipNode = {
    if (slist.size()==0) return null;

    Vector object_probs = new Vector();
    for (int i=0; i<slist.size(); i++){
      slipnode s = (slipnode)slist.elementAt(i);
      double val = temperature_adjusted_value(s.conceptual_depth);
      object_probs.addElement(new Double(val));
    }
    return (slipnode)slist.elementAt(select_list_position(object_probs));

  }*/
/*
  Moved to Workspace.chooseNeighbor
  def choose_neighbor(fromob: WorkspaceObject): WorkspaceObject = {
    Vector objects = new Vector();

    for (int i=0; i<workspace.workspace_objects.size(); i++){
      workspace_object ob = (workspace_object)workspace.workspace_objects.elementAt(i);
      if (ob.string==fromob.string){
        if ((ob.left_string_position==(fromob.right_string_position+1))||
          (fromob.left_string_position==(ob.right_string_position+1)))
          objects.addElement(ob);
      }
    }
    return choose_object_from_list(objects,"intra_string_salience");
  }
  Moved to Workspace.chooseNeighbor
  def choose_left_neighbor(workspace_object fromob): WorkspaceObject = {
    Vector objects = new Vector();

    for (int i=0; i<workspace.workspace_objects.size(); i++){
      workspace_object ob = (workspace_object)workspace.workspace_objects.elementAt(i);
      if (ob.string==fromob.string){
        if (fromob.left_string_position==(ob.right_string_position+1))
          objects.addElement(ob);
      }
    }
    return choose_object_from_list(objects,"intra_string_salience");
  }

  Moved to Workspace.chooseNeighbor
  def choose_right_neighbor(workspace_object fromob): WorkspaceObject = {
    Vector objects = new Vector();

    for (int i=0; i<workspace.workspace_objects.size(); i++){
      workspace_object ob = (workspace_object)workspace.workspace_objects.elementAt(i);
      if (ob.string==fromob.string){
        if (ob.left_string_position==(fromob.right_string_position+1))
          objects.addElement(ob);
      }
    }
    return choose_object_from_list(objects,"intra_string_salience");
  }
*/
/* moved to Workspace and slipnet
  static slipnode choose_bond_facet(workspace_object fromob,workspace_object toob){
    Vector fromob_facets = new Vector();
    Vector bond_facets = new Vector();

    for (int i=0; i<fromob.descriptions.size(); i++){
      description d = (description)fromob.descriptions.elementAt(i);
      slipnode dt = d.description_type;
      if (slipnet.bond_facets.contains(dt))  fromob_facets.addElement(dt);
    }

    for (int i=0; i<toob.descriptions.size(); i++){
      description d = (description)toob.descriptions.elementAt(i);
      slipnode dt = d.description_type;
      if (fromob_facets.contains(dt))  bond_facets.addElement(dt);
    }

    if (bond_facets.size()==0) return null;


    Vector object_probs = new Vector();
    for (int i=0; i<bond_facets.size(); i++){
      slipnode ob = (slipnode)bond_facets.elementAt(i);
      double val = total_description_type_support(ob,fromob.string);
      object_probs.addElement(new Double(val));
    }
    return (slipnode)bond_facets.elementAt(select_list_position(object_probs));
  }
*/

  /* Moved to WorkspaceObject.get_descriptor
    static slipnode get_descriptor(workspace_object wo, slipnode dt){
     for (int i=0; i<wo.descriptions.size(); i++){
        description d = (description) wo.descriptions.elementAt(i);
        if (d.description_type==dt) return d.descriptor;
     }
     return null;
  }

   */
/*

  def total_description_type_support(slipnode description, workspace_string s): Double = {
    return (description.activation+local_description_type_support(description,s))/2.0;
  }
*/

  // See workspace
/*  def structure_vs_structure(s1: WorkspaceStructure,
                             w1: Double,
                             s2: WorkspaceStructure,
                             w2: Double): Boolean = {
    s1.update_strength_value();
    s2.update_strength_value();
    val v1 = s1.total_strength*w1;
    val v2 = s2.total_strength*w2;
    v1=temperature_adjusted_value(v1);
    v2=temperature_adjusted_value(v2);
    !(((v1+v2) * Random.rnd(null))>v1)
  }
*/
  // See workspace
/*
  def fight_it_out(wo: WorkspaceStructure, v1: Double, structs: List[WorkspaceStructure], v2: Double): Boolean = {
    if (structs.isEmpty) {
      true
    } else {
      !structs.find(ws => (!structure_vs_structure(wo,v1,ws,v2))).isDefined
    }
  }
 */


  def local_relevance(wString: WorkspaceString, categoryID: Option[String], bondFlavor: Bond => Option[SlipNodeRep]): Double =
    // is a function of how many bonds in the string have this bond category

    if (wString.objects.size == 1) 0.0 else {
      val oll = wString.objects.filter(wo => !wo.spans_string).size
      val bc = wString.objects.filter(wo => {
        System.out.println("local_bond_category_relevance wo " + wo);
        val bcs = if (wo.right_bond.isDefined) bondFlavor(wo.right_bond.get) else "null"
        System.out.println("local_bond_category_relevance wo.spans_string " + wo.spans_string
          + " wo.right_bond.isDefined " + wo.right_bond.isDefined + " bondFlavor(wo.right_bond.get) " + bcs);

        !wo.spans_string &&
          wo.right_bond.isDefined &&
          bondFlavor(wo.right_bond.get).map(_.id) == categoryID
      }).size
      System.out.println("local_bond_category_relevance bc " + bc + " oll " + oll);

      100.0 * bc / (oll-1.0)
    }

/* Merge into 1 method
  static double local_bond_category_relevance(workspace_string string, slipnode cat){
    // is a function of how many bonds in the string have this bond category
    double oll=0.0, bc=0.0;
    if (string.objects.size()==1) return 0.0;
    for (int i=0; i<string.objects.size(); i++){
      workspace_object wo = (workspace_object) string.objects.elementAt(i);
      if (!wo.spans_string){
        oll+=1.0;
        if (wo.right_bond!=null)
          if (wo.right_bond.bond_category==cat) bc+=1.0;
      }
    }
    return 100.0*bc/(oll-1.0);
  }

  static double local_direction_category_relevance(workspace_string string, slipnode dir){
    // is a function of how many bonds in the string have this bond category
    double oll=0.0, bc=0.0;
    for (int i=0; i<string.objects.size(); i++){
      workspace_object wo = (workspace_object) string.objects.elementAt(i);
      if (!wo.spans_string){
        oll+=1.0;
        if (wo.right_bond!=null)
          if (wo.right_bond.direction_category==dir) bc+=1.0;
      }
    }
    return 100.0*bc/(oll-1.0);
  }
*/

  def get_common_groups(from_obj: WorkspaceObject, to_obj: WorkspaceObject): List[Group] = {
    val stOpt = from_obj.workspaceString()
    stOpt match {
      case Some(st) =>
        st.objects.filter(wo =>
          ((from_obj.left_string_position>=wo.left_string_position)&&
          (from_obj.right_string_position<=wo.right_string_position)&&
          (to_obj.left_string_position>=wo.left_string_position)&&
          (to_obj.right_string_position<=wo.right_string_position))
        ).toList.asInstanceOf[List[Group]]
      case None => List.empty[Group]
    }
  }

  def get_incompatible_groups(obj: Group): List[Group] = {
    val v = obj.object_list.toList.map(wo => {
      wo.relatedGroups()
    })
    v.flatten
  }


  def same_group(gp1: Group, gp2: Group): Boolean = {
    if (gp1.left_string_position!=gp2.left_string_position) return false;
    if (gp1.right_string_position!=gp2.right_string_position) return false;
    if (gp1.group_category != gp2.group_category) return false;
    if (gp1.direction_category != gp2.direction_category) return false;
    if (gp1.bond_facet != gp2.bond_facet) return false;
    return true;
  }

  def group_present(proposed: Group): Boolean = {
    // returns true if a group matching this description already exists
    equivalent_group(proposed).isDefined
  }

  def equivalent_group(proposed: Group): Option[WorkspaceObject] = {
    // returns true if a group matching this description already exists
    val stOpt: Option[WorkspaceString] = proposed.wString
    stOpt match {
      case Some(st) =>
        st.objects.find(wo => wo.isInstanceOf[Group] && same_group(proposed,wo.asInstanceOf[Group]))
      case None =>
        // Should not happen ? let's return false
        println("Oups proposed group has no workspace string")
        None
    }
  }

/*
  def unrelated_objects(): List[WorkspaceObject] = {
    // returns a list of all objects in the workspace that have at least
    // one bond slot open
    Vector uo=new Vector();
    for (int i=0; i<workspace.workspace_objects.size();i++){
      workspace_object wo=(workspace_object)workspace.workspace_objects.elementAt(i);
      boolean ok =((wo.string==workspace.initial)||(wo.string==workspace.target));
      boolean left = ((wo.left_bond==null)&&(!wo.leftmost));
      boolean right = ((wo.right_bond==null)&&(!wo.rightmost));
      if ((ok)&&(!wo.spans_string)){
        if ((right)||(left)) uo.addElement(wo);;
      }
    }
    return uo;
  }

  def ungrouped_objects(): List[WorkspaceObject] = {
    // returns a list of all objects in the workspace that are not
    // in a group

    Vector uo=new Vector();
    for (int i=0; i<workspace.workspace_objects.size();i++){
      workspace_object wo=(workspace_object)workspace.workspace_objects.elementAt(i);
      boolean ok =((wo.string==workspace.initial)||(wo.string==workspace.target));
      if ((ok)&&(!wo.spans_string)){
        if (wo.group==null) uo.addElement(wo);;
      }
    }
    return uo;
  }

// see Workspace
  def unreplaced_objects(): List[WorkspaceObject] = {
    // returns a list of all objects in the initial string that are not
    // replaced

    Vector uo=new Vector();
    for (int i=0; i<workspace.workspace_objects.size();i++){
      workspace_object wo=(workspace_object)workspace.workspace_objects.elementAt(i);
      boolean ok =((wo.string==workspace.initial)&&(wo instanceof letter));
      if ((ok)&&(wo.replacement==null)){
        uo.addElement(wo);;
      }
    }
    return uo;
  }

  def uncorresponding_objects(): List[WorkspaceObject] = {
    // returns a list of all objects in the initial string that are not
    // replaced

    Vector uo=new Vector();
    for (int i=0; i<workspace.workspace_objects.size();i++){
      workspace_object wo=(workspace_object)workspace.workspace_objects.elementAt(i);
      boolean ok =((wo.string==workspace.initial)||(wo.string==workspace.target));
      if ((ok)&&(wo.correspondence==null)){
        uo.addElement(wo);;
      }
    }
    return uo;
  }
*/
  val SlipnetSamenessID = "sm"
  def possible_group_bond_list(log: LoggingAdapter,
                                bond_cat: SlipNodeRep,
                                direction: Option[SlipNodeRep],
                                bond_facet: SlipNodeRep,
                                bond_list: List[Bond],
                                slipnetLeft: SlipNodeRep,
                                slipnetRight: SlipNodeRep,
                                slipnet: ActorRef): Option[List[Bond]] = {
    val new_list = bond_list.map(b => {
      if ((b.bond_category==bond_cat) && (b.direction_category==direction)) {
        Some(b)
      } else {
        // a modified bond might be made
        if (bond_cat.id == SlipnetSamenessID) {
          // a different bond cannot be made here
          None
        } else {
          if ((b.bond_category==bond_cat) || (b.direction_category==direction)) {
            // a different bond cannot be made here
            None
          }  else {
            if (b.bond_category.id == SlipnetSamenessID) {
              None
            } else {
              val newB = new Bond(log,
                b.to_obj,
                b.from_obj,
                bond_cat,
                bond_facet,
                b.to_obj_descriptor,
                b.from_obj_descriptor,
                slipnetLeft,
                slipnetRight,
                slipnet
              )
              Some(newB)
            }
          }
        }
      }
    })
    val new_listFlatten = new_list.flatten
    if (new_listFlatten.size < bond_list.size) None else Some(new_listFlatten)
  }

}
