package models

import akka.actor.ActorRef
import akka.event.LoggingAdapter
import models.Bond.BondRep
import models.Group.GroupRep
import models.SlipNode.{GroupSlipnetInfo, SlipNodeRep}
import models.WorkspaceObject.WorkspaceObjectRep
import models.WorkspaceStructure.WorkspaceStructureRep

import scala.collection.mutable.ListBuffer

object Group {
  case class GroupRep(
                       uuid: String,
                       //workspaceObjectRep: WorkspaceObjectRep,
                       group_category: SlipNodeRep,
                       direction_category: Option[SlipNodeRep],
                       bond_facet: SlipNodeRep,
                       bond_list: List[BondRep]
                     )

  case class FutureGroupRep(
                             group_category: SlipNodeRep,
                             direction_category: Option[SlipNodeRep],
                             bond_facet: SlipNodeRep,
                             bond_category: SlipNodeRep,
                             bond_list: List[BondRep],
                             groupSlipnetInfo: GroupSlipnetInfo
                     )


}

class Group (
              log: LoggingAdapter,
              ws: WorkspaceString,
              val group_category: SlipNodeRep,
              val direction_category: Option[SlipNodeRep],
              val bond_facet: SlipNodeRep,
              var object_list: ListBuffer[WorkspaceObject],
              var bond_list: ListBuffer[Bond],
              var bond_category: SlipNodeRep,
              groupSlipnetInfo: GroupSlipnetInfo,
              temperature: Double,
              slipnet: ActorRef,
              activationBySlipNodeID: Map[String, Double]
            ) extends WorkspaceObject(log, ws) {
  import Slipnet.SetSlipNodeBufferValue

  //var bond_category: Option[SlipNodeRep] = None
  val leftob = object_list(0)
  val rightob = object_list.last
  left_string_position = leftob.left_string_position;
  leftmost = (left_string_position==1);
  right_string_position = rightob.right_string_position;
  rightmost = (right_string_position==(ws.length));

  spans_string = (leftmost&rightmost);

  if (!bond_list.isEmpty){
    val bbf = bond_list(0).bond_facet;
    add_bond_description(new Description(log, this,groupSlipnetInfo.bond_facet,Some(bbf)))
  }
  add_bond_description(new Description(log,this,groupSlipnetInfo.bond_category,Some(bond_category)))
  add_description(groupSlipnetInfo.object_category, Some(groupSlipnetInfo.group))
  add_description(groupSlipnetInfo.group_category, Some(group_category))
  if (direction_category.isEmpty){
    // sameness group - find letter_category
    val letter = object_list(0).get_description(bond_facet)
    add_description(bond_facet,letter)
  }

  if (direction_category.isDefined) add_description(groupSlipnetInfo.direction_category,direction_category);
  if (spans_string) add_description(groupSlipnetInfo.string_position_category, Some(groupSlipnetInfo.whole));
  else if (left_string_position==1)
    add_description(groupSlipnetInfo.string_position_category, Some(groupSlipnetInfo.leftmost))
  else if (right_string_position == ws.length)
    add_description(groupSlipnetInfo.string_position_category, Some(groupSlipnetInfo.rightmost))
  else if (middle_object())
    this.add_description(groupSlipnetInfo.string_position_category, Some(groupSlipnetInfo.middle))

  // check whether or not to add length description category
  val prob = length_description_probability(activationBySlipNodeID,temperature);
  if (Random.rnd() < prob){
    val length = object_list.size
    if (length<6) add_description(groupSlipnetInfo.length, Some(groupSlipnetInfo.slipnet_numbers(length-1)))
  }



  def bondReps(): List[BondRep] = bond_list.toList.map(_.bondRep())

  def groupRep(): GroupRep = GroupRep(
    uuid,
    //workspaceObjectRep(),
    group_category, direction_category, bond_facet, bondReps)


//  def add_description(dt: SlipNodeRep, d: SlipNodeRep){
//    description ds = new description(this, string, dt, d);
//    descriptions.addElement(ds);
//  }
  def add_bond_description(d: Description) {
    bond_descriptions += d
  }


  def single_letter_group_probability(lengthActivation: Double, temperature: Double): Double = {
    val loc = number_of_local_supporting_groups()
    if (loc==0) {
      //System.out.println("single letter prob = 0.0");
      0.0
    } else {
      val exp = if (loc==1) 4.0 else if (loc==2) 2.0 else 1.0
      val value = Formulas.temperature_adjusted_probability(
        Math.pow(
          (local_support()/100.0) * (lengthActivation / 100.0),
          exp
        ),
        temperature
      )
      //System.out.println("single letter prob ="+val);
      value
    }
  }

  def build_group() = {
    ws.objects += this
    for (wo <- object_list) {
      wo.group = Some(this)
    }
  }

  def activate_descriptions() = {
    for (d <- descriptions) {
      d.descriptor match {
        case Some(descriptor) =>
          slipnet ! SetSlipNodeBufferValue(descriptor.id, 100.0)

        case None =>
          println("oup oups !");
      }
    }
  }

  def length_description_probability(activationBySlipNodeID: Map[String, Double], temperature: Double): Double = {
    val length = object_list.size
    if (length>5) return 0.0;
    val cube = (length*length*length).toDouble
    val prob = Math.pow(0.5,cube*
      ((100.0- Workspace.activationWithSlipNodeRep(activationBySlipNodeID, groupSlipnetInfo.length))/100.0));

    val value = Formulas.temperature_adjusted_probability(prob, temperature)
    if (value < 0.06) {
      0.0 // otherwise 1/20 chance always
    } else {
      //System.out.println(this.toString()+" length description prob = "+val);
      value
    }
  }


  def break_group() = {
    System.out.println("breaking group "+this);

    for (wo <- object_list) {
      wo.group = None
    }
    ws.break_group(this)
  }

  def update_strength_value(degree_of_association: Double) = {
    calculate_internal_strength(degree_of_association)
    calculate_external_strength()
    calculate_total_strength()
  };

  //    val bc = (slipnet_formulas.get_related_node(group_category,slipnet.bond_category)).degree_of_association();
  def calculate_internal_strength(degree_of_association: Double) = {
    val bff = if (bond_facet.id == SlipNode.id.letter_category) 1.0 else 0.5
    //System.out.println("related node:"+(slipnet_formulas.get_related_node(group_category,slipnet.bond_category)).pname);
    val len = object_list.size;
    val lc = len match {
      case 1 => 5.0
      case 2 => 20.0
      case 3 => 60.0
      case _ => 90.0
    }

    val bcw = Math.pow(degree_of_association,0.98);
    val lcw = 100.0-bcw;
    internal_strength = Formulas.weighted_average(degree_of_association,bcw,lc,lcw);
    //System.out.println(this+" bc:"+bc+" bcw:"+bcw+" lc:"+lc+" lcw:"+" internal strength = "+internal_strength);
  }

  def calculate_external_strength(){
    if (spans_string) external_strength = 100.0;
    else external_strength = local_support();
    //System.out.println(this+" external strength = "+external_strength);
  }

  def local_support(): Double = {
    val num = number_of_local_supporting_groups().toDouble
    if (num==0.0) 0.0 else {
      val density = local_density();
      val ad= 100.0*Math.sqrt(density/100.0);
      val nfr = Math.pow(0.6,1/(num*num*num));
      val nf = if (nfr > 1.0) 1.0 else nfr
      //System.out.println("local support  density="+density+"num="+num+" ad="+ad+"nf="+nf);
      ad*nf
    }
  }

  def number_of_local_supporting_groups() = {
    val grs = ws.objects.filter(wo =>
      if (wo.isInstanceOf[Group])  {
        val g = wo.asInstanceOf[Group]
        ((g.right_string_position<left_string_position)||
          (g.left_string_position>right_string_position)) &&
        ((g.group_category == group_category)&&
            (g.direction_category == direction_category))
    } else false)
    grs.size
  }
  def local_density() = {
    val sg = number_of_local_supporting_groups().toDouble
    val ln = ws.length / 2.0
    //System.out.println(this+" local density="+ln+"  sg="+sg+" ln="+ln);
    100.0*sg/ln;
  }

}
