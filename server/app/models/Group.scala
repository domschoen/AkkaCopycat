package models

import akka.actor.ActorRef
import models.Bond.BondRep
import models.Group.GroupRep
import models.WorkspaceStructure.WorkspaceStructureRep

import scala.collection.mutable.ListBuffer

object Group {
  case class GroupRep(
                       groupCategorySlipNodeID: String,
                       directionCategorySlipNodeID: Option[String],
                       bondFacetSlipNodeID: String,
                       bond_list: List[BondRep]
                     )


}

class Group (
              wString: WorkspaceString,
              val groupCategorySlipNodeID: String,
              val directionCategorySlipNodeID: Option[String],
              val bondFacetSlipNodeID: String,
              var object_list: ListBuffer[WorkspaceObject],
              var bond_list: ListBuffer[Bond],
              slipnet: ActorRef
            ) extends WorkspaceObject(wString) {
  import Slipnet.SetSlipNodeBufferValue


  def bondReps(): List[BondRep] = bond_list.toList.map(_.bondRep())

  def groupRep(): GroupRep = GroupRep(
    groupCategorySlipNodeID, directionCategorySlipNodeID, bondFacetSlipNodeID, bondReps)


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
  def break_group() = {
    for (wo <- object_list) {
      wo.group = None
    }
    if (workspaceString().isDefined) {
      workspaceString().get.break_group(this)
    }
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
    val grs = wString.objects.filter(wo =>
      if (wo.isInstanceOf[Group]) false else {
        val g = wo.asInstanceOf[Group]
        ((g.right_string_position<left_string_position)||
          (g.left_string_position>right_string_position)) &&
        ((g.groupCategorySlipNodeID == groupCategorySlipNodeID)&&
            (g.directionCategorySlipNodeID == directionCategorySlipNodeID))
    })
    grs.size
  }
  def local_density() = {
    val sg = number_of_local_supporting_groups().toDouble
    val ln = wString.length / 2.0
    //System.out.println(this+" local density="+ln+"  sg="+sg+" ln="+ln);
    100.0*sg/ln;
  }

}
