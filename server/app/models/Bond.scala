package models

import akka.actor.ActorRef
import models.SlipNode.SlipNodeRep
import models.Slipnet.SetSlipNodeBufferValue

object Bond {
  case class BondRep(
                      uuid: String,
                      from_obj: String,
                     to_obj: String,
                     bondCategorySlipNodeID: String,
                     bondFacetSlipNodeID: String,
                     from_obj_descriptorSlipNodeID: Option[String],
                     to_obj_descriptorSlipNodeID: Option[String],
                      direction_category: Option[SlipNodeRep]
                    )

}

class Bond (
             val from_obj: WorkspaceObject,
             val to_obj: WorkspaceObject,
             val bond_category: SlipNodeRep,
             val bond_facet: SlipNodeRep,
             val from_obj_descriptor: Option[SlipNodeRep],
             val to_obj_descriptor: Option[SlipNodeRep],
             slipnetLeft: SlipNodeRep,
             slipnetRight: SlipNodeRep,
             slipnet: ActorRef
           ) extends WorkspaceStructure {

  import Bond.BondRep

  val leftGreater = from_obj.left_string_position > to_obj.right_string_position
  val left_obj: WorkspaceObject = if (leftGreater) to_obj else from_obj
  val right_obj: WorkspaceObject = if (leftGreater) from_obj else to_obj

  val bidirectional = from_obj_descriptor == to_obj_descriptor // true if sameness bond

  val direction_category: Option[SlipNodeRep] = if (bidirectional) None else {
    Some(if (leftGreater) slipnetLeft else slipnetRight)
  }
  val right = to_obj == right_obj // true if to_obj is on the right

  wString = from_obj.wString

  override def toString(): String = {
    s"${bond_category.id} bond between ${left_obj} and ${right_obj}"
  }

  def bondRep(): BondRep = BondRep(
    uuid,
    from_obj.uuid,
    to_obj.uuid,
    bond_category.id,
    bond_facet.id,
    from_obj_descriptor.map(_.id),
    to_obj_descriptor.map(_.id),
    direction_category
  )

  def get_incompatible_bonds(): List[Bond] = {
    workspaceString().get.bonds().filter(b =>
      (b.left_obj == left_obj) || (b.right_obj == right_obj)
    )
  }

  // partially moved to Workspace.addBond
  def build_bond() = {
    System.out.println("bond.build_bond | string.bonds.add this " + workspaceString().get.s + " this " + this);

    workspaceString().get.addBond(this);

    activateDescriptor()
    left_obj.right_bond = Some(this);
    right_obj.left_bond = Some(this);
    left_obj.addBond(this)
    right_obj.addBond(this)
  }

  // partially moved to Workspace.break_bond
  def break_bond() = {
    System.out.println("bond.break_bond " + this);

    if (workspaceString().isDefined) workspaceString().get.break_bond(this)
    left_obj.right_bond = None;
    right_obj.left_bond = None;
    left_obj.break_bond(this);
    right_obj.break_bond(this);
// GUI   workspace.WorkspaceArea.Redraw = true;
  }


  def activateDescriptor() = {
    slipnet ! SetSlipNodeBufferValue(bond_category.id, 100.0)
    if (direction_category.isDefined) {
      slipnet ! SetSlipNodeBufferValue(direction_category.get.id, 100.0)
    }
  }

  def correspondenceInDirection(
                                  cwo: WorkspaceObject,
                                  dirMost: WorkspaceObject => Boolean,
                                  sideBond: WorkspaceObject => Option[Bond],
                                  initial: WorkspaceString
                               ): Option[Correspondence] = {
    if ((dirMost(cwo)) && (cwo.correspondence.isDefined)) {
      val locOpt = cwo.correspondence;
      if (locOpt.isDefined) {
        val loc = locOpt.get
        val wo = if (workspaceString() == initial) loc.obj2 else loc.obj1
        if ((dirMost(wo)) && (sideBond(wo).isDefined)) {
            val rb = sideBond(wo).get
            // ignore test on rb.direction_category != null
            if (rb.direction_category != direction_category) {
              Some(loc)
            } else None
        } else None
      } else None
    } else None
  }

  def get_incompatible_correspondences(initial: WorkspaceString): List[Correspondence] = {
    // returns a list of correspondences that are incompatible with this bond
    val locOpt = correspondenceInDirection(
      left_obj,
      (wo: WorkspaceObject) => wo.leftmost,
      (wo: WorkspaceObject) => wo.right_bond,
      initial)
    val rocOpt = correspondenceInDirection(
      right_obj,
      (wo: WorkspaceObject) => wo.rightmost,
      (wo: WorkspaceObject) => wo.left_bond,
      initial)
    List(locOpt,rocOpt).flatten
  }
  def calculate_internal_strength(bond_degree_of_association: Double) = {
    // bonds between objects of same type(ie. letter or group) are
    // stronger than bonds between different types
    val fromgp = (from_obj.left_string_position!=from_obj.right_string_position);
    val togp = (to_obj.left_string_position!=to_obj.right_string_position);
    val member_compatibility = if (fromgp==togp) 1.0 else 0.7

    // letter category bonds are stronger
    val bff = if (bond_facet.id == SlipNode.id.letter_category) 1.0 else 0.7
    val intstr = member_compatibility * bff * bond_degree_of_association;
    //System.out.println(bond_category.pname+" bdoa:"+bond_category.bond_degree_of_association());
    internal_strength = if (intstr > 100.0 ) 100.0 else intstr
    //System.out.println(this+" internal strength:"+internal_strength);
    internal_strength
  }


  def number_of_local_supporting_bonds(): Int = {
    wString match {
      case Some(ws) =>
        println("ws.bonds " + ws.bonds.size);
        println("from_obj.wString " + from_obj.wString);
        println("wString " + ws);
        println("wString " + ws.s);
        ws.bonds.filter(ob => {
          println("ob " + ob.wString);

          (ob.wString.equals(from_obj.wString)) &&
           (
             (!(left_obj.letter_distance(ob.left_obj)==0)) &&
             (!(right_obj.letter_distance(ob.right_obj)==0)) &&
             (bond_category==ob.bond_category) &&
             (direction_category==ob.direction_category)
           )
          }
        ).size
      case None => 0
    }
  }

  def local_density(wos: List[WorkspaceObject]): Double = {
    wString match {
      case Some(ws) =>
        // returns a rough measure of the density in the string
        // of the same bond-category and the direction-category of
        // the given bond
        var slot_sum =0.0
        var support_sum = 0.0

        for (ob1 <- wos) {
          if (ob1.wString == wString){
            for (ob2 <- wos) {
              if (ob1.wString == ob2.wString)
                if ((ob1.left_string_position==(ob2.right_string_position+1))||
                  (ob1.right_string_position==(ob2.left_string_position-1))){
                  // they are neighbours
                  slot_sum+=1.0;
                  for (b <- ws.bonds) {
                    if ((b!=this)&&(((from_obj==ob1)&&(to_obj==ob2))||
                      ((from_obj==ob2)&&(to_obj==ob1))))

                      if ((b.bond_category==bond_category)&&
                        (b.direction_category==direction_category))
                        support_sum+=1.0;

                  }
                }
            }
          }
        }

        if (slot_sum==0.0) 0.0 else 100.0*support_sum/slot_sum;

      case None => 0.0

    }
  }

  def update_strength_value(activationBySlipNodeID: Map[String, Double], bond_category_degree_of_association: Double, wos: List[WorkspaceObject]) = {
    calculate_internal_strength(bond_category_degree_of_association)
    calculate_external_strength(activationBySlipNodeID, wos)
    calculate_total_strength()
  };


  override def calculate_external_strength(activationBySlipNodeID: Map[String, Double], wos: List[WorkspaceObject]) = {
    // equals the local support

    val num : Double = number_of_local_supporting_bonds()
    println("calculate_external_strength " + num)
    val extstr = if (num > 0.0) {
      val density = Math.sqrt(local_density(wos) / 100) * 100.0
      val nf1 = Math.pow(0.6,(1.0/(num*num*num)));
      val nf = if (nf1 < 1.0) 1.0 else nf1
      nf * density
    } else 0.0

    external_strength = extstr;
    //System.out.println(this+" external strength:"+external_strength);
  }

}