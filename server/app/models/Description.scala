package models

import models.SlipNode.SlipNodeRep


object Description {
  case class DescriptionRep(
                             uuid:String,
                             descriptionType: SlipNodeRep,
                             descriptor: Option[SlipNodeRep]
                           )

}

case class Description (
                         var wObject: WorkspaceObject,
                         var descriptionType : SlipNodeRep,
                         var descriptor : Option[SlipNodeRep]
                       ) extends WorkspaceStructure {
  //val descriptionTypeSlipNodeIDOpt = Option.empty[String]
  import Description.DescriptionRep

  // this is for GUI
  var visible = false

  def this(ob: WorkspaceObject, ws: WorkspaceString, dt: SlipNodeRep, dc: Option[SlipNodeRep]) = {
    this(ob, dt, dc)
    wString = Some(ws)
  }




  def descriptionRep(): DescriptionRep = DescriptionRep(uuid, descriptionType, descriptor)

  def break_description() = {
    wObject.break_description(this);
  }
}
