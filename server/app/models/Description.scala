package models

import models.Slipnet.DescriptionRep

case class SlipNodeRep(id: String, activation: Double)

case class Description (
                         var wObject: WorkspaceObject,
                         var descriptionType : SlipNodeRep,
                         var descriptor : Option[SlipNodeRep]
                       ) extends WorkspaceStructure {
  //val descriptionTypeSlipNodeIDOpt = Option.empty[String]

  // this is for GUI
  var visible = false

  def this(ob: WorkspaceObject, ws: WorkspaceString, dt: SlipNodeRep, dc: Option[SlipNodeRep]) = {
    this(ob, dt, dc)
    wString = Some(ws)
  }




  def descriptionRep(): DescriptionRep = DescriptionRep(uuid, descriptionType.id, if(descriptor.isEmpty) None else Some(descriptor.get.id))


}
