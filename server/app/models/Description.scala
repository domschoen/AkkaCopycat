package models

import akka.event.LoggingAdapter
import models.SlipNode.SlipNodeRep


object Description {
  case class DescriptionRep(
                             uuid:String,
                             descriptionType: SlipNodeRep,
                             descriptor: Option[SlipNodeRep]
                           )

}

case class Description (log: LoggingAdapter,
                         var wObject: WorkspaceObject,
                          wsOpt: Option[WorkspaceString],
                         var description_type : SlipNodeRep,
                         var descriptor : Option[SlipNodeRep]
                       ) extends WorkspaceStructure(log) {
  //val descriptionTypeSlipNodeIDOpt = Option.empty[String]
  import Description.DescriptionRep
  wString = wsOpt

  //var wObject : WorkspaceObject = null;

  // this is for GUI
  var visible = false

  def this(log: LoggingAdapter, ob: WorkspaceObject,  dt: SlipNodeRep, dc: Option[SlipNodeRep]) = {
    this(log, ob, ob.wString, dt, dc)
    printNewDescription()
  }

  def this(log: LoggingAdapter, ob: WorkspaceObject, ws: WorkspaceString, dt: SlipNodeRep, dc: Option[SlipNodeRep]) = {
    this(log, ob, Some(ws), dt, dc)
    printNewDescription()
  }


  def printNewDescription() = {
    log.debug(s"New Description string: ${wString.map(_.description)} wo: ${wObject} " +
      s"description type: ${SlipNode.displayStringWithOptionalSlipNodeRep(Some(description_type))} " +
      s"descriptor: ${SlipNode.displayStringWithOptionalSlipNodeRep(descriptor)}")

  }

  def descriptionRep(): DescriptionRep = DescriptionRep(uuid, description_type, descriptor)


  override def toString(): String = {
    val descriptorString = descriptor match {
      case Some(sn) => SlipNode.longNameWithId(sn.id)
      case None => ""
    }
    val stringLocation = wObject.wString match {
      case Some(ws) => ws.description
      case None => ""
    }
    s"description(${descriptorString}) of $wObject in ${stringLocation} string"
  }

  override def calculate_internal_strength() = {
    internal_strength = descriptor match {
      case Some(d) => d.conceptual_depth
      case None => 0.0
    }
  }
  override def calculate_external_strength(activationBySlipNodeID: Map[String, Double], wos: List[WorkspaceObject]) = {
    external_strength = (local_support(wos) + Workspace.activationWithSlipNodeRep(activationBySlipNodeID, description_type))/2.0;
  }

  def local_support(wos: List[WorkspaceObject]): Double = {
    var num_supporting_objects = 0;
    // num_supporting_objects = the number of objects in the string
    // with a description of the given object facet
    for (wo <- wos){
      if (wo != wObject) {
        if (!(wObject.recursive_group_member(wo) || wo.recursive_group_member(wObject))){
          // check to see if this obejct has descriptions that have same description_type
          for (ds <- wo.descriptions){
            if (ds.description_type==description_type) num_supporting_objects += 1
          }
        }
      }
    }
    if (num_supporting_objects==0) return 0.0;
    if (num_supporting_objects==1) return 20.0;
    if (num_supporting_objects==2) return 60.0;
    if (num_supporting_objects==3) return 90.0;
    return 100.0;
  }





  def break_description() = {
    wObject.break_description(this);
  }
}
