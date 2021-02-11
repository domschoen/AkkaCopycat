package models

import scala.collection.mutable.ListBuffer
import Letter.LetterSlipnetComplement
import akka.event.LoggingAdapter

class WorkspaceString (log: LoggingAdapter, val s: String, x1: Int, y1: Int, x2: Int, y2: Int, val description: String) {
  val length = s.length
  var intra_string_unhappiness = 0.0

  var bondRefs = Map.empty[String, Bond]

  // Graphics var ratio = 100.0;  // every letter is 100 long unless >(x2-x1)/len
  println("WorkspaceString " + s)

  var objects: ListBuffer[WorkspaceObject] = (for (i <- 0 to s.length -1) yield {
    new Letter(log,this, i+1, i+1).asInstanceOf[WorkspaceObject]
  }).to[ListBuffer]

  def letterSlipnetComplements(): List[LetterSlipnetComplement] = {
    objects.toList.asInstanceOf[List[Letter]].map(_.letterSlipnetComplement())
  }

 /* def bonds(): List[Bond] = {
    val subset = structureRefs.filter { case (k,v) => v.isInstanceOf[Bond] }
    subset.asInstanceOf[Map[String, WorkspaceObject]]
  }*/


  def update_relative_importance() = {
    // updates the normalized importances of all the objects in the string
    val total_raw_importance = objects.map(ob => ob.raw_importance).sum
    log.debug("update_relative_importance " + total_raw_importance);


    for (ob <- objects){
      log.debug("ob " + ob);

      if (total_raw_importance==0.0) {
        ob.relative_importance=0.0
        log.debug("1ob.relative_importance " + ob.relative_importance);

      } else {
        ob.relative_importance=ob.raw_importance/total_raw_importance
        log.debug("2ob.relative_importance " + ob.relative_importance);

      };
    }
  }

  def group_present(wg: Group): Option[Group] = {
    // searches for the group in the string
    // if an equivalent group exists, return this group
    // otherwise return null;
    val groups = objects.toList.filter(wo => wo.isInstanceOf[Group]).asInstanceOf[List[Group]]
    groups.find(gp => {
        (gp.left_string_position == wg.left_string_position) &&
          (gp.right_string_position == wg.right_string_position) &&
          (gp.bond_category == wg.bond_category) &&
          (gp.direction_category == wg.direction_category)
      })
  }

  def update_intra_string_unhappiness() = {
    //returns the average of the the intra-string unhapinesses of all
    // the objects in the string
    val isuRaw = objects.map(ob => ob.intra_string_unhappiness).sum

    val len = objects.size.toDouble
    val isu = if (len>0) (isuRaw/len) else isuRaw
    intra_string_unhappiness = isu;
  }


  def addBond(b: Bond) = {
    bondRefs += (b.uuid -> b)
  }
  def break_bond(b: Bond) = {
    bondRefs -= b.uuid
  }

  def bonds(): List[Bond] = {
    bondRefs.values.toList
  }

  def break_group(gr: Group): Unit = {
    objects -= gr
  }
}
