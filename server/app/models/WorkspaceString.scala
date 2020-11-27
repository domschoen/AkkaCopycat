package models

import scala.collection.mutable.ListBuffer
import Letter.LetterSlipnetComplement

class WorkspaceString (val s: String, x1: Int, y1: Int, x2: Int, y2: Int) {
  val length = s.length
  var intra_string_unhappiness = 0.0

  var bondRefs = Map.empty[String, Bond]

  // Graphics var ratio = 100.0;  // every letter is 100 long unless >(x2-x1)/len
  println("WorkspaceString " + s)

  var objects: ListBuffer[WorkspaceObject] = (for (i <- 0 to s.length -1) yield {
    new Letter(this, i+1, i+1).asInstanceOf[WorkspaceObject]
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


    for (ob <- objects){
      if (total_raw_importance==0.0) ob.relative_importance=0.0;
      else ob.relative_importance=ob.raw_importance/total_raw_importance;
    }
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
