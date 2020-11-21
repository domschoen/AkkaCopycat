package models

import scala.collection.mutable.ListBuffer
import Letter.LetterSlipnetComplement

class WorkspaceString (val s: String, x1: Int, y1: Int, x2: Int, y2: Int) {
  val length = s.length
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
