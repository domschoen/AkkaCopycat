package models

import scala.collection.mutable.ListBuffer
import Letter.LetterSlipnetComplement

class WorkspaceString (val s: String, x1: Int, y1: Int, x2: Int, y2: Int) {
  val length = s.length
  // Graphics var ratio = 100.0;  // every letter is 100 long unless >(x2-x1)/len
  println("WorkspaceString " + s)

  var objects: ListBuffer[WorkspaceObject] = (for (i <- 0 to s.length -1) yield {
    new Letter(this, i+1, i+1).asInstanceOf[WorkspaceObject]
  }).to[ListBuffer]

  def letterSlipnetComplements(): List[LetterSlipnetComplement] = {
    objects.toList.asInstanceOf[List[Letter]].map(_.letterSlipnetComplement())
  }

}
