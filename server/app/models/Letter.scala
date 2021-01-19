package models

object Letter {

  case class LetterSlipnetComplement(
                                    uuid: String,
                                    x: Int,
                                    letval: Int,
                                    len: Int
                                    )

}

class Letter (
               ws: WorkspaceString,
               lf: Int,
               rt: Int
             ) extends WorkspaceObject(ws) {

  import Letter.LetterSlipnetComplement

  left_string_position = lf
  right_string_position = rt

  leftmost = lf == 1
  rightmost = rt == ws.s.length
  spans_string = ((lf==1)&&(rt==(ws.s.length)))


  def letterSlipnetComplement(): LetterSlipnetComplement = {
    val x = lf - 1
    val s2 = ws.s.substring(x,x+1)
    val letterChar = s2(0)
    val letterInt = letterChar.toInt
    val letval = if (letterInt > 96) letterInt- 97 else letterInt -65

    LetterSlipnetComplement(uuid, x, letval, ws.length)
  }
}
