package models

class Letter (
               wString: WorkspaceString,
               lf: Int,
               rt: Int
             ) extends WorkspaceObject(wString) {



  left_string_position = lf
  right_string_position = rt


}
