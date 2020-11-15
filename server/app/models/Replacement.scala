package models

case class Replacement (
                         from: WorkspaceObject,
                         to : WorkspaceObject,
                         relation: Option[String]
                       ) extends WorkspaceStructure
