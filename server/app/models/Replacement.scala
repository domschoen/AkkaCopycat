package models

import akka.event.LoggingAdapter

case class Replacement (log: LoggingAdapter,
                         from: WorkspaceObject,
                         to : WorkspaceObject,
                         relation: Option[String]
                       ) extends WorkspaceStructure(log)
