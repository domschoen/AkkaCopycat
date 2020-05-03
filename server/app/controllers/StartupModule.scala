package controllers

import com.google.inject.AbstractModule
import play.api.libs.concurrent.AkkaGuiceSupport

class StartupModule extends AbstractModule with AkkaGuiceSupport {

  override def configure() = {
    //bindActor[NodeActor]("node-actor")
  }
}


