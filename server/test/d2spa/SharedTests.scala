package d2spa

import d2spa.shared._
import akka.actor.ActorSystem
import akka.testkit.{ImplicitSender, TestActors, TestKit}
import models.{ExecutionRun, WebSocketActor}
import models.ExecutionRun.{Found, Run}
import org.scalatest.{BeforeAndAfterAll, Matchers, WordSpecLike}

import scala.concurrent.duration._


object TestData {

}



class StackSpec extends  TestKit(ActorSystem("MySpec"))
  with ImplicitSender
  with WordSpecLike
  with Matchers
  with BeforeAndAfterAll {


  "Typical cases" must {
    "test 1: abc -> abd then ijk -> ijl " in {
      val runActor = system.actorOf(ExecutionRun.props)
      runActor ! Run("abc", "abd", "ijk")
      expectMsg(30.seconds, WebSocketActor.Found("ijl"))
    }
    "test 2: abc -> abd then mrrjjj -> mrrkkk " in {
      val runActor = system.actorOf(ExecutionRun.props)
      runActor ! Run("abc", "abd", "mrrjjj")
      expectMsg(15.seconds, WebSocketActor.Found("mrrkkk"))
    }
    "test 3: abc -> abd then iijjkk -> iijjll " in {
      val runActor = system.actorOf(ExecutionRun.props)
      runActor ! Run("abc", "abd", "iijjkk")
      expectMsg(15.seconds, WebSocketActor.Found("iijjll"))
    }
    "test 4: abc -> abd then kji -> kjh " in {
      val runActor = system.actorOf(ExecutionRun.props)
      runActor ! Run("abc", "abd", "kji")
      expectMsg(15.seconds, WebSocketActor.Found("kjh"))
    }
    "test 5: abc -> abd then xyz -> xyd " in {
      val runActor = system.actorOf(ExecutionRun.props)
      runActor ! Run("abc", "abd", "xyz")
      expectMsg(15.seconds, WebSocketActor.Found("xyd"))
    }
    "test 6: abcm -> abcn then rijk -> rijn " in {
      val runActor = system.actorOf(ExecutionRun.props)
      runActor ! Run("abcm", "abcn", "rijk")
      expectMsg(15.seconds, WebSocketActor.Found("rijn"))
    }
    "test 7: abc -> abd then ijklmnop -> ijklmnod " in {
      val runActor = system.actorOf(ExecutionRun.props)
      runActor ! Run("abc", "abd", "ijklmnop")
      expectMsg(15.seconds, WebSocketActor.Found("ijklmnod"))
    }
    "test 8: abc -> abd then xlg -> xld " in {
      val runActor = system.actorOf(ExecutionRun.props)
      runActor ! Run("abc", "abd", "xlg")
      expectMsg(15.seconds, WebSocketActor.Found("xld"))
    }
    "test 9: abc -> abd then abcd -> abcd " in {
      val runActor = system.actorOf(ExecutionRun.props)
      runActor ! Run("abc", "abd", "abcd")
      expectMsg(15.seconds, WebSocketActor.Found("abcd"))
    }
    "test 10: abc -> abd then xcg -> xcd " in {
      val runActor = system.actorOf(ExecutionRun.props)
      runActor ! Run("abc", "abd", "xcg")
      expectMsg(15.seconds, WebSocketActor.Found("xcd"))
    }

    "test 11: abc -> abd then cde -> cdd " in {
      val runActor = system.actorOf(ExecutionRun.props)
      runActor ! Run("abc", "abd", "cde")
      expectMsg(15.seconds, WebSocketActor.Found("cdd"))
    }
  }

}