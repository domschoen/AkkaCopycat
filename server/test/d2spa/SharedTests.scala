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
   /* "test 1: abc -> abd then ijk -> ijl " in {
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
      runActor ! Run("abc", "abd", "cde")C
      expectMsg(15.seconds, WebSocketActor.Found("cdd"))
    }
    "test 12: abc -> abd then cab -> dab " in {
      val runActor = system.actorOf(ExecutionRun.props)
      runActor ! Run("abc", "abd", "cab")
      expectMsg(15.seconds, WebSocketActor.Found("dab"))
    }
    "test 13: abc -> abd then cmg -> cmd " in {
      val runActor = system.actorOf(ExecutionRun.props)
      // Most expected answer: cmh
      runActor ! Run("abc", "abd", "cmg")
      expectMsg(15.seconds, WebSocketActor.Found("cmd"))
    }
    "test 14: abc -> qbc then ijk -> qjk " in {
      val runActor = system.actorOf(ExecutionRun.props)
      runActor ! Run("abc", "qbc", "ijk")
      expectMsg(15.seconds, WebSocketActor.Found("qjk"))
    }
        "test 15: aabc -> aabd then ijkk -> ijll " in {
          val runActor = system.actorOf(ExecutionRun.props)
          runActor ! Run("aabc", "aabd", "ijkk")
          expectMsg(15.seconds, WebSocketActor.Found("ijll"))
        }
        "test 16: abcm -> abcn then rijk -> sijk " in {
          val runActor = system.actorOf(ExecutionRun.props)
          runActor ! Run("abcm", "abcn", "rijk")
          // Most expected answer: sijk
          expectMsg(15.seconds, WebSocketActor.Found("rijn"))
        }
        "test 17: abc -> abd then iijjkk -> iijjll " in {
           val runActor = system.actorOf(ExecutionRun.props)
           runActor ! Run("abc", "abd", "iijjkk")
           expectMsg(15.seconds, WebSocketActor.Found("iijjll"))
         }
       "test 18: abc -> abd then hhwwqq -> hhwwrr " in {
        val runActor = system.actorOf(ExecutionRun.props)
        runActor ! Run("abc", "abd", "hhwwqq")
         // Most expected answer: hhwwqq
        expectMsg(15.seconds, WebSocketActor.Found("hhwwqd"))
      }
        "test 19: abc -> abd then lmfgop -> lmfgoq " in {
          val runActor = system.actorOf(ExecutionRun.props)
          runActor ! Run("abc", "abd", "lmfgop")
          // Most expected answer: lmfgoq
          expectMsg(50.seconds, WebSocketActor.Found("lmfgpq"))
        }
        "test 20: abc -> abd then lmnfghopq -> lmnfghpqr " in {
          val runActor = system.actorOf(ExecutionRun.props)
          runActor ! Run("abc", "abd", "lmnfghopq")
          // Most expected answer: lmnfghpqr
          expectMsg(50.seconds, WebSocketActor.Found("lmnfghddd"))
        }*/
      "test 21: aabbcc -> aabbcd then iijjkk -> iijjkl " in {
        val runActor = system.actorOf(ExecutionRun.props)
        runActor ! Run("aabbcc", "aabbcd", "iijjkk")
        // Most expected answer: iijjkl

        expectMsg(50.seconds, WebSocketActor.Found("iijjkd"))
      }
     /* "test 22: abc -> abd then kji -> kjh " in {
        val runActor = system.actorOf(ExecutionRun.props)
        runActor ! Run("abc", "abd", "kji")
        expectMsg(15.seconds, WebSocketActor.Found("kjh"))
      }
      "test 23: abc -> abd then edc -> edb " in {
        val runActor = system.actorOf(ExecutionRun.props)
        runActor ! Run("abc", "abd", "edc")
        expectMsg(15.seconds, WebSocketActor.Found("edb"))
      }
      "test 24: abc -> abd then cba -> dba " in {
        val runActor = system.actorOf(ExecutionRun.props)
        runActor ! Run("abc", "abd", "cba")
        expectMsg(15.seconds, WebSocketActor.Found("dba"))
      }
      "test 25: abc -> abd then mrrjjj -> mrrkkk " in {
        val runActor = system.actorOf(ExecutionRun.props)
        runActor ! Run("abc", "abd", "mrrjjj")
        expectMsg(15.seconds, WebSocketActor.Found("mrrkkk"))
      }
      "test 26: abc -> abd then mrr -> mss " in {
        val runActor = system.actorOf(ExecutionRun.props)
        runActor ! Run("abc", "abd", "mrr")
        expectMsg(15.seconds, WebSocketActor.Found("mss"))
      }
      "test 27: abc -> abd then mmrrrjjjj -> mmrrrkkkk " in {
        val runActor = system.actorOf(ExecutionRun.props)
        runActor ! Run("abc", "abd", "mmrrrjjjj")
        expectMsg(15.seconds, WebSocketActor.Found("mmrrrkkkk"))
      }
      "test 28: abc -> abd then rssttt -> rssuuu " in {
        val runActor = system.actorOf(ExecutionRun.props)
        runActor ! Run("abc", "abd", "rssttt")
        expectMsg(15.seconds, WebSocketActor.Found("rssuuu"))
      }
      "test 29: abc -> abd then xpqdef -> xpqefg " in {
        val runActor = system.actorOf(ExecutionRun.props)
        runActor ! Run("abc", "abd", "xpqdef")
        expectMsg(15.seconds, WebSocketActor.Found("xpqefg"))
      }
      "test 30: abc -> abd then xyz -> xyd " in {
        val runActor = system.actorOf(ExecutionRun.props)
        runActor ! Run("abc", "abd", "xyz")
        expectMsg(15.seconds, WebSocketActor.Found("xyd"))
      }
      "test 31: abc -> qbc then xyz -> qyz " in {
        val runActor = system.actorOf(ExecutionRun.props)
        runActor ! Run("abc", "qbc", "xyz")
        expectMsg(15.seconds, WebSocketActor.Found("qyz"))
      }
      "test 32: rst -> rsu then xyz -> xyu " in {
        val runActor = system.actorOf(ExecutionRun.props)
        runActor ! Run("rst", "rsu", "xyz")
        expectMsg(15.seconds, WebSocketActor.Found("xyu"))
      }
      "test 33: abc -> abd then glz -> hlz " in {
        val runActor = system.actorOf(ExecutionRun.props)
        runActor ! Run("abc", "abd", "glz")
        expectMsg(15.seconds, WebSocketActor.Found("hlz"))
      }*/

  }

}