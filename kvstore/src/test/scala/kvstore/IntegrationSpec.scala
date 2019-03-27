package kvstore

import akka.testkit.TestProbe
import org.scalatest.{FunSuiteLike, Matchers}

trait IntegrationSpec
  extends FunSuiteLike
        with Matchers { this: KVStoreSuite =>
  import Arbiter._

  /*
   * Recommendation: write a test case that verifies proper function of the whole system,
   * then run that with flaky Persistence and/or unreliable communication (injected by
   * using an Arbiter variant that introduces randomly message-dropping forwarder Actors).
   */

  test("IntegrationSpec-case1: Proper replication to new joiners") {
    val arbiter = TestProbe()
    val primary = system.actorOf(Replica.props(arbiter.ref, Persistence.props(flaky = true)), "integration-case1-primary")
    val secondary = system.actorOf(Replica.props(arbiter.ref, Persistence.props(flaky = true)), "integration-case1-secondary")
    val user = session(primary)
    val user_secondary = session(secondary)

    arbiter.expectMsg(Join)
    arbiter.send(primary, JoinedPrimary)

    user.setAcked("k1", "v1")
    user.setAcked("k2", "v2")
    user.setAcked("k3", "v3")
    user.setAcked("k5", "v5")
    arbiter.expectMsg(Join)
    arbiter.send(secondary, JoinedSecondary)
    arbiter.send(primary, Replicas(Set(primary, secondary)))

    user.setAcked("k1", "v2")
    user.removeAcked("k2")

    user_secondary.get("kq") should === (None)
    user_secondary.get("k1") should === (Some("v2"))
    user_secondary.get("k2") should === (None)
    user_secondary.get("k3") should === (Some("v3"))

    user.getAndVerify("k3")
    user.getAndVerify("k1")
    user.getAndVerify("k2")

    user.setAcked("k1", "v9")
    user.removeAcked("k4")
    user.removeAcked("k3")

    user.getAndVerify("k3")
    user.getAndVerify("k1")

    user_secondary.get("k1") should === (Some("v9"))
    user_secondary.get("k3") should === (None)

    user.getAndVerify("k5")
    user_secondary.get("k5") should === (Some("v5"))

  }
  }
