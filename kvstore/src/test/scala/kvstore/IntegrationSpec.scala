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
    val third = system.actorOf(Replica.props(arbiter.ref, Persistence.props(flaky = true)), "integration-case1-third")
    val user = session(primary)
    val user_secondary = session(secondary)
    val user_third = session(third)

    arbiter.expectMsg(Join)
    arbiter.send(primary, JoinedPrimary)

    user.setAcked("k1", "v1")
    user.setAcked("k2", "v2")
    user.setAcked("k3", "v3")
    user.setAcked("k5", "v5")
    user.setAcked("k6", "v6")
    user.removeAcked("k6")
    arbiter.expectMsg(Join)
    arbiter.send(secondary, JoinedSecondary)
    arbiter.send(primary, Replicas(Set(primary, secondary)))

    user.setAcked("k1", "v2")
    user.removeAcked("k2")

    user_secondary.get("kq") should === (None)
    user_secondary.get("k1") should === (Some("v2"))
    user_secondary.get("k2") should === (None)
    user_secondary.get("k6") should === (None)
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

    arbiter.expectMsg(Join)
    arbiter.send(third, JoinedSecondary)
    arbiter.send(primary, Replicas(Set(primary, secondary, third)))

    user.setAcked("k9123", "asc")

    user_third.get("k5") should === (Some("v5"))
    user_third.get("k3") should === (None)

  }

  test("IntegrationSpec-case2: Match the edx") {
    val arbiter = TestProbe()
    val primary = system.actorOf(Replica.props(arbiter.ref, Persistence.props(flaky = true)), "integration-case2-primary")
    val secondary = system.actorOf(Replica.props(arbiter.ref, Persistence.props(flaky = true)), "integration-case2-secondary1")
    val third = system.actorOf(Replica.props(arbiter.ref, Persistence.props(flaky = true)), "integration-case2-secondary2")
    val user = session(primary)
    val user_secondary = session(secondary)
    val user_third = session(third)

    arbiter.expectMsg(Join)
    arbiter.send(primary, JoinedPrimary)

    user.get("k1") should === (None)
    user.setAcked("k1", "v1")
    user.get("k1") should === (Some("v1"))
    user.setAcked("k1", "v11")
    user.get("k2") should === (None)
    user.setAcked("k2", "v2")
    user.get("k2") should === (Some("v2"))

    arbiter.expectMsg(Join)
    arbiter.send(secondary, JoinedSecondary)
    arbiter.send(primary, Replicas(Set(primary, secondary)))

    arbiter.expectMsg(Join)
    arbiter.send(third, JoinedSecondary)
    arbiter.send(primary, Replicas(Set(primary, secondary, third)))

    user_secondary.get("k1") should === (None)
    user_secondary.get("k2") should === (None)
    user_third.get("k2") should === (None)
    user.removeAcked("k1")
    user.get("k1") should === (None)
    user_secondary.get("k2") should === (Some("v2"))

    user_third.get("k2") should === (Some("v2"))

    //arbiter.send(primary, Replicas(Set(primary, secondary)))

    user.removeAcked("k2")
    user_secondary.get("k2") should === (None)


  }
  }
