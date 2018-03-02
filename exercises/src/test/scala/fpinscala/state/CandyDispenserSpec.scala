package fpinscala.state

import org.scalatest.{FlatSpec, Matchers}

class CandyDispenserSpec extends FlatSpec
  with Matchers {

  "Locked machine" should "unlock on inserted coin if any candy left" in {
    State.simulateMachine(List(
      Coin
    )).run(
      Machine(locked = true, candies = 1, coins = 1)
    ) shouldEqual ((0, 0), Machine(locked = false, candies = 1, coins = 2))
  }

}
