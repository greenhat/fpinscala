package fpinscala.state

import org.scalatest.{FlatSpec, Matchers}

class CandyDispenserSpec extends FlatSpec
  with Matchers {

  "Locked machine" should "unlock on inserted coin if any candy left" in {
    State.simulateMachine(List(
      Coin
    )).run(
      Machine(locked = true, candies = 1, coins = 1)
    ) shouldEqual ((1, 2), Machine(locked = false, candies = 1, coins = 2))
  }

  it should "do nothing on knob turn" in {
    State.simulateMachine(List(
      Turn
    )).run(
      Machine(locked = true, candies = 1, coins = 1)
    ) shouldEqual ((1, 1), Machine(locked = true, candies = 1, coins = 1))
  }

  it should "do nothing if no candies are left" in {
    State.simulateMachine(List(
      Coin,
      Turn
    )).run(
      Machine(locked = true, candies = 0, coins = 0)
    ) shouldEqual ((0, 0), Machine(locked = true, candies = 0, coins = 0))
  }

  "Unlocked machine" should "dispense a candy and become locked on knob turn" in {
    State.simulateMachine(List(
      Turn
    )).run(
      Machine(locked = false, candies = 1, coins = 1)
    ) shouldEqual ((0, 1), Machine(locked = true, candies = 0, coins = 1))
  }

  it should "do nothing on coin insert" in {
    State.simulateMachine(List(
      Coin
    )).run(
      Machine(locked = false, candies = 1, coins = 1)
    ) shouldEqual ((1, 1), Machine(locked = false, candies = 1, coins = 1))
  }

  "Locked machine" should "process scenario 1" in {
    State.simulateMachine(List(
      Coin,
      Turn
    )).run(
      Machine(locked = true, candies = 1, coins = 1)
    ) shouldEqual ((0, 2), Machine(locked = true, candies = 0, coins = 2))
  }
}
