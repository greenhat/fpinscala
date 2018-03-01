package fpinscala.state

import org.scalacheck.Gen
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{Matchers, PropSpec}
import RNG._

class RNGSpec extends PropSpec
  with Matchers
  with GeneratorDrivenPropertyChecks {

  private val longGen = Gen.choose(Long.MinValue, Long.MaxValue)

  property("nonNegativeInt") {
    forAll(longGen) { l: Long =>
      nonNegativeInt(Simple(l))._1 should (be >= 0 and be < Int.MaxValue)
    }
  }

  property("double") {
    forAll(longGen) { l: Long =>
      double(Simple(l))._1 should (be >= 0.0 and be < 1.0)
    }
  }

  property("map2") {
    forAll(longGen) { l: Long =>
      map2(int, int)(_.toLong + _.toLong)(Simple(l))._1 should (be >= Long.MinValue and be <= Long.MaxValue)
    }
  }

  property("sequence") {
    forAll(longGen) { l: Long =>
      sequence(List.fill(10)(int))(Simple(l))._1.length shouldBe 10
    }
  }

  property("flatMap") {
    forAll(longGen) { l: Long =>
      flatMap(int)(a => unit(a))(Simple(l))._1 should (be >= Int.MinValue and be <= Int.MaxValue)

    }
  }

  property("nonNegativeLessThan") {
    forAll(longGen) { l: Long =>
      nonNegativeLessThan(10)(Simple(l))._1 should (be >= 0 and be < 10)

    }
  }
}
