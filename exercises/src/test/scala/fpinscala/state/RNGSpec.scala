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

}
