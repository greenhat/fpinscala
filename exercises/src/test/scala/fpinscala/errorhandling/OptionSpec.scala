package fpinscala.errorhandling

import fpinscala.datastructures.List
import org.scalatest.WordSpec

class OptionSpec extends WordSpec {

  "a sequence" in {
    assert(Option.sequence(List(Some(1), None)) == None)
    assert(Option.sequence(List(Some(1), Some(2))) == Some(List(1, 2)))
    assert(Option.sequence(List(Some(1), None, Some(2))) == None)
  }
}
