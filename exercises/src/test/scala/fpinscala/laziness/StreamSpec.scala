package fpinscala.laziness

import org.scalatest.WordSpec

class StreamSpec extends WordSpec {

  "Stream" should {
    "force eval in toList" in {
      assert(Stream(1, 2, 3).toList == List(1, 2, 3))
    }

    "take n" in {
      assert(Stream(1, 2, 3).take(1).toList == List(1))
      assert(Stream(1, 2, 3).take(2).toList == List(1, 2))
      assert(Stream(1, 2, 3).take(4).toList == List(1, 2, 3))
    }
  }
}
