package fpinscala.laziness

import org.scalatest.WordSpec

class StreamSpec extends WordSpec {

  "Stream" should {
    "force eval in toList" in {
      assert(Stream(1, 2, 3).toList == List(1, 2, 3))
    }
  }
}
