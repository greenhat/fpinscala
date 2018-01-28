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

    "take n is lazy" in {
      var effect = 0
      val stream = Stream.cons({ effect += 1; 1 }, Stream(2, 3))
      assert(effect == 0)
      val takenStream = stream.take(1)
      assert(effect == 0)
      assert(takenStream.toList == List(1))
      assert(effect == 1)
    }

    "drop n" in {
      assert(Stream(1, 2, 3).drop(1).toList == List(2, 3))
      assert(Stream(1, 2, 3).drop(2).toList == List(3))
      assert(Stream(1, 2, 3).drop(4).toList == Nil)
    }

    "drop n is lazy" in {
      var effect1 = 0
      var effect2 = 0
      val stream = Stream.cons({ effect1 += 1; 1 }, Stream.cons({ effect2 += 1; 2 }, Stream(3)))
      assert(effect1 == 0)
      assert(effect2 == 0)
      val droppedStream = stream.drop(1)
      assert(effect1 == 0)
      assert(effect2 == 0)
      assert(droppedStream.toList == List(2, 3))
      assert(effect1 == 0)
      assert(effect2 == 1)
    }
  }
}
