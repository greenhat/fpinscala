package fpinscala.laziness

import org.scalatest.{Matchers, WordSpec}

class StreamSpec extends WordSpec with Matchers {

  "Stream" can {
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

    "takeWhile" in {
      assert(Stream(1, 2, 3).takeWhile(_ < 0).toList == Nil)
      assert(Stream(1, 2, 3).takeWhile(_ < 2).toList == List(1))
      assert(Stream(1, 2, 3).takeWhile(_ < 5).toList == List(1, 2, 3))
    }

    "forAll" should {
      "be correct" in {
        Stream(1, 2, 3).forAll(_ > 1) shouldBe false
        Stream(1, 2, 3).forAll(_ > 0) shouldBe true
      }
      "be lazy" in {
        var effect = 0
        val stream = Stream.cons(1, Stream.cons({ effect += 1; 2 }, Stream(3)))
        stream.forAll(_ > 1) shouldBe false
        effect shouldBe 0
      }
    }

    "headOption" in {
      assert(Stream(1, 2).headOption.contains(1))
      assert(Stream.empty[Int].headOption.isEmpty)
    }

    "map" in {
      assert(Stream(1, 2).map(_.toString).toList == List("1", "2"))
    }

    "filter" in {
      Stream(1, 2).filter(_ % 2 == 0).toList shouldBe List(2)
      Stream(1, 2).filter(_ == 1).toList shouldBe List(1)
    }

    "append" in {
      Stream(1, 2).append(Stream(3, 4)).toList shouldBe List(1, 2, 3, 4)
      Stream(1, 2).append(Stream.empty[Int]).toList shouldBe List(1, 2)
      Stream.empty[Int].append(Stream(3, 4)).toList shouldBe List(3, 4)
    }

    "flatMap" in {
      Stream(1, 2).flatMap(a => Stream(a.toString)).toList shouldBe List("1", "2")
    }

    "from" in {
      Stream.from(2).take(3).toList shouldBe List(2, 3, 4)
    }

    "fibs" in {
      Stream.fibs.take(6).toList shouldBe List(0, 1, 1, 2, 3, 5)
    }

    "unfold terminates" in {
      Stream.unfold(3)(s => if (s > 0) Some((s, s - 1)) else None).toList shouldBe List(3, 2, 1)
    }
  }
}