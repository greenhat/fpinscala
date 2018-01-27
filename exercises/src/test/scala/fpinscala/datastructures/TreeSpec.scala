package fpinscala.datastructures

import org.scalatest.WordSpec

class TreeSpec extends WordSpec {

  "tree" when {
    "one leaf" should {
      "have size = 1" in {
        assert(Tree.size(Leaf(1)) == 1)
      }
    }

    "n leaves" should {
      val t = Branch(
        Branch(Leaf(1),
          Branch(Leaf(12),Leaf(2))
        ),
        Branch(Leaf(11),
          Branch(Leaf(21), Leaf(22))
        )
      )

      "have size" in {
        assert(Tree.size(t) == 11)
      }

      "have max" in {
        assert(Tree.maximum(t) == 22)
      }

      "have depth" in {
        assert(Tree.depth(t) == 4)
      }

      "have map" in {
        assert(Tree.maximum(Tree.map[Int, Int](t, _ + 1)) == 23)
      }
    }
  }
}

