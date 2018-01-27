package fpinscala.datastructures

import org.scalatest.WordSpec

class ListSpec extends WordSpec {

  "sum" when {
    "empty list" should {
      "be zero" in {
        assert(List.sum(Nil) == 0)
      }
    }
  }

  "product" when {
    "zero start list" should {
      "be a zero" in {
        assert(List.product(List(0, 1)) == 0)
      }
    }
    "list containing a zero" should {
      "be a zero" in {
        assert(List.product(List(4, 0, 5)) == 0)
      }
    }
    "list containing no zero" should {
      "be a product" in {
        assert(List.product(List(4, 2)) == 8)
      }
    }
  }

  "tail" when {
    "empty list" should {
      "be an empty list" in {
        assert(List.tail(Nil) == Nil)
      }
    }
    "one item list" should {
      "be empty list" in {
        assert(List.tail(List(1)) == Nil)
      }
    }
    "multi item list" should {
      "be tail" in {
        assert(List.tail(List(1, 5, 8)) == List(5, 8))
      }
    }
  }

  "setHead" when {
    "empty list" should {
      "be an empty list" in {
        assert(List.setHead(Nil, 1) == Nil)
      }
    }
    "non-empty list" should {
      "be a list with new head" in {
        assert(List.setHead(List(1, 2, 3), 2) == List(2, 2, 3))
      }
    }
  }

  "drop" when {
    "empty list" should {
      "be an empty list" in {
        assert(List.drop(Nil, 1) == Nil)
      }
    }
    "non-empty list" should {
      "be a new list" in {
        assert(List.drop(List(1), 1) == Nil)
        assert(List.drop(List(1, 2), 1) == List(2))
      }
    }
  }

  "dropWhile" when {
    "empty list" should {
      "be an empty list" in {
        assert(List.dropWhile[Int](Nil, _ == 0) == Nil)
      }
    }
    "non-empty list" should {
      "be the same list if predicate is false for the head" in {
        assert(List.dropWhile[Int](List(1), _ == 0) == List(1))
      }
      "be a new list if predicate is true for the head" in {
        assert(List.dropWhile[Int](List(1, 2, 3), _ == 1) == List(2, 3))
        assert(List.dropWhile[Int](List(1, 1, 3), _ == 1) == List(3))
      }
      "be the same list if predicate is false for the head, but true for other item" in {
        assert(List.dropWhile[Int](List(3, 2, 1), _ == 1) == List(3, 2, 1))
      }
    }
  }

  "initDropLast" when {
    "empty list" should {
      "be an empty list" in {
        assert(List.initDropLast(Nil) == Nil)
      }
    }
    "non-empty list" should {
      "be empty list if has one item" in {
        assert(List.initDropLast(List(1)) == Nil)
      }
      "be non-empty list if has more than one item" in {
        assert(List.initDropLast(List(1, 2)) == List(1))
      }
    }
  }

  "foldRight" when {
    "used with Cons" should {
      "produce a new list" in {
        val r =  List.foldRight(List(1,2,3), Nil:List[Int])(Cons(_,_))
        assert(r == List(1, 2, 3))
      }
    }
  }

  "foldLeft" when {
    "used with Cons" should {
      "produce a new list" in {
        val r =  List.foldLeft(List(1,2,3), Nil:List[Int])((z, a) => Cons(a, z))
        assert(r == List(3, 2, 1))
      }
    }
  }

  "length" when {
    "empty list" should {
      "be zero" in {
        assert(List.length(Nil) == 0)
      }
    }
    "non-empty list" should {
      "be a number of items" in {
        assert(List.length(List(1)) == 1)
        assert(List.length(List(1, 1)) == 2)
      }
    }
  }

  "reverse" when {
    "non-empty list" should {
      "be reversed list" in {
        assert(List.reverse(List(1, 2, 3)) == List(3, 2, 1))
      }
    }
  }

  "foldLeftWithFoldRight" when {
    "used with Cons" should {
      "produce a new list" in {
        val r =  List.foldLeftViaFoldRight(List(1,2,3), Nil:List[Int])((z, a) => Cons(a, z))
        assert(r == List(3, 2, 1))
      }
    }
  }

  "foldRightWithFoldLeft" when {
    "used with Cons" should {
      "produce a new list" in {
        val r =  List.foldRightViaFoldLeft(List(1,2,3), Nil:List[Int])((a, z) => Cons(a, z))
        assert(r == List(1, 2, 3))
      }
    }
  }

  "flatten" when {
    "non-empty list of lists" should {
      "be a list" in {
        assert(List.flatten(List(List(1, 2), List(3,4))) == List(1, 2, 3, 4))
      }
    }
  }

  "map" when {
    "non-empty list" should {
      "be a new list with transformed elements" in {
        assert(List.map(List(1, 2, 3))(_.toString()) == List("1", "2", "3"))
      }
    }
  }

  "filter" when {
    "non-empty list" should {
      "be a list without failed items" in {
        assert(List.filter(List(1, 2, 3))(_ > 2) == List(3))
      }
    }
  }

  "flatMap" when {
    "non-empty list" should {
      "be a new list" in {
        assert(List.flatMap(List(1,2,3))(i => List(i,i)) == List(1,1,2,2,3,3))
      }
    }
  }

  "zipWith" when {
    "non-empty list" should {
      "be a new zipped list" in {
        assert(List.zipWith(List(1, 2), List(1, 2))(_ + _) == List(2, 4))
      }
    }
  }

  "hasSubSequence" when {
    "both are non-empty and is a sub list" should {
      "true" in {
        assert(List.hasSubsequence(List(1, 2, 3, 4), List(1, 2)))
        assert(List.hasSubsequence(List(1, 2, 3, 4), List(2, 3)))
        assert(List.hasSubsequence(List(1, 2, 3, 4), List(3, 4)))
        assert(List.hasSubsequence(List(1, 2, 3, 4), List(1)))
        assert(List.hasSubsequence(List(1, 2, 3, 4), List(2)))
        assert(List.hasSubsequence(List(1, 2, 3, 4), List(4)))
      }
      "false" in {
        assert(!List.hasSubsequence(List(1, 2, 3, 4), List(5)))
        assert(!List.hasSubsequence(List(1, 2, 3, 4), List(4, 5)))
        assert(!List.hasSubsequence(List(Nil), List(4, 5)))
      }
    }
  }
}
