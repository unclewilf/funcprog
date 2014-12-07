package datastructures

import org.scalatest.prop.PropertyChecks
import org.scalatest.prop.Tables.Table
import org.scalatest.{PropSpecLike, Matchers, FlatSpec}

class ListSpec extends FlatSpec with Matchers {

  "list" should "return 3 for pattern patch example" in {

    List.x should be(3)
  }

  it should "return tail" in new PropertyChecks {

    val validCombos =
      Table(
        ("head", "tail"),
        (List(1), List()),
        (List(1,2), List(2)),
        (List(1,2,3), List(2,3))
      )

    forAll(validCombos) { (full: List[Int], tail: List[Int]) =>
      List.tail(full) should be(tail)
    }
  }

  it should "set head" in new PropertyChecks {

    val validCombos =
      Table(
        ("list", "head", "expected"),
        (List(1), 2, List(2)),
        (List(1,2), 3, List(3,2)),
        (List(1,2,3), 4, List(4,2,3))
      )

    forAll(validCombos) { (l: List[Int], h: Int, expected: List[Int]) =>
      List.setHead(l, h) should be(expected)
    }
  }

  it should "drop" in new PropertyChecks {

    val validCombos =
      Table(
        ("list", "drop", "expected"),
        (List(1), 1, List()),
        (List(1,2), 1, List(2)),
        (List(1,2), 2, List()),
        (List(1,2,3,4), 3, List(4))
      )

    forAll(validCombos) { (l: List[Int], d: Int, expected: List[Int]) =>
      List.drop(l, d) should be(expected)
    }
  }

  it should "drop while under 5" in {

    val list = List(1,2,3,4,5,6,7)

    val res = List.dropWhile(list, (i: Int) => i < 5)

    res should be(List(5,6,7))
  }

  it should "init" in {

    val list = List(1,2,3,4)

    val res = List.init(list)

    res should be(List(1,2,3))
  }

  it should "calculate length using foldright" in new PropertyChecks {

    val validCombos =
      Table(
        ("list", "size"),
        (List(1), 1),
        (List(1,2), 2),
        (List(1,2,3), 3),
        (List(1,2,3,4), 4)
      )

    forAll(validCombos) { (l: List[Int], size: Int) =>
      List.length(l) should be(size)
    }
  }

  it should "fold left sum" in new PropertyChecks {

    val validCombos =
      Table(
        ("list", "sum"),
        (List(1), 1),
        (List(1,2), 3),
        (List(1,2,3), 6),
        (List(1,2,3,4), 10)
      )

    forAll(validCombos) { (l: List[Int], sum: Int) =>
      List.foldLeft(l,0)(_+_) should be(sum)
    }
  }

  it should "fold left product" in new PropertyChecks {

    val validCombos =
      Table(
        ("list", "product"),
        (List(1), 1),
        (List(1,2), 2),
        (List(1,2,3), 6),
        (List(1,2,3,4), 24)
      )

    forAll(validCombos) { (l: List[Int], product: Int) =>
      List.foldLeft(l,1)(_*_) should be(product)
    }
  }

  it should "fold left size" in new PropertyChecks {

    val validCombos =
      Table(
        ("list", "size"),
        (List(1), 1),
        (List(1,2), 2),
        (List(1,2,3), 3),
        (List(1,2,3,4), 4)
      )

    forAll(validCombos) { (l: List[Int], size: Int) =>
      List.foldLeft(l,0)((b, _) => b + 1) should be(size)
    }
  }

  it should "fold a list in reverse" in {

    val li = List(1,2,3)

    val res = List.reverse(li)

    res should be(List(3,2,1))
  }

  it should "append" in new PropertyChecks {

    val validCombos =
      Table(
        ("lista", "listb", "expected"),
        (List(1), List(2), List(1,2)),
        (List(1,2), List(3), List(1,2,3)),
        (List(1,2,3), List(4,5), List(1,2,3,4,5))
      )

    forAll(validCombos) { (l1: List[Int], l2: List[Int], expected: List[Int]) =>
      List.append(l1, l2) should be(expected)
    }
  }

  it should "flatten" in {

    val megalist = List(List(1), List(2))

    List.flatten(megalist) should be(List(1,2))
  }

  it should "map to increase each element by 1" in {

    val megalist = List(1,2,3,4,5)

    List.map(megalist)(_+1) should be(List(2,3,4,5,6))
  }

  it should "map double to string" in {

    val megalist = List(1.0,2.1,3.2)

    List.map(megalist)(_.toString) should be(List("1.0","2.1","3.2"))
  }

  it should "filter while number is even" in {

    val list = List(1,2,3,4,5,6,7)

    val res = List.filter(list)(_ % 2 == 0)

    res should be(List(2,4,6))
  }

  it should "flatmap" in {

    val list = List(1,2,3)

    val res = List.flatMap(list)(i => List(i,i))

    res should be(List(1, 1, 2, 2, 3, 3))
  }

  it should "filter while number is even via flatmap" in {

    val list = List(1,2,3,4,5,6,7)

    val res = List.filterViaFlatmap(list)(_ % 2 == 0)

    res should be(List(2,4,6))
  }

  it should "zip some lists by adding numbers together" in {

    val list1 = List(1,2,3)
    val list2 = List(4,5,6)

    val res = List.zipAdd(list1, list2)

    res should be(List(5,7,9))
  }

  it should "know about subsequences" in new PropertyChecks {

    val seq = List(1,2,3,4,5,6)

    val combos =
      Table(
        ("list", "contained"),
        (List(1), true),
        (List(4), true),
        (List(2,3), true),
        (List(2,3,4), true),
        (List(1,4), false)
      )

    forAll(combos) { (l: List[Int], contained: Boolean) =>
      List.hasSubsequence(seq, l) should be(contained)
    }
  }
}
