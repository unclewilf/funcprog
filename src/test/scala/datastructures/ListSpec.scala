package datastructures

import org.scalatest.prop.PropertyChecks
import org.scalatest.prop.Tables.Table
import org.scalatest.{PropSpecLike, Matchers, FlatSpec}

class ListSpec extends FlatSpec with Matchers {

  "list" should "return 3 for pattern patch example" in {

    List2.x should be(3)
  }

  it should "return tail" in new PropertyChecks {

    val validCombos =
      Table(
        ("head", "tail"),
        (List2(1), List2()),
        (List2(1,2), List2(2)),
        (List2(1,2,3), List2(2,3))
      )

    forAll(validCombos) { (full: List2[Int], tail: List2[Int]) =>
      List2.tail(full) should be(tail)
    }
  }

  it should "set head" in new PropertyChecks {

    val validCombos =
      Table(
        ("list", "head", "expected"),
        (List2(1), 2, List2(2)),
        (List2(1,2), 3, List2(3,2)),
        (List2(1,2,3), 4, List2(4,2,3))
      )

    forAll(validCombos) { (l: List2[Int], h: Int, expected: List2[Int]) =>
      List2.setHead(l, h) should be(expected)
    }
  }

  it should "drop" in new PropertyChecks {

    val validCombos =
      Table(
        ("list", "drop", "expected"),
        (List2(1), 1, List2()),
        (List2(1,2), 1, List2(2)),
        (List2(1,2), 2, List2()),
        (List2(1,2,3,4), 3, List2(4))
      )

    forAll(validCombos) { (l: List2[Int], d: Int, expected: List2[Int]) =>
      List2.drop(l, d) should be(expected)
    }
  }

  it should "drop while under 5" in {

    val list = List2(1,2,3,4,5,6,7)

    val res = List2.dropWhile(list, (i: Int) => i < 5)

    res should be(List2(5,6,7))
  }

  it should "init" in {

    val list = List2(1,2,3,4)

    val res = List2.init(list)

    res should be(List2(1,2,3))
  }

  it should "calculate length using foldright" in new PropertyChecks {

    val validCombos =
      Table(
        ("list", "size"),
        (List2(1), 1),
        (List2(1,2), 2),
        (List2(1,2,3), 3),
        (List2(1,2,3,4), 4)
      )

    forAll(validCombos) { (l: List2[Int], size: Int) =>
      List2.length(l) should be(size)
    }
  }

  it should "fold left sum" in new PropertyChecks {

    val validCombos =
      Table(
        ("list", "sum"),
        (List2(1), 1),
        (List2(1,2), 3),
        (List2(1,2,3), 6),
        (List2(1,2,3,4), 10)
      )

    forAll(validCombos) { (l: List2[Int], sum: Int) =>
      List2.foldLeft(l,0)(_+_) should be(sum)
    }
  }

  it should "fold left product" in new PropertyChecks {

    val validCombos =
      Table(
        ("list", "product"),
        (List2(1), 1),
        (List2(1,2), 2),
        (List2(1,2,3), 6),
        (List2(1,2,3,4), 24)
      )

    forAll(validCombos) { (l: List2[Int], product: Int) =>
      List2.foldLeft(l,1)(_*_) should be(product)
    }
  }

  it should "fold left size" in new PropertyChecks {

    val validCombos =
      Table(
        ("list", "size"),
        (List2(1), 1),
        (List2(1,2), 2),
        (List2(1,2,3), 3),
        (List2(1,2,3,4), 4)
      )

    forAll(validCombos) { (l: List2[Int], size: Int) =>
      List2.foldLeft(l,0)((b, _) => b + 1) should be(size)
    }
  }

  it should "fold a list in reverse" in {

    val li = List2(1,2,3)

    val res = List2.reverse(li)

    res should be(List2(3,2,1))
  }

  it should "append" in new PropertyChecks {

    val validCombos =
      Table(
        ("lista", "listb", "expected"),
        (List2(1), List2(2), List2(1,2)),
        (List2(1,2), List2(3), List2(1,2,3)),
        (List2(1,2,3), List2(4,5), List2(1,2,3,4,5))
      )

    forAll(validCombos) { (l1: List2[Int], l2: List2[Int], expected: List2[Int]) =>
      List2.append(l1, l2) should be(expected)
    }
  }

  it should "flatten" in {

    val megalist = List2(List2(1), List2(2))

    List2.flatten(megalist) should be(List2(1,2))
  }

  it should "map to increase each element by 1" in {

    val megalist = List2(1,2,3,4,5)

    List2.map(megalist)(_+1) should be(List2(2,3,4,5,6))
  }

  it should "map double to string" in {

    val megalist = List2(1.0,2.1,3.2)

    List2.map(megalist)(_.toString) should be(List2("1.0","2.1","3.2"))
  }

  it should "filter while number is even" in {

    val list = List2(1,2,3,4,5,6,7)

    val res = List2.filter(list)(_ % 2 == 0)

    res should be(List2(2,4,6))
  }
}
