package datastructures

import org.scalatest.prop.PropertyChecks
import org.scalatest.prop.Tables.Table
import org.scalatest.{FlatSpec, Matchers}

class TreeSpec extends FlatSpec with Matchers {

  val fourLeaf = Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(3), Leaf(4)))

  "A Tree" should "count the number of nodes" in {
    Tree.size(fourLeaf) should be(7)
  }

  it should "know the maximum for tree of ints" in new {
    Tree.maximum(fourLeaf) should be(4)
  }

  it should "know the depth" in new PropertyChecks {

    val combos =
      Table(
        ("tree", "depth"),
        (fourLeaf, 2),
        (Branch(Leaf(1), fourLeaf), 3),
        (Branch(fourLeaf, Leaf(1)), 3)
      )

    forAll(combos) { (l: Tree[Int], depth: Int) =>
      Tree.depth(l) should be(depth)
    }
  }

  it should "map" in new {

    val expected = Branch(Branch(Leaf(2), Leaf(3)), Branch(Leaf(4), Leaf(5)))

    Tree.map(fourLeaf)(_ + 1) should be(expected)
  }
}
