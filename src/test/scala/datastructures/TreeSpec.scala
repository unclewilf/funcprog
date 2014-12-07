package datastructures

import org.scalatest.{FlatSpec, Matchers}

class TreeSpec extends FlatSpec with Matchers {

  "A Tree" should "count the number of nodes" in {

    val tree = Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(3), Leaf(4)))

    Tree.size(tree) should be(7)
  }

  it should "know the maximum for tree of ints" in {

    val tree = Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(3), Leaf(4)))

    Tree.maxInt(tree) should be(4)
  }

}
