package datastructures

import org.scalatest.{FlatSpec, FunSuite}

class TreeSpec extends FlatSpec {

  "A Tree" should "count the number of nodes" in {

    val tree = Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(3), Leaf(4)))

    Tree.size(tree)
  }

}
