package datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {

  def size[A](a: Tree[A]): Int = a match {

    case Leaf(_) => 1
    case Branch(l,r) => (size(l) + size(r)) + 1
  }

  def maximum(a: Tree[Int]): Int = a match {

    case Leaf(v) => v
    case Branch(l,r) => maximum(l) max maximum(r)
  }

  def depth(a: Tree[Int]): Int = a match {

    case Leaf(v) => 0
    case Branch(l,r) => (depth(l) max depth(r)) + 1
  }

}