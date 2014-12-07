package datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {

  def size[A](a: Tree[A]): Int = a match {

    case Leaf(_) => 1
    case Branch(l,r) => 1 + size(l) + size(r)
  }

  def maxInt(a: Tree[Int]): Int = a match {

    case Leaf(v) => v
    case Branch(l,r) => maxInt(l).max(maxInt(r))
  }

}