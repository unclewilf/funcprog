package datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {

  def size[A](t: Tree[A]): Int = t match {

    case Leaf(_) => 1
    case Branch(l,r) => (size(l) + size(r)) + 1
  }

  def maximum(t: Tree[Int]): Int = t match {

    case Leaf(v) => v
    case Branch(l,r) => maximum(l) max maximum(r)
  }

  def depth[A](t: Tree[A]): Int = t match {

    case Leaf(v) => 0
    case Branch(l,r) => (depth(l) max depth(r)) + 1
  }

  def map[A,B](t: Tree[A])(f: A => B): Tree[B] = t match {

    case Leaf(v) => Leaf(f(v))
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
  }

}