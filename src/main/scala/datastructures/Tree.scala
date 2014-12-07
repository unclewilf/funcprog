package datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {

  def size[A](t: Tree[A]): Int = {
    fold(t)(_ => 1)((l: Int,r: Int) => l + r + 1)
  }

  def maximum(t: Tree[Int]): Int = {
    fold(t)(a => a)(_ max _)
  }

  def depth[A](t: Tree[A]): Int = {
    fold(t)(_ => 0)((l: Int,r: Int) => 1 + (l max r))
  }

  def map[A,B](t: Tree[A])(f: A => B): Tree[B] = {
    fold(t)(a => Leaf(f(a)): Tree[B])(Branch(_,_))
  }

  def fold[A,B](t: Tree[A])(le: A => B)(b: (B, B) => B): B = t match {

    case Leaf(v) => le(v)
    case Branch(l ,r) => b(fold(l)(le)(b), fold(r)(le)(b))
  }

}