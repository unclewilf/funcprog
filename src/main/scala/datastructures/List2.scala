package datastructures

import scala.annotation.tailrec

sealed trait List2[+A]

// `List` data type, 2parameterized on a type, `A`
case object Nil extends List2[Nothing]

// A `List` data constructor representing the empty list
case class Cons[+A](head: A, tail: List2[A]) extends List2[A]

// Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`, which may be `Nil` or another `Cons`.

object List2 {
  // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List2[Int]): Int = ints match {
    // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
  }

  def product(ds: List2[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List2[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List2(1, 2, 3, 4, 5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def append[A](a1: List2[A], a2: List2[A]): List2[A] = {
      List2.foldRight(a1, a2)((a:A, b: List2[A]) => Cons(a, b))
  }

//  def foldRight[A, B](as: List2[A], z: B)(f: (A, B) => B): B = {
//    foldLeft(as, z)((a, b) => f(b,a))
//  }

  def foldRight[A, B](as: List2[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List2[Int]) =
    foldRight(ns, 0)((x, y) => x + y)

  def product2(ns: List2[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar


  def tail[A](l: List2[A]): List2[A] = l match {
    case Nil => Nil
    case Cons(h, t) => t
  }

  def setHead[A](l: List2[A], h: A): List2[A] = l match {
    case Cons(_, t) => Cons(h, t)
  }

  @tailrec
  def drop[A](l: List2[A], n: Int): List2[A] = n match {
    case 0 => l
    case _ => drop(List2.tail(l), n - 1)
  }

  def dropWhile[A](l: List2[A], f: A => Boolean): List2[A] = {

    @tailrec
    def loop[A](l: List2[A], r: List2[A], f: A => Boolean): List2[A] =
      l match {
        case Nil => r
        case Cons(h, t) if f(h) => loop(t, r, f)
        case Cons(h, t) => loop(t, List2.append(r, List2(h)), f)
      }

    loop(l, List2(), f)
  }

  def init[A](l: List2[A]): List2[A] = {

    @tailrec
    def loop(l: List2[A], r: List2[A]): List2[A] =
      l match {
        case Cons(h, Nil) => r
        case Cons(h, t) => loop(t, List2.append(r, List2(h)))
      }

    loop(l, List2())
  }

  def length[A](l: List2[A]): Int = {
    List2.foldRight(l,0)((_, l) => l + 1)
  }

  @tailrec
  def foldLeft[A, B](l: List2[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
  }

  def reverse[A](l: List2[A]): List2[A] = {
    List2.foldLeft(l, List2[A]())((b,a) => Cons(a,b))
  }

  def flatten[A](l: List2[List2[A]]): List2[A] = {

    @tailrec
    def loop(l:List2[List2[A]], res: List2[A]): List2[A] = l match {
      case Nil => res
      case Cons(x, xs) => loop(xs, List2.append(res, x))
    }

    loop(l, List2())
  }

  def map[A, B](l: List2[A])(f: A => B): List2[B] = {

    @tailrec
    def loop(l: List2[A], r: List2[B]): List2[B] = l match {
      case Nil => r
      case Cons(x, xs) => loop(xs, Cons(f(x), r))
    }

    loop(reverse(l), List2())
  }
}