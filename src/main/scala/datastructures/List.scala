package datastructures

import scala.annotation.tailrec

sealed trait List[+A]

// `List` data type, 2parameterized on a type, `A`
case object Nil extends List[Nothing]

// A `List` data constructor representing the empty list
case class Cons[+A](head: A, tail: List[A]) extends List[A]

// Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`, which may be `Nil` or another `Cons`.

object List {
  // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match {
    // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1, 2, 3, 4, 5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] = {
      List.foldRight(a1, a2)((a:A, b: List[A]) => Cons(a, b))
  }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = {
    foldLeft(reverse(as), z)((a, b) => f(b,a))
  }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x, y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar


  def tail[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(h, t) => t
  }

  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Cons(_, t) => Cons(h, t)
  }

  @tailrec
  def drop[A](l: List[A], n: Int): List[A] = n match {

    case 0 => l
    case _ => drop(List.tail(l), n - 1)
  }

  @tailrec
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {

    case Cons(x, xs) if f(x) => dropWhile(xs, f)
    case _ => l
  }

  def init[A](l: List[A]): List[A] = {

    @tailrec
    def loop(l: List[A], r: List[A]): List[A] =
      l match {
        case Cons(h, Nil) => r
        case Cons(h, t) => loop(t, List.append(r, List(h)))
      }

    loop(l, List())
  }

  def length[A](l: List[A]): Int = {
    List.foldRight(l,0)((_, l) => l + 1)
  }

  @tailrec
  def foldLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
  }

  def reverse[A](l: List[A]): List[A] = {
    List.foldLeft(l, List[A]())((b,a) => Cons(a,b))
  }

  def flatten[A](l: List[List[A]]): List[A] = {

    @tailrec
    def loop(l:List[List[A]], res: List[A]): List[A] = l match {
      case Nil => res
      case Cons(x, xs) => loop(xs, List.append(res, x))
    }

    loop(l, List())
  }

  def map[A, B](l: List[A])(f: A => B): List[B] = {

    @tailrec
    def loop(l: List[A], r: List[B]): List[B] = l match {
      case Nil => r
      case Cons(x, xs) => loop(xs, Cons(f(x), r))
    }

    loop(reverse(l), List())
  }

  def filter[A](l: List[A])(f: A => Boolean): List[A] = {

    @tailrec
    def loop[A](l: List[A], r: List[A], f: A => Boolean): List[A] =
      l match {
        case Nil => r
        case Cons(h, t) if f(h) => loop(t, List.append(r, List(h)), f)
        case Cons(h, t) => loop(t, r, f)
      }

    loop(l, List(), f)
  }

  def flatMap[A,B](l: List[A])(f: A => List[B]): List[B] = {
    flatten(map(l)(f))
  }

  def filterViaFlatmap[A](l: List[A])(f: A => Boolean): List[A] = {

    flatMap(l)(x => if (f(x)) List(x) else List())
  }

  def head[A](l: List[A]): A = l match {
    case Cons(h, t) => h
  }

  def zipWith[A](l1: List[A], l2: List[A])(f: (A, A) => A): List[A] = {

    @tailrec
    def loop(l1: List[A], l2: List[A], res: List[A]): List[A] = l1 match {
      case Nil => res
      case Cons(x, xs) => loop(tail(l1), tail(l2), Cons(f(x, head(l2)), res))
    }

    loop(reverse(l1), reverse(l2), Nil)
  }

  def zipAdd(l1: List[Int], l2: List[Int]): List[Int] = {

    zipWith(l1, l2)(_ + _)
  }

  def hasSubsequence[A](l: List[A], sub: List[A]): Boolean = {

    def allSeq(l: List[A], all: List[List[A]]): List[List[A]] = l match {
      case Cons(x, Nil) =>
        all
      case Cons(x, xs) =>
        val f = xs
        val r = reverse(drop(reverse(Cons(x, xs)), 1))
        append(allSeq(f, Cons(f, all)), allSeq(r, Cons(r, all)))
    }

    val all = allSeq(l, Nil)
//    map(all)(println(_))

    val filtered = filter(all)(_ == sub)

//    println("done")
    length(filtered) > 0
  }
}