package gettingstarted

import scala.annotation.tailrec

object Sorted {

  @tailrec
  def isSorted[A] (as: Array[A], ordered: (A,A) => Boolean): Boolean = as match {

    case Array() | Array(_) => true
    case Array(a, b, _*) if !ordered(a, b) => false
    case _ => isSorted(as.tail, ordered)
  }
}
