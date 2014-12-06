package intro

import scala.annotation.tailrec

object Sorted {

  @tailrec
  def isSorted[A] (as: Array[A], ordered: (A,A) => Boolean): Boolean = {

    if (as.length < 2) {
      true
    } else if (!ordered(as(0), as(1))){
      false
    } else {
      isSorted(as.tail, ordered)
    }
  }
}
