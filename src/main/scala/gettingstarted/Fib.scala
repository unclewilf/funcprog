package gettingstarted

import scala.annotation.tailrec

object Fib {

  def fib(i: Int) : Int = {

    @tailrec
    def loop(i:Int, pos:Int, prev: Int, current: Int): Int = {

      if (pos == i) {
        current
      } else {
        loop(i, pos+1, current, prev+current)
      }
    }

    if (i == 0) 0 else loop(i, 1, 0, 1)
  }
}
