package datastructures

import org.scalatest.{Matchers, FlatSpec}

class ListSpec extends FlatSpec with Matchers {

  "Pattern matching problem" should "return 3" in {

    List2.x should be(3)
  }

}
