package intro

import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}

class FibSpec extends PropSpec with Matchers with PropertyChecks {

  property("Fraction constructor throws IAE on bad data.") {

    val validCombos =
      Table(
        ("n","d"),
        (0,0),
        (1,1),
        (2,1),
        (3,2),
        (4,3),
        (5,5),
        (6,8)
      )

    forAll (validCombos) { (n: Int, d: Int) =>
      Fib.fib(n) should be (d)
    }
  }
}

