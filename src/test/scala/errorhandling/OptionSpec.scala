package errorhandling

import org.scalatest.{Matchers, FlatSpec}

class OptionSpec extends FlatSpec with Matchers {

  def add100(i: Int): Option[Int] = {
    Some(i + 100)
  }

  "Option" should "map when containing value" in {
    Some(100).map(add100) should be(Some(Some(200)))
  }

  it should "map none when empty" in {

    None.map(_ => 200) should be(None)
  }

  it should "return present result for getorelse" in {

    Some("hello").getOrElse() should be("hello")
  }

  it should "return replacement result for getorelse" in {

    None.getOrElse("error") should be("error")
  }

  it should "flatmap an option" in {

    Some(100).flatMap(add100) should be(Some(200))
  }




}
