package errorhandling

import org.scalatest.{Matchers, FlatSpec}

class OptionSpec extends FlatSpec with Matchers {

  "Option" should "map when containing value" in {

    Some(100).map(_+100) should be(Some(200))
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



}
