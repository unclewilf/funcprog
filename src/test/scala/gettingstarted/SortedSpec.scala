package gettingstarted

import org.scalatest.{Matchers, FlatSpec}

class SortedSpec extends FlatSpec with Matchers {

  "Sorted" should "return true with sorted integers" in {

    val sorted = Array(1,2,3,4)

    val res = Sorted.isSorted(sorted, (a: Int,b: Int) => a < b)

    res shouldBe true
  }

  it should "return false with unsorted integers" in {

    val unsorted = Array(2,1,4,3)

    val res = Sorted.isSorted(unsorted, (a: Int,b: Int) => a < b)

    res shouldBe false
  }

  it should "return true with sorted strings" in {

    val unsorted = Array("apple","orange","pear")

    val res = Sorted.isSorted(unsorted, (a: String, b: String) => a < b)

    res shouldBe true
  }

  it should "return false with unsorted strings" in {

    val unsorted = Array("apple","orange","pear", "banana")

    val res = Sorted.isSorted(unsorted, (a: String, b: String) => a < b)

    res shouldBe false
  }
}
