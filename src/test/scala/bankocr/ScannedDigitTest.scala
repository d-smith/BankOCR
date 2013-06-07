package bankocr

import org.scalatest.FunSuite


class ScannedDigitTest extends FunSuite {
  import ScannedDigit._
  test("lines SUS PUP PUP comprise a digit") {
    val digit = new ScannedDigit(List(SUS,PUP,PUP))
    assert(digit.isDigit)
  }

  test("Valid top and middle and unknown bottom is not a digit") {
    val digit = new ScannedDigit(List(SUS,PUP, "___"))
    assert(digit.isDigit === false)
  }

  test("foo and bar differ by three characters") {
    val diff = ScannedDigit.lineCharDiffs("foo","bar")
    assert(diff === 3)
  }

  test("foo and fob differ by one character") {
    val diff = ScannedDigit.lineCharDiffs("foo","fob")
    assert(diff === 1)
  }

  test("foo and foo differ by no characters") {
    val diff = ScannedDigit.lineCharDiffs("foo", "foo")
    assert(diff == 0)
  }

  test("EIGHT and NINE differ by one line") {
    assert(EIGHT.lineDiffs(NINE) === 1)
  }

  test("EIGHT and EIGHT differ by no lines") {
    assert(EIGHT.lineDiffs(EIGHT) === 0)
  }

  test("There are three digits off by one from eight") {
    val offByOne = EIGHT.offByOneScannedDigits
    assert(offByOne.toSet.size === 3)
    assert(offByOne.contains(NINE) === true)
    assert(offByOne.contains(ZERO) === true)
    assert(offByOne.contains(SIX) === true)
  }

  test("There are two digits one off from five") {
    val offByOne  = FIVE.offByOneScannedDigits
    assert(offByOne.contains(NINE) === true)
    assert(offByOne.contains(SIX) === true)
  }

  test("One") {
    val offByOne = ONE.offByOneScannedDigits
    offByOne.foreach(println)
  }
}
