package bankocr

import org.scalatest.FunSuite


class ScannedLineTest extends FunSuite {
  import OCRUtils.scannedLineFromDigitString

  test("all zeros returns 000000000") {
    assert("000000000" === scannedLineFromDigitString("000000000").toDigitString)
  }

  test("all ones returns 111111111") {
    assert("111111111" === scannedLineFromDigitString("111111111").toDigitString)
  }

  test("all twos returns 222222222") {
    assert("222222222" === scannedLineFromDigitString("222222222").toDigitString)
  }

  test("all threes returns 333333333") {
    assert("333333333" === scannedLineFromDigitString("333333333").toDigitString)
  }

  test("fours and fives returns 454545454") {
    assert("454545454" === scannedLineFromDigitString("454545454").toDigitString)
  }

  test("sixes and severs returns 676767676") {
    assert("676767676" === scannedLineFromDigitString("676767676").toDigitString)
  }

  test("eights and nines returns 898989898") {
    assert("898989898" === scannedLineFromDigitString("898989898").toDigitString)
  }
}