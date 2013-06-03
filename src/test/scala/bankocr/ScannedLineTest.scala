package bankocr

import org.scalatest.FunSuite


class ScannedLineTest extends FunSuite {
  import OCRUtils.scannedLineFromDigitString

  test("all zeros returns 000000000") {
    assert(scannedLineFromDigitString("000000000") === "000000000")
  }

  test("all ones returns 111111111") {
    assert(scannedLineFromDigitString("111111111") === "111111111")
  }

  test("all twos returns 222222222") {
    assert(scannedLineFromDigitString("222222222") === "222222222")
  }

  test("all threes returns 333333333") {
    assert(scannedLineFromDigitString("333333333") === "333333333")
  }

  test("fours and fives returns 454545454") {
    assert(scannedLineFromDigitString("454545454") === "454545454")
  }

  test("sixes and severs returns 676767676") {
    assert(scannedLineFromDigitString("676767676") === "676767676")
  }

  test("eights and nines returns 898989898") {
    assert(scannedLineFromDigitString("898989898") === "898989898")
  }

  test("invalid digits yield an exceptions") {
      scannedLineFromDigitString("12233344X")
  }

}