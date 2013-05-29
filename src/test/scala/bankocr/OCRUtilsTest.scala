package bankocr

import org.scalatest.FunSuite


class OCRUtilsTest extends FunSuite {
  test("a sequence of digits can be converted to a sequence of scanned digits") {
    val scannedDigits = OCRUtils.digits2ScannedDigits("01")
    assert(scannedDigits.head === 0)
    assert(scannedDigits.tail.head === 1)
  }

  test("a sequence of scanned digits can be flattened into lines") {
    val scannedDigits = OCRUtils.digits2ScannedDigits("01")
    val scannedLine = OCRUtils.scannedDigitsToScannedLine(scannedDigits)
    assert(scannedLine.flatten().filter(c => c == '\n').size === 4)
  }

}
