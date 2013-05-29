package bankocr


object OCRUtils {
  import ScannedDigit.{ZERO,ONE,TWO,THREE,FOUR,FIVE,SIX,SEVEN,EIGHT,NINE}

  def digit2ScannedDigit(digit: Char) : ScannedDigit = {
    digit match {
      case '0' => ZERO
      case '1' => ONE
      case '2' => TWO
      case '3' => THREE
      case '4' => FOUR
      case '5' => FIVE
      case '6' => SIX
      case '7' => SEVEN
      case '8' => EIGHT
      case '9' => NINE
      case _ => throw new Error("unrecognized or unimplemented digit")
    }
  }

  def digits2ScannedDigits(digits: String) : List[ScannedDigit] = {
    digits.foldLeft(List[ScannedDigit]()) {
      (scanned, digit) => scanned :+ digit2ScannedDigit(digit)
    }
  }

  def scannedDigitsToScannedLine(digits: List[ScannedDigit]) : ScannedLine = {
    digits.foldLeft(ScannedLine()) {
      (scannedLine, digit) => scannedLine.append(digit.toScannedLine)
    }
  }

  def digitsToRawLines(digits: String) : String = {
    val scannedDigits = digits2ScannedDigits(digits)
    scannedDigitsToScannedLine(scannedDigits).flatten
  }

  def scannedLineFromDigitString(digitString: String) : ScannedLine = {
     val scannedDigits = OCRUtils.digits2ScannedDigits(digitString)
     OCRUtils.scannedDigitsToScannedLine(scannedDigits)
   }
}
