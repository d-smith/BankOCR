package bankocr


class AccountNo private (digits: List[ScannedDigit]) {
  require(digits.length == 9, "Valid account numbers must have 9 digits")

  override def toString() : String = {
    if(isValid) digitsAsString
    else generateAlernativesString
  }

  def digitsAsString() : String = {
    digits.foldLeft("") {
      (s,d) => s + d.asString
    }
  }

  override def equals(accountNo: Any): Boolean = {
    accountNo match {
      case a:String => digitsAsString.equals(a)
      case a:AccountNo => digitsAsString.equals(a.digitsAsString)
      case _ => false
    }
  }

  def allValidDigits() : Boolean = {
    digits.foldLeft(true)((acc, c) => if(acc) c.isDigit else acc)
  }

  def isValid() : Boolean = {
    val pairs =  (List.range(1,10).reverse, digitsAsString).zipped
    val factors = pairs.map((i, c)=> i * (c - '0'))
    factors.sum % 11 == 0
  }

  def getIllegibleCharIdx() : Int = {
    digits.indexWhere((x) => !x.isDigit)
  }

  def getAlternateDigitsAtPos(pos: Integer) : List[ScannedDigit] = {
    digits(pos).offByOneScannedDigits()
  }

  def replaceDigitAtPos(pos: Int, digit: ScannedDigit) : AccountNo = {
    AccountNo(digits.updated(pos,digit))
  }

  def genLegible() : List[AccountNo] = {
    def genLegibleR(numbers: List[AccountNo]) : List[AccountNo] =  {
      val (valid, invalid) = numbers.partition((accountNo)=>accountNo.allValidDigits)
      if(invalid.size == 0) valid
      else {
        val alternatives = invalid.foldLeft(List[AccountNo]()) {
          (acc, n) => {
            acc ++ n.genAlternativesFromIll
          }
        }
        genLegibleR(valid ++ alternatives)
      }
    }

    if(allValidDigits()) List[AccountNo]()
    else genLegibleR(List[AccountNo](this))
  }



  def genAlternativesFromIll() : List[AccountNo] = {
    val firstInvalidDigit = getIllegibleCharIdx
    if(firstInvalidDigit == -1) List[AccountNo]()
    else {
      getAlternateDigitsAtPos(firstInvalidDigit).foldLeft(List[AccountNo]()) {
        (acc, d) => replaceDigitAtPos(firstInvalidDigit, d) :: acc
      }

    }
  }

  def genAlternatives() : List[AccountNo] = {
    def genAlternativesR(numbers: List[AccountNo], pos: Int) : List[AccountNo] = {
      if(pos == 9) numbers
      else {
        val alternativeDigits = digits(pos).offByOneScannedDigits()
        val alternativeAccountNos = alternativeDigits.foldLeft(List[AccountNo]()) {
          (acc, digit) => {
            replaceDigitAtPos(pos, digit) :: acc
          }
        }
        genAlternativesR(numbers ++ alternativeAccountNos, pos + 1)
      }
    }
    genAlternativesR(List[AccountNo](), 0).filter((x) => x.isValid)
  }

  def alternatives2String(alternatives: List[AccountNo]) = alternatives match {
    case h :: Nil => h.digitsAsString
    case _ :: _ => digitsAsString + " AMB [" + alternatives.map(_.digitsAsString).mkString(", ") + "]"
    case Nil => if(!allValidDigits) digitsAsString + " ILL" else digitsAsString + " ERR"
  }

  def generateAlernativesString : String = {
    val illegibleAlternatives = genLegible
    if(illegibleAlternatives.size > 0) alternatives2String(illegibleAlternatives)
    else alternatives2String(genAlternatives)
  }


}

object AccountNo {
  import OCRUtils.scannedLineFromDigitString

  def apply(scannedLine: ScannedLine) : AccountNo = {
    new AccountNo(scannedLine.toScannedDigits)
  }

  def apply(digits: String) : AccountNo = {
    new AccountNo(scannedLineFromDigitString(digits).toScannedDigits)
  }

  def apply(digits: List[ScannedDigit]) : AccountNo = {
    new AccountNo(digits)
  }
}

