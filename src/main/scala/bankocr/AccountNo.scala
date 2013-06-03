package bankocr


class AccountNo private (scannedLine: ScannedLine) {
  require(scannedLine.toString.length == 9, "Valid account numbers must have 9 digits")

  override def toString() : String = {
    val base = scannedLine.toString
    if(base.contains("?")) {
      base + " ILL"
    } else if(!isValid) {
      base + " ERR"
    } else {
      base
    }
  }

  override def equals(accountNo: Any): Boolean = {
    accountNo match {
      case a:String => scannedLine.toString.equals(a)
      case _ => false
    }
  }

  def isValid() : Boolean = {
    val pairs =  (List.range(1,10).reverse, scannedLine.toString).zipped
    val factors = pairs.map((i, c)=> i * (c - '0'))
    factors.sum % 11 == 0
  }
}

object AccountNo {
  import OCRUtils.scannedLineFromDigitString

  def apply(scannedLine: ScannedLine) : AccountNo = {
    new AccountNo(scannedLine)
  }

  def apply(digits: String) : AccountNo = {
    new AccountNo(scannedLineFromDigitString(digits))
  }
}

