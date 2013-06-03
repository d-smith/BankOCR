package bankocr


case class ScannedLine(lines: List[String]) {
  require(lines.size == 3, "must have three raw lines")

  def flatten() : String = {
    lines.foldLeft("")((acc, s) => acc + (s + "\n" )) + "\n"
  }

  def append(scannedLine: ScannedLine) : ScannedLine = {
    copy((lines, scannedLine.lines).zipped map (_+_))
  }

  def toScannedDigits() : List[ScannedDigit] = {
   def toScannedDigitsR(l1: String, l2: String, l3: String, digits: List[ScannedDigit]) : List[ScannedDigit] = {
      if(l1.length == 0) digits
      else {
        val l1Split = l1.splitAt(3)
        val l2Split = l2.splitAt(3)
        val l3Split = l3.splitAt(3)
        toScannedDigitsR(l1Split._2, l2Split._2, l3Split._2,
          digits :+ new ScannedDigit(List(l1Split._1, l2Split._1, l3Split._1)))
      }
    }

    toScannedDigitsR(lines(0), lines(1), lines(2), List[ScannedDigit]())
  }

  override def toString : String = {
    require(lines.foldLeft(true)((b,s) => s.length == 27), "each line must be 27 characters long")
    val scannedDigits = toScannedDigits
    scannedDigits.foldLeft("")((acc, d)=> acc + d.asString)
  }

  override def equals(digits: Any): Boolean = {
    digits match {
      case d:String => toString.equals(d)
      case sl:ScannedLine => toString.equals(sl.toString)
      case _ => false
    }
  }
}

object ScannedLine {
  def apply() : ScannedLine = new ScannedLine(List("","",""))
}
