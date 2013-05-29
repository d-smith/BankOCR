package bankocr

class ScannedDigit(lines: List[String])  {

  override def hashCode(): Int = {
    lines.foldLeft("")((acc: String, s: String)=> acc + s).hashCode
  }

  override def equals(p1: Any): Boolean = {
    p1 match {
      case i: Int if i == 0 => this.hashCode == ScannedDigit.ZERO.hashCode
      case i: Int if i == 1 => this.hashCode == ScannedDigit.ONE.hashCode
      case i: Int if i == 2 => this.hashCode == ScannedDigit.TWO.hashCode
      case i: Int if i == 3 => this.hashCode == ScannedDigit.THREE.hashCode
      case i: Int if i == 4 => this.hashCode == ScannedDigit.FOUR.hashCode
      case i: Int if i == 5 => this.hashCode == ScannedDigit.FIVE.hashCode
      case i: Int if i == 6 => this.hashCode == ScannedDigit.SIX.hashCode
      case i: Int if i == 7 => this.hashCode == ScannedDigit.SEVEN.hashCode
      case i: Int if i == 8 => this.hashCode == ScannedDigit.EIGHT.hashCode
      case i: Int if i == 9 => this.hashCode == ScannedDigit.NINE.hashCode
      case _ => super.equals(p1)
    }
  }

  def asString(): String = {
    this match {
      case sd:ScannedDigit if(sd.hashCode == ScannedDigit.ZERO.hashCode) => "0"
      case sd:ScannedDigit if(sd.hashCode == ScannedDigit.ONE.hashCode) => "1"
      case sd:ScannedDigit if(sd.hashCode == ScannedDigit.TWO.hashCode) => "2"
      case sd:ScannedDigit if(sd.hashCode == ScannedDigit.THREE.hashCode) => "3"
      case sd:ScannedDigit if(sd.hashCode == ScannedDigit.FOUR.hashCode) => "4"
      case sd:ScannedDigit if(sd.hashCode == ScannedDigit.FIVE.hashCode) => "5"
      case sd:ScannedDigit if(sd.hashCode == ScannedDigit.SIX.hashCode) => "6"
      case sd:ScannedDigit if(sd.hashCode == ScannedDigit.SEVEN.hashCode) => "7"
      case sd:ScannedDigit if(sd.hashCode == ScannedDigit.EIGHT.hashCode) => "8"
      case sd:ScannedDigit if(sd.hashCode == ScannedDigit.NINE.hashCode) => "9"
      case _ => throw new Error("Not a ScannedDigit")
    }
  }

  def toScannedLine : ScannedLine = {
    ScannedLine(lines)
  }
}

object ScannedDigit {

  val zeroLine1 = " _ "
  val zeroLine2 = "| |"
  val zeroLine3 = "|_|"

  val oneLine1 = "   "
  val oneLine2 = "  |"
  val oneLine3 = "  |"

  val twoLine1 = " _ "
  val twoLine2 = " _|"
  val twoLine3 = "|_ "

  val threeLine1 = " _ "
  val threeLine2 = " _|"
  val threeLine3 = " _|"

  val fourLine1 = "   "
  val fourLine2 = "|_|"
  val fourLine3 = "  |"

  val fiveLine1 = " _ "
  val fiveLine2 = "|_ "
  val fiveLine3 = " _|"

  val sixLine1 = " _ "
  val sixLine2 = "|_ "
  val sixLine3 = "|_|"

  val sevenLine1 = " _ "
  val sevenLine2 = "  |"
  val sevenLine3 = "  |"

  val eightLine1 = " _ "
  val eightLine2 = "|_|"
  val eightLine3 = "|_|"

  val nineLine1 = " _ "
  val nineLine2 = "|_|"
  val nineLine3 = " _|"

  val ZERO = new ScannedDigit(List(zeroLine1, zeroLine2, zeroLine3))
  val ONE = new ScannedDigit(List(oneLine1, oneLine2, oneLine3))
  val TWO = new ScannedDigit(List(twoLine1, twoLine2, twoLine3))
  val THREE = new ScannedDigit(List(threeLine1, threeLine2, threeLine3))
  val FOUR = new ScannedDigit(List(fourLine1, fourLine2, fourLine3))
  val FIVE = new ScannedDigit(List(fiveLine1, fiveLine2, fiveLine3))
  val SIX = new ScannedDigit(List(sixLine1, sixLine2, sixLine3))
  val SEVEN = new ScannedDigit(List(sevenLine1, sevenLine2, sevenLine3))
  val EIGHT = new ScannedDigit(List(eightLine1, eightLine2, eightLine3))
  val NINE = new ScannedDigit(List(nineLine1, nineLine2, nineLine3))


}
