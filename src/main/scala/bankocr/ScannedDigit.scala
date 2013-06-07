package bankocr

class ScannedDigit(lines: List[String])  {
  import ScannedDigit.{ZERO,ONE,TWO,THREE,FOUR,FIVE,SIX,SEVEN,EIGHT,NINE,validDigits,tops, middles, bottoms, lineCharDiffs}

  private def getLines = lines

  override def hashCode(): Int = {
    lines.foldLeft("")((acc: String, s: String)=> acc + s).hashCode
  }

  override def equals(p1: Any): Boolean = {
    p1 match {
      case d: ScannedDigit => {
        (d.getLines,this.getLines).zipped.map(_.equals(_)).reduceLeft((acc,c) => acc && c)
      }
      case i: Int if i == 0 => this.hashCode == ZERO.hashCode
      case i: Int if i == 1 => this.hashCode == ONE.hashCode
      case i: Int if i == 2 => this.hashCode == TWO.hashCode
      case i: Int if i == 3 => this.hashCode == THREE.hashCode
      case i: Int if i == 4 => this.hashCode == FOUR.hashCode
      case i: Int if i == 5 => this.hashCode == FIVE.hashCode
      case i: Int if i == 6 => this.hashCode == SIX.hashCode
      case i: Int if i == 7 => this.hashCode == SEVEN.hashCode
      case i: Int if i == 8 => this.hashCode == EIGHT.hashCode
      case i: Int if i == 9 => this.hashCode == NINE.hashCode
      case _ => super.equals(p1)
    }
  }

  def asString(): String = {
    this match {
      case sd:ScannedDigit if(sd.hashCode == ZERO.hashCode) => "0"
      case sd:ScannedDigit if(sd.hashCode == ONE.hashCode) => "1"
      case sd:ScannedDigit if(sd.hashCode == TWO.hashCode) => "2"
      case sd:ScannedDigit if(sd.hashCode == THREE.hashCode) => "3"
      case sd:ScannedDigit if(sd.hashCode == FOUR.hashCode) => "4"
      case sd:ScannedDigit if(sd.hashCode == FIVE.hashCode) => "5"
      case sd:ScannedDigit if(sd.hashCode == SIX.hashCode) => "6"
      case sd:ScannedDigit if(sd.hashCode == SEVEN.hashCode) => "7"
      case sd:ScannedDigit if(sd.hashCode == EIGHT.hashCode) => "8"
      case sd:ScannedDigit if(sd.hashCode == NINE.hashCode) => "9"
      case _ => "?"
    }
  }

  def toScannedLine : ScannedLine = {
    ScannedLine(lines)
  }

  override def toString() : String = {
    lines.mkString("\n") + "\n"
  }



  def isDigit() : Boolean = {
   validDigits.contains(this)
  }

  def findSingleDifferingLine(validDigit: ScannedDigit) : Option[(String,Int)] = {
    val (myLines,theirLines,lineNos) = (this.lines, validDigit.getLines, List.range(1,4)).zipped.filter((x,y,_) => !x.equals(y))
    lineNos.size match {
      case 1 if (lineCharDiffs(myLines(0),theirLines(0)) == 1) => Some(myLines(0),lineNos(0))
      case _ => None
    }
  }

  def lineDiffs(that: ScannedDigit) : Int = {
    val (_,_,lineNos) = (this.lines, that.getLines, List.range(1,4)).zipped.filter((x,y,_) => !x.equals(y))
    return lineNos.size
  }

  def offByOneScannedDigits() : List[ScannedDigit] = {
    val offByOne  = validDigits.foldLeft(List[ScannedDigit]()) {
      (acc, validDigit) => {
        findSingleDifferingLine(validDigit) match {
          case Some(diffCtx) => acc ++ validDigit.findOffByOneDigits(diffCtx._1, diffCtx._2)
          case None => acc
        }
      }
    }

    offByOne.distinct
  }

  def findOffByOneDigits(diffLine: String, lineNo: Int) : List[ScannedDigit] = {
    val (list, digitBuilder) = lineNo match {
      case 1 => (tops, makeDigit(_ : String, lines(1),lines(2)))
      case 2 => (middles, makeDigit(lines(0), _ : String, lines(2)))
      case _ => (bottoms, makeDigit(lines(0), lines(1), _ : String))
    }

    list.filter((x) => lineCharDiffs(diffLine, x) == 1).foldLeft(List[ScannedDigit]()) {
      (acc, elem) => {
            val newDigit = digitBuilder.apply(elem)
            if(newDigit.isDigit) newDigit :: acc else acc
      }
    }
  }

  def makeDigit(l1: String, l2: String, l3: String) : ScannedDigit = {
    new ScannedDigit(List(l1,l2,l3))
  }

}

object ScannedDigit {

  val SUS = " _ "
  val PSP = "| |"
  val PUP = "|_|"
  val SSS = "   "
  val SSP = "  |"
  val SUP = " _|"
  val PUS = "|_ "

  val tops = List(SUS, SSS)
  val middles = List(PSP, SSP, SUP, PUP, PUS)
  val bottoms = List(PUP, SSP, PUS, SUP)

  val ZERO = new ScannedDigit(List(SUS, PSP, PUP))
  val ONE = new ScannedDigit(List(SSS, SSP, SSP))
  val TWO = new ScannedDigit(List(SUS, SUP, PUS))
  val THREE = new ScannedDigit(List(SUS, SUP, SUP))
  val FOUR = new ScannedDigit(List(SSS, PUP, SSP))
  val FIVE = new ScannedDigit(List(SUS, PUS, SUP))
  val SIX = new ScannedDigit(List(SUS, PUS, PUP))
  val SEVEN = new ScannedDigit(List(SUS, SSP, SSP))
  val EIGHT = new ScannedDigit(List(SUS, PUP, PUP))
  val NINE = new ScannedDigit(List(SUS, PUP, SUP))
  val ILL = new ScannedDigit(List("xxx","xxx","xxx"))

  val validDigits = List(ZERO,ONE, TWO,THREE,FOUR, FIVE,SIX,SEVEN, EIGHT,NINE)

  def lineCharDiffs(l1: String, l2: String) : Int = {
    (l1,l2).zipped.map(_ == _).filter(x => !x).size
  }



}
