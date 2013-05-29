package bankocr

import scala.io.Source
import java.io.File

class RawScanProcessor private (reader: Source) {
  val lines = reader.getLines

  def readLine() : Option[ScannedLine] = {
    val threeLines = lines.take(3).toList
    if(threeLines.size == 3) {
      lines.take(1).toList
      Some(ScannedLine(threeLines))
    } else {
      None
    }

  }

}

object RawScanProcessor {
  def apply(lines: String) : RawScanProcessor = {
    new RawScanProcessor(Source.fromString(lines))
  }

  def apply(file: File) : RawScanProcessor = {
    new RawScanProcessor(Source.fromFile(file))
  }
}
