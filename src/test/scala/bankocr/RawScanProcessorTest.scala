package bankocr

import org.scalatest.FunSuite


class RawScanProcessorTest extends FunSuite {
  import OCRUtils.scannedLineFromDigitString
  test("raw lines are processed into scanned lines") {
    val rawScanProcessor = RawScanProcessor(OCRUtils.digitsToRawLines("01"))
    rawScanProcessor.readLine match {
      case Some(x) => ()
      case None => throw new Error("Expected Some - got None")
    }

  }

  test("returns None if we process input less than 3 lines...") {
    RawScanProcessor("foo").readLine match {
      case Some(x) => throw new Error("Expected None - got Some")
      case None => ()
    }
  }

  test("we can process many lines") {
    val rawScanProcessor = RawScanProcessor(OCRUtils.digitsToRawLines("00")
      + "\n"
      + OCRUtils.digitsToRawLines("11"));
    assert(rawScanProcessor.readLine.isDefined)
    assert(rawScanProcessor.readLine.isDefined)
  }

  test("an account number can be read from a file") {
    val rawScanProcessor = RawScanProcessor(new java.io.File("./src/main/resources/singleLineFile"))
    rawScanProcessor.readLine match {
      case Some(line) => assert(scannedLineFromDigitString("123456789") === line)
      case None => throw new Error("None returned - expected a Some(ScannedLine)")
    }
  }


  test("we can read files with multiple lines") {
    val rawScanProcessor = RawScanProcessor(new java.io.File("./src/main/resources/twoLineFile"))
    rawScanProcessor.readLine match {
      case Some(scannedLine) => assert(scannedLineFromDigitString("123456789") === scannedLine)
      case None => throw new Error("None returned - expected a Some(ScannedLine)")
    }

    rawScanProcessor.readLine match {
      case Some(scannedLine) => assert(scannedLineFromDigitString("123456789") === scannedLine)
      case None => throw new Error("None returned - expected a Some(ScannedLine)")
    }
  }

  test("we can read lines until no more lines are available") {
    val rawScanProcessor = RawScanProcessor(new java.io.File("./src/main/resources/twoLineFile"))
    while(rawScanProcessor.readLine.isDefined){}
  }

}
