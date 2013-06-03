package bankocr

import org.scalatest.FunSuite


class AccountNoTest extends FunSuite {
  import OCRUtils.scannedLineFromDigitString
  test("we can create an Account No from a ScannedLine") {
    val accountNo = AccountNo(scannedLineFromDigitString("000000000"))
    assert(accountNo === "000000000")
  }

  test("we can create an AccountNo from a string") {
    val accountNo = AccountNo("000000000")
    assert(accountNo === "000000000")
  }

  test("lines constructed from string that contain invalid digits are rejected") {
    intercept[InvalidDigitException] {
      AccountNo("this is not an account number")
    }
  }

  test("an account number with a valid checksum is valid") {
    val accountNo = AccountNo("345882865")
    assert(accountNo.isValid === true)
  }

  test("an account number with invalid checksums are invalid") {
    val accountNo = AccountNo("111111111")
    assert(accountNo.isValid === false)
  }

}
