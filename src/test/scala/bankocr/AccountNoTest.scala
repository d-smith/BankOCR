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

  test("account numbers created from too many characters throw IllegalArgumentException") {
    intercept[IllegalArgumentException] {
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

  test("invalid account numbers indicate this when printed") {
    val accountNo = AccountNo("111111111")
    assert(accountNo.toString === "111111111 ERR")
  }

  test("an account number with illegible characters substitutes ? characters for the illegible chars") {
    val accountNo = AccountNo("123ff6789")
    assert(accountNo === "123??6789")
  }

  test("an account number wil illegible characters indicates this when printed") {
    val accountNo = AccountNo("987xx4321")
    assert(accountNo === "987??4321")
    assert(accountNo.toString === "987??4321 ILL")
  }

}
