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
    val accountNo = AccountNo("100000005")
    assert(accountNo.toString === "100000005 ERR")
  }

  test("an account number with illegible characters substitutes ? characters for the illegible chars") {
    val accountNo = AccountNo("123ff6789")
    assert(accountNo === "123??6789")
  }

  test("an account number wil illegible characters indicates this when printed") {
    val accountNo = AccountNo("xxxxxxx07")
    assert(accountNo === "???????07")
    assert(accountNo.toString === "???????07 ILL")
  }

  test("We can generate legible account numbers from illegible lines") {
    val scannedLines = List(
    "    _  _  _  _  _  _     _ ",
    "|_||_|| || ||_   |  |  | _ ",
    "  | _||_||_||_|  |  |  | _|")

    val accountNo = AccountNo(new ScannedLine(scannedLines))
    val legible = accountNo.genLegible
    assert(legible.size === 2)
    assert(legible.contains(AccountNo("490067713")) === true)
    assert(legible.contains(AccountNo("490067715")) === true)
  }

  test("We can generate valid account nos from an illegible line") {
    val scannedLines = List(
    "    _  _  _  _  _  _     _ ",
    "|_||_|| ||_||_   |  |  | _ ",
    "  | _||_||_||_|  |  |  | _|")

    val accountNo = AccountNo(new ScannedLine(scannedLines))
    val alternatives = accountNo.genLegible.filter((an)=>an.isValid)
    assert(alternatives.size === 1)
    assert(alternatives.contains(AccountNo("490867715")) === true)

  }

  test("888888888 is invalid and there are three possible alternatives") {
    val accountNo = AccountNo("888888888")
    val alternatives = accountNo.genAlternatives
    assert(alternatives.size === 3)
    assert(alternatives.contains(AccountNo("888886888")) === true)
    assert(alternatives.contains(AccountNo("888888880")) === true)
    assert(alternatives.contains(AccountNo("888888988")) === true)
  }

}
