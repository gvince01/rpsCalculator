import org.scalatest.FunSuite
import infixutils.InfixUtils

/**
 * @author gvince01
 *
 */

class InfixUtilsTest extends FunSuite with InfixUtils {
  
  test("convertFromInfixToReversePolish test one") {
    val input = "4 + 18 / ( 9 - 3 )"
    val inputLst = input.split(" ").toList

    val output = convertFromInfixToReversePolish(inputLst)
    val expectedResult = List("4", "18", "9", "3", "-", "/", "+")

    assert(output == expectedResult)
  }

  test("convertFromInfixToReversePolish test two") {
    val input = "6 * ( 4 + 5 ) - 25 / ( 2 + 3 )"
    val inputList = input.split(" ").toList

    val output = convertFromInfixToReversePolish(inputList)
    val expectedResult = List("6", "4", "5", "+", "*", "25", "2", "3", "+", "/", "-")

    assert(output == expectedResult)
  }

  test("convertFromInfixToReversePolish test three") {
    val input = "( 2 * 5 + 4 ) / ( 3 * 2 + 1 )"
    val inputLst = input.split(" ").toList

    val output = convertFromInfixToReversePolish(inputLst)

    val expectedResult = List("2", "5", "*", "4", "+", "3", "2", "*", "1", "+", "/")

    assert(output == expectedResult)

  }
}
