import org.scalatest.FunSuite
import infixutils.InfixUtils

/**
 * @author gvince01
 *
 */

class InfixUtilsTest extends FunSuite with InfixUtils {


  test("ShuntingYardHelperTest one") {
    val input = "4 + 18 / ( 9 - 3 )"
    val inputLst = input.split(" ").toList

    val output = convertFromInfixToReversePolish(inputLst)
    val expectedResult = List("4", "18", "9", "3", "-", "/", "+")

    assert(output == expectedResult)
  }
}
