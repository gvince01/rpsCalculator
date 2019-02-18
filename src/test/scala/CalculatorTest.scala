import infixutils.InfixUtils
import org.scalatest.FunSuite


class CalculatorTest extends FunSuite with InfixUtils with ReversePolishLogic {

  test("Main Calc test 1") {
    val input = "1 + 10 * 2"
    val inputArr = input.split(" ").toList

    val reversePolishNotationArray = convertFromInfixToReversePolish(inputArr)
    val result = evaluateReversePolishInput(reversePolishNotationArray)


    assert(result == 21.0)
  }

  test("Main Calc test 2") {
    val input = "( 15 + 5 ) / ( 2 + 3 )"

    val inputArr = input.split(" ").toList

    val reversePolishNotationArray = convertFromInfixToReversePolish(inputArr)
    val result = evaluateReversePolishInput(reversePolishNotationArray)


    assert(result == 4.0)
  }

}
