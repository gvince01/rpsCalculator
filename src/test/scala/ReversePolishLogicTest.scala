import org.scalatest._


/**
 * @author gvince01
 *
 */

class ReversePolishLogicTest extends FunSuite {

  def calculatorLogic = new ReversePolishLogic
  val lst = List(1.0, 2.0)

  test("reversePolishExecutionHelper addition test") {
    val operator = "+"
    val result = calculatorLogic.reversePolishExecutionHelper(lst, operator)
    assert(result == List(3.0))
  }

  test("reversePolishExecutionHelper minus test") {
    val operator = "-"
    val result = calculatorLogic.reversePolishExecutionHelper(lst, operator)
    assert(result == List(1.0))
  }

  test("reversePolishExecutionHelper multiply test") {
    val operator = "*"
    val result = calculatorLogic.reversePolishExecutionHelper(lst, operator)
    assert(result == List(2.0))
  }

  test("reversePolishExecutionHelper divide test") {
    val operator = "/"
    val result = calculatorLogic.reversePolishExecutionHelper(lst, operator)
    assert(result == List(2.0))
  }

  test("reversePolishExecutionHelper power test") {
    val operator = "^"
    val result = calculatorLogic.reversePolishExecutionHelper(lst, operator)
    assert(result == List(2.0))
  }

  test("calculate test 1") {
    val input = "3 5 + 7 2 - *"
    val result = calculatorLogic.calculate(input)

    assert(result == 40.0)
  }


  test("calculate test 2") {
    val input = "162 2 1 + 4 ^ /"
    val result = calculatorLogic.calculate(input)

    assert(result == 2.0)
  }

  test("calculate test 3") {
    val input = "162 2 1 + 4 ^ /"
    val result = calculatorLogic.calculate(input)

    assert(result == 2.0)
  }

  test("calculate test 4") {
    val input = "6 3 - 2 ^ 11 -"
    val result = calculatorLogic.calculate(input)

    assert(result == -2.0)
  }
}
