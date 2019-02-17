import org.scalatest._


/**
 * @author gvince01
 *
 */

class ReversePolishLogicTest extends FunSuite with ReversePolishLogic {

  val lst = List(1.0, 2.0)

  test("reversePolishExecutionHelper addition test") {
    val operator = "+"
    val result = reversePolishExecutionHelper(lst, operator)
    assert(result == List(3.0))
  }

  test("reversePolishExecutionHelper minus test") {
    val operator = "-"
    val result = reversePolishExecutionHelper(lst, operator)
    assert(result == List(1.0))
  }

  test("reversePolishExecutionHelper multiply test") {
    val operator = "*"
    val result = reversePolishExecutionHelper(lst, operator)
    assert(result == List(2.0))
  }

  test("reversePolishExecutionHelper divide test") {
    val operator = "/"
    val result = reversePolishExecutionHelper(lst, operator)
    assert(result == List(2.0))
  }

  test("evaluateReversePolishInput Test 1") {
    val input = "3 5 + 7 2 - *"
    val inputArr = input.split(" ").toList
    val result = evaluateReversePolishInput(inputArr)

    assert(result == 40.0)
  }

  test("evaluateReversePolishInput Test 2") {
    val input = "3 2 * 11 -"
    val inputArr = input.split(" ").toList
    val result = evaluateReversePolishInput(inputArr)

    assert(result == -5.0)
  }

}
