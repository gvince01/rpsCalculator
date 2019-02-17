package infixutils

/**
 * @author gvince01
 *
 */

trait InfixUtils {

  /**
   * Given an expression in infix notation, converts it to reverse polish notation
   *
   * Uses the Shunting Yard Algorithm, for further details see
   * https://en.wikipedia.org/wiki/Shunting-yard_algorithm
   * e.g 
   *
   */
  def convertFromInfixToReversePolish(input: List[String]): List[String] = {

    def helper(inputTokenStr: List[String], output: List[String], stack: List[String]): List[String] = {
      inputTokenStr match {
        case x if x == List.empty =>
          stack.reverse ::: output

        case x :: xs => x match {
          case inputToken if InfixUtils.isLeftBracket(inputToken) =>
            helper(xs, output, inputToken :: stack)

          case inputToken if InfixUtils.isRightBracket(inputToken) =>
            val operators: List[String] = stack.takeWhile(op => !InfixUtils.isLeftBracket(op))
            val newStack = stack.drop(operators.size).dropWhile(op => InfixUtils.isLeftBracket(op))

            helper(xs, operators ::: output, newStack)

          case inputToken if InfixUtils.isOperator(inputToken) =>
            val operators: List[String] = InfixUtils.shouldPopToken(inputToken, stack)
            val newStack = stack.drop(operators.size)

            helper(xs, operators ::: output, inputToken :: newStack)

          case inputToken if InfixUtils.isOperand(inputToken) =>
            helper(xs, inputToken :: output, stack)
        }
      }
    }

    val result = helper(input, List(), List())
    result.reverse
  }

}

object InfixUtils {
  private val precedence = Map(
    "+" -> 0,
    "-" -> 0,
    "*" -> 1,
    "/" -> 1,
    "(" -> -1,
    ")" -> -1
  )

  def shouldPopToken(op1: String, stack: List[String]): List[String] = {
    if (stack.isEmpty) List.empty else {
      // TODO check that the map contains the element
      stack.takeWhile(head => precedence(head) > precedence(op1))
    }
  }

  def isRightBracket(operator: String): Boolean = operator == ")"

  def isLeftBracket(operator: String): Boolean = operator == "("

  def isOperand(inputToken: String): Boolean = !isOperator(inputToken)

  def isOperator(inputToken: String): Boolean = precedence.contains(inputToken)

}
