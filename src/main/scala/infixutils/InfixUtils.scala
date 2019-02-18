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
        case x if x.isEmpty =>
          // When we have processed all the in put, append the stack to the output queue
          stack.reverse ::: output

        case x :: xs => x match {
          case inputToken if InfixUtils.isLeftBracket(inputToken) =>
            // Left token gets added to the stack
            helper(xs, output, inputToken :: stack)

          case inputToken if InfixUtils.isRightBracket(inputToken) =>
            // Remove elements off the stack (and add to output) until we find the left bracket
            val operators: List[String] = stack.takeWhile(op => !InfixUtils.isLeftBracket(op))

            // Then remove the left bracket
            val newStack = stack.drop(operators.size).dropWhile(op => InfixUtils.isLeftBracket(op))

            helper(xs, operators ::: output, newStack)

          case inputToken if InfixUtils.isOperator(inputToken) =>
            // remove operators from the stack while the top element has a higher precedence than our current operator
            // append them to the output
            val operators: List[String] = InfixUtils.shouldPopToken(inputToken, stack)
            val newStack = stack.drop(operators.size)

            helper(xs, operators ::: output, inputToken :: newStack)

          case inputToken if InfixUtils.isOperand(inputToken) =>
            // operands get added straight to the output
            helper(xs, inputToken :: output, stack)
        }
      }
    }

    val output = helper(input, List(), List())
    output.reverse
  }

}

object InfixUtils {
  def shouldPopToken(op1: String, stack: List[String]): List[String] = {
    if (stack.isEmpty) List.empty else {
      // TODO check that the map contains the element
      stack.takeWhile(head => operandToPrecedenceMap(head) > operandToPrecedenceMap(op1))
    }
  }

  def isRightBracket(operator: String): Boolean = operator == ")"

  def isLeftBracket(operator: String): Boolean = operator == "("

  def isOperand(inputToken: String): Boolean = !isOperator(inputToken)

  def isOperator(inputToken: String): Boolean = operandToPrecedenceMap.contains(inputToken)

  val operandToPrecedenceMap: Map[String, Int] = Map(
    "+" -> 0,
    "-" -> 0,
    "*" -> 1,
    "/" -> 1,
    "(" -> -1,
    ")" -> -1
  )
}
