

trait ReversePolishLogic {

  /**
   * Given an expression in reverse polish notation, evaluates it and returns the result
   * @param inputList
   * @return
   */
  def evaluateReversePolishInput(inputList: List[String]): Double = {
    def helper(inputList: List[String], stack: List[Double]): Double = {
      inputList match {
        // Don't actually need a sum here, the stack should contain just one element
        case _ if inputList.isEmpty => stack.sum

        case command :: xs =>
          helper(xs, reversePolishExecutionHelper(stack, command))
      }
    }

    helper(inputList, List.empty)
  }


  /**
   * Given a input, that can be either an operator or a number,
   * apply the correct operation and return a new list
   */
  def reversePolishExecutionHelper(data: List[Double], input: String): List[Double] = {
    data match {
      case List(_) | Nil => input.toDouble :: data
      case x::y::xs => input match {
        case "+" => operators.Add(y, x) :: xs
        case "*" => operators.Multiply(y, x) :: xs
        case "/" => operators.Divide(y, x) :: xs
        case "-" => operators.Minus(y, x) :: xs
        case _ => input.toDouble :: data
      }
    }
  }
}
