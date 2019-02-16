

class ReversePolishLogic {

  /**
   * Checks the input is in a usable format,
   * e.g "1 + 2"
   * and not containing a-z or unrecognised operators
   *
   * @return Optionally returns an error message to display to the user
   */
  def inputSanityCheck(inputStr: String): Option[List[String]] = {
    Option(inputStr.split(" ").toList)
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
        case "^" => operators.Power(y, x) :: xs
        case _ => input.toDouble :: data
      }
    }
  }

  def helper(inputList: List[String], stack: List[Double]): Double = {
    inputList match {
      case _ if inputList.isEmpty => stack.sum

      case command :: xs =>
        helper(xs, reversePolishExecutionHelper(stack, command))
    }
  }

  def calculate(input: String): Double = {
    inputSanityCheck(input) match {
      case None =>
        0.0

      case Some(inputList) =>
        helper(inputList, List.empty)
    }
  }
}
