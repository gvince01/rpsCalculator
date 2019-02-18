import infixutils.InfixUtils

import scala.util.control.NonFatal

/**
 * @author gvince01
 *
 */

class Main extends InfixUtils with ReversePolishLogic {


  def welcome(): Unit = {
    println("Welcome to the Calculator.")
    println("Please input in the following format '1 + 1', Operations currently supported are + / - *")
    println("The use of brackets is also allowed.")
  }

  /**
   * Main "driver"
   */
  def runCalculator(): Unit = {
    try {
      val userInput = scala.io.StdIn.readLine("Enter your query: ")

      // Check there aren't any unexpected characters
      inputSanityCheck(userInput) match {
        case (None, desc) =>
          println("Hmm... $desc, please try again?")
          runCalculator()

        case (Some(inputLst), _) =>
          // Convert the infix into reverse polish, and then evaluate
          val reversePolishNotationArray = convertFromInfixToReversePolish(inputLst)
          val expressionResult = evaluateReversePolishInput(reversePolishNotationArray)

          println(s"Result = $expressionResult")

          def runAgain(): Unit = {
            val userInput = scala.io.StdIn.readLine("Would you like to use the calculator again? (Y/N) ")

            userInput.toLowerCase match {
              case "y" | "yes" =>
                runCalculator()

              case "n" | "no" =>
                println("Goodbye!")

              case _ =>
                println("Hmm... I didn't recognise that input...")
                runAgain()
            }
          }
          runAgain()
      }
    } catch {
      case NonFatal(e) =>
        println(s"Something went wrong... ${e.getMessage}")
        runCalculator()
    }
  }

  /**
   * Checks the input is in a usable format, and returns a desc is there was an error
   *
   */
  def inputSanityCheck(userInputStr: String): (Option[List[String]], String) = {
    try {
      val inputLst = userInputStr.split(" ").toList

      // ensure that either a) the character is a digit or its a valid operand
      val validInput = inputLst.forall(_.forall(char => char.isDigit || InfixUtils.operandToPrecedenceMap.contains(char.toString)))

      if (!validInput) {
        (None, "I don't recognise some of these characters")

      } else {
        (Option(inputLst), "")
      }
      
    } catch {
      case NonFatal(e) =>
        // normally log this
        (None, "Did you separate each operand/operator with a space?")
    }
  }

}


object Main extends App {

  val main = new Main
  main.welcome()
  main.runCalculator()
  
}
