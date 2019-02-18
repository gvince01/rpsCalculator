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

  def run(): Unit = {
    try {
      val userInput = scala.io.StdIn.readLine("Enter your query: ")

      inputSanityCheck(userInput) match {
        case None =>
          println("Hmm... I don't recognise some of these characters, would you like to try again?")
          run()

        case Some(inputArr) =>
          // Convert the infix into reverse polish, and then evaluate
          val reversePolishNotationArray = convertFromInfixToReversePolish(inputArr)
          val expressionResult = evaluateReversePolishInput(reversePolishNotationArray)

          println(s"Result = $expressionResult")
          runAgain()

          def runAgain(): Unit = {
            val userInput = scala.io.StdIn.readLine("Would you like to use the calculator again? (Y/N) ")

            userInput.toLowerCase match {
              case "y" | "yes" =>
                run()

              case "n" | "no" =>
                println("Goodbye!")

              case _ =>
                println("Hmm... I didn't recognise that input...")
                runAgain()
            }
          }
      }
    } catch {
      case NonFatal(e) =>
        println(s"Something went wrong... ${e.getMessage}")
        run()
    }
  }

  /**
   * Checks the input is in a usable format
   *
   */
  def inputSanityCheck(userInputStr: String): Option[List[String]] = {
    val inputLst = userInputStr.split(" ").toList

    // ensure that either a) the character is a digit or its a valid operand
    val validInput = inputLst.forall(_.forall(char => char.isDigit || InfixUtils.operandToPrecedenceMap.contains(char.toString)))

    if (!validInput) {
      None
    } else {
      Option(inputLst)
    }
  }
  
}


object Main extends App {

  val main = new Main
  main.welcome()
  main.run()


}
