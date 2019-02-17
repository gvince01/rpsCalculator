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
          println("Hmm... Something went wrong, would you like to try again?")
          run()

        case Some(inputArr) =>
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
   * Checks the input is in a usable format,
   * e.g "1 + 2"
   * and not containing a-z or unrecognised operators
   *
   * @return Optionally returns an error message to display to the user
   */
  def inputSanityCheck(inputStr: String): Option[List[String]] = {
    
    Option(inputStr.split(" ").toList)
  }
  
}


object Main extends App {

  val main = new Main
  main.welcome()
  main.run()


}
