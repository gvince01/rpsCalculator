import infixutils.InfixUtils

/**
 * @author gvince01
 *
 */

class Main extends InfixUtils with ReversePolishLogic {

  def run(): Unit = {
    println("Please input in the following format '1 + 1', Operations currently supported are + / - *")
    val input = scala.io.StdIn.readLine("Enter your query: ")

    inputSanityCheck(input) match {
      case None =>
        println("Hmm... Something went wrong, would you like to try again?")
        run()

      case Some(inputArr) =>
        val reversePolishForm = convertFromInfixToReversePolish(inputArr)

        val result = evaluateReversePolishInput(reversePolishForm)

        println(s"Result = $result")
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
  main.run()


}
