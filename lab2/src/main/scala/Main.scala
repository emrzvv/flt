import model.{SSNF, Term}

object Main {
  def main(args: Array[String]): Unit = {
    val init = "(((ab|c)|b*)*)*"
    val init2 = "abc****"
    val init3 = "a***b*c*****"
    val init4 = "abc(de)"
    val init5 = "(a*)*"
    val init6 = "a|b|c|c|a"
    val res = RegexParser.apply(init6)
    println(res.map(_.toString))

    res match {
      case Some(result) => {
        println(Term.prettyTree(result))
        println(SSNF(result))
      }
      case None => println("didn't parse lol")
    }
  }
}