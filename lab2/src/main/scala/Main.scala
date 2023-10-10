import model.Term

object Main {
  def main(args: Array[String]): Unit = {
    val init = "(((ab|c)|b*)*)*"
    val init2 = "abc****"
    val init3 = "a***b*c*****"
    val init4 = "abc(de)"
    val init5 = "(((a*)*)*)*|(b*)*"
    val init6 = "a|b|c|c|a"
    val init7 = "a|b|c"
    val init8 = "abcde"
    val init9 = "c*|d*"
    val res = RegexParser.apply(init5)
    println(res.map(_.toString))

    res match {
      case Some(result) => {
        println(Term.prettyTree(result))
        println(Term.applySSNF(result))
      }
      case None => println("didn't parse lol")
    }
  }
}