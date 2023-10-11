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
    val init10 = "(k|f)|a|b|(c|d|(e|f))"
    val init11 = "(gh)ab(cd(ef))"
    val res = RegexParser.apply(init)
    println(res.map(_.toString))

    res match {
      case Some(result) => {
        println(Term.prettyTree(result))
        println(Term.applySSNF(result))
        val ssnfApplied = Term.applySSNF(result)
        Term.transformToLeftAssociativity(ssnfApplied)
//        println(Term.prettyTree(ssnfApplied))
        Term.normalizeAlternatives(ssnfApplied, isLeftChild = false)
        println(Term.prettyTree(ssnfApplied))
      }
      case None => println("didn't parse lol")
    }
  }
}