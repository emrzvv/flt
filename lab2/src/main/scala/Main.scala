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
    val init12 = "xz|xy"
    val init13 = "yx|zx"
    val res = RegexParser.apply(init12)
    println(res.map(_.toString))

    res match {
      case Some(result) => {
        println(Term.prettyTree(result))
//        println(Term.applySSNF(result))
        val ssnfApplied = Term.applySSNF(result)

        Term.transformToLeftAssociativity(ssnfApplied)
        println("APPLYING LA")
        println(Term.prettyTree(ssnfApplied))
        println("FINISHED LA")
        Term.normalizeAlternatives(ssnfApplied, isLeftChild = false)
        println("NORMALIZING ALT")
        println(Term.prettyTree(ssnfApplied))

        Term.applyDstr(ssnfApplied, isLeftChild = false)
        println("APPLYING DSTR")
        println(Term.prettyTree(ssnfApplied))
      }
      case None => println("didn't parse lol")
    }
  }
}