import model.{RegexTree, Term}

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
        val tree = RegexTree(Term.applySSNF(result))

        Term.transformToLeftAssociativity(tree.root)
        println("APPLYING LA")
        println(Term.prettyTree(tree.root))

        Term.normalizeAlternatives(tree.root, isLeftChild = false, parent = Some(tree))
        println("NORMALIZING ALT")
        println(Term.prettyTree(tree.root))

        Term.applyDstr(tree.root, isLeftChild = false, parent = Some(tree))
        println("APPLYING DSTR")
        println(Term.prettyTree(tree.root))

        println(tree.toRegex)
      }
      case None => println("didn't parse lol")
    }
  }
}