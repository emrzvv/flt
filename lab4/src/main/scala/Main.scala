import scala.collection.mutable

object Main {
  def main(args: Array[String]) {
//    val grammar = CFG("S", Set(
//      Rule("S", Seq(Term("a"), NonTerm("A"), NonTerm("B"), Term("b"))),
//      Rule("A", Seq(Term("c"))),
//      Rule("A", Seq()),
//      Rule("B", Seq(Term("d"))),
//      Rule("B", Seq())
//    ))

    val grammar = CFG("S'", Set(
      Rule("S'", Seq(NonTerm("S"), Term("$"))),
      Rule("S", Seq(NonTerm("E"))),
      Rule("E", Seq(NonTerm("T"), NonTerm("Q"))),
      Rule("Q", Seq(Term("+"), NonTerm("T"), NonTerm("Q"))),
      Rule("Q", Seq()),
      Rule("T", Seq(NonTerm("F"), NonTerm("P"))),
      Rule("P", Seq(Term("*"), NonTerm("F"), NonTerm("P"))),
      Rule("P", Seq()),
      Rule("F", Seq(Term("i"))),
      Rule("F", Seq(Term("("), NonTerm("E"), Term(")")))

    ))
//    val parser = LLParser(grammar)

    val firsts: Firsts = Firsts(grammar)
    val follows = Follows(grammar, firsts)

    pprint.pprintln(firsts)
    pprint.pprintln(follows)
    pprint.pprintln(LLParser(grammar).table)
//    pprint.pprintln(LLParser(grammar).parseToTree(List("a", "d", "b")))
//    val res = LLParser(grammar).parseToTree(List("a", "d", "b"), 0)
    val parser = LLParser(grammar)
    val w0 = "i+i+i+i$"
    val T0 = parser.parseToTree(List("i", "+", "i", "+", "i", "+", "i", EndMarker.name), 0)
    val T1 = parser.incrementalParseToTree(List("i", "+", "i", "+", "i", "+", "i"), T0.get, List("i", "*", "i", "+", "i", "+", "i"))
    Node.printTree(T0.get)
    println("---------")
    Node.printTree(T1)

    Node.toGraphviz(T0.get, w0, "./T0_parse_tree")
  }
}
