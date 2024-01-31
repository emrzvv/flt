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

//    val grammar = CFG("S'", Set(
//      Rule("S'", Seq(NonTerm("S"), Term("$"))),
//      Rule("S", Seq(NonTerm("E"))),
//      Rule("E", Seq(NonTerm("T"), NonTerm("Q"))),
//      Rule("Q", Seq(Term("+"), NonTerm("T"), NonTerm("Q"))),
//      Rule("Q", Seq()),
//      Rule("T", Seq(NonTerm("F"), NonTerm("P"))),
//      Rule("P", Seq(Term("*"), NonTerm("F"), NonTerm("P"))),
//      Rule("P", Seq()),
//      Rule("F", Seq(Term("i"))),
//      Rule("F", Seq(Term("("), NonTerm("E"), Term(")")))
//
//    ))
//    val parser = LLParser(grammar)
    val (w0, w1, grammar) = CFG.fromFile("./src/test/input5")
    val firsts: Firsts = Firsts(grammar)
    val follows = Follows(grammar, firsts)

    pprint.pprintln(grammar)
    pprint.pprintln(firsts)
    pprint.pprintln(follows)
    pprint.pprintln(LLParser(grammar).table)
    val parser = LLParser(grammar)
//    pprint.pprintln(LLParser(grammar).parseToTree(List("a", "d", "b")))
//    val res = LLParser(grammar).parseToTree(List("a", "d", "b"), 0)
//    val parser = LLParser(grammar)
//    val w0 = "i+i+i+i$"
//    val w1 = "i*i+i+i$"

    val T0 = parser.parseToTreeDefault(w0.split("").toList)
    val T1 = parser.incrementalParseToTree(w0.split("").toList, T0.get, w1.split("").toList)
    Node.printTree(T0.get)
    println("---------")
    Node.printTree(T1)

    Node.toGraphviz(T0.get, w0, "./T0_parse_tree")
    Node.toGraphviz(T1, w1, "./T1_parse_tree")
  }
}
