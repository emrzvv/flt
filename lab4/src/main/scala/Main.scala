import scala.collection.mutable

object Main {
  def main(args: Array[String]) {
    val grammar = CFG("S", Set(
      Rule("S", Seq(Term("a"), NonTerm("A"), NonTerm("B"), Term("b"))),
      Rule("A", Seq(Term("c"))),
      Rule("A", Seq()),
      Rule("B", Seq(Term("d"))),
      Rule("B", Seq())
    ))
//    val parser = LLParser(grammar)
//


    val firsts: Firsts = Firsts(grammar)
    val follows = Follows(grammar, firsts)

    pprint.pprintln(firsts)
    pprint.pprintln(follows)
    pprint.pprintln(LLParser(grammar).table)
//    pprint.pprintln(LLParser(grammar).parseToTree(List("a", "d", "b")))
    val res = LLParser(grammar).parseToTree(List("a", "d", "b"), 0)
    Node.printTree(res.get)
  }
}
