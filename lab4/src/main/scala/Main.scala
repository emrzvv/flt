import scala.collection.mutable

object Main {
  def main(args: Array[String]) {
    val (w0, w1, grammar) = CFG.fromFile(args(0))
    val firsts: Firsts = Firsts(grammar)
    val follows = Follows(grammar, firsts)

//    pprint.pprintln(grammar)
//    pprint.pprintln(firsts)
//    pprint.pprintln(follows)
//    pprint.pprintln(LLParser(grammar).table)
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
