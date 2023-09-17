import model.Unfolder
import parser.Parser

object Main {
  def main(args: Array[String]): Unit = {
    val test1 =
      """
        |variables = x, a, b, y
        |
        |f(g(x, y), g(x, y)) = g(x, y)
        |(g(f(x), f(x)) = h(g(x, x), x))
        |""".stripMargin
    val test2 =
      """
        |variables = x, y
        |f(g(x, y)) = g(h(y), x)
        |""".stripMargin

    val (trs, vars) = Parser.parse(test1)
    println(trs)

    trs.rules.map(rule => InequalitiesMaker.make(Unfolder.unfold(rule.left), Unfolder.unfold(rule.right)))
      .foreach(println)
  }
}