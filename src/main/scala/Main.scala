import parser.Parser

import scala.io.StdIn
import scala.language.postfixOps
import sys.process._

object Main {
  def main(args: Array[String]): Unit = {
    println("Пример ввода:\nvariables = x, a, b, y\nf(g(x, y), g(x, y)) = g(x, y)\n\nПустая строка - конец ввода")
    val test1 =
      """
        |variables = x, a, b, y
        |
        |f(g(x, y), g(x, y)) = g(x, y)
        |g(f(x, y), f(x, y)) = h(g(x, x), x)
        |""".stripMargin
    val test2 =
      """
        |variables = x, y
        |f(g(x, y)) = g(h(y), x)
        |""".stripMargin
    val test3 =
      """
        |variables = x
        |f(g(x)) = g(f(x))
        |""".stripMargin

    val in = Iterator.continually(io.StdIn.readLine)
      .takeWhile(Option(_).fold(false)(_.nonEmpty)).mkString("\n")

    val (trs, vars) = Parser.parse(in)
    println(vars)
    println(trs)

    val filename = SmtWriter.transformToSmt2(trs)
    val result = s"z3 -smt2 ./$filename" !!

    println(result)
    s"rm ./$filename" !!
  }
}