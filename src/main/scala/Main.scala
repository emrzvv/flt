import parser.Parser

import scala.io.StdIn._
import scala.language.postfixOps
import scala.sys.process._
import scala.util.{Failure, Success, Try}
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

    val in = Iterator.continually(scala.io.StdIn.readLine)
      .takeWhile(Option(_).fold(false)(_.nonEmpty)).mkString("\n")

    val (trs, vars) = Parser.parse(in)
    println(vars)
    println(trs)

    val filename = SmtWriter.transformToSmt2(trs)
    val command = s"z3 -smt2 ./$filename"
//    val stdout = new StringBuilder()
//    val stderr = new StringBuilder()
//    Try(s"z3 -smt2 ./$filename" !!< ProcessLogger(stdout append _, stderr append _)) match {
//      case Success(value) => println(value)
//      case Failure(exception) => println(stderr.toString())
//    }

//    val status = command ! ProcessLogger(stdout append _, stderr append _)
//    println(stdout.toString())
//    println(stderr.toString())
//    println(status)
    var result = ""
    var resultErr = ""
    val io = new ProcessIO(
      stdin => {
                stdin.close()},
      stdout => {result = scala.io.Source.fromInputStream(stdout).mkString
                  stdout.close()},
      stderr => {resultErr = scala.io.Source.fromInputStream(stderr).mkString
                  stderr.close()}
    )

    command.run(io).exitValue()
    println(result)
    s"rm ./$filename" !!
  }
}