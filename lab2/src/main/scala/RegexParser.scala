import model._

import scala.util.parsing.combinator._

object RegexParser extends RegexParsers with PackratParsers {

  def symbol: Parser[Term] = ("""\w""".r) ^^ { s => Symbol(s.head) }
  def brackets: Parser[Term] = "(" ~> maxPriority <~ ")"
  def repeat: Parser[Term] = (lowPriority <~ "*") ^^ (r => Repeat(r))
  def concat: Parser[Term] = rep(midPriority) ^^ (list => listToConcat(list.reverse))

  lazy val or: PackratParser[Term] = (maxPriority ~ "|" ~ highPriority) ^^ { case l ~ "|" ~ r => Or(l, r) }


  def lowPriority: Parser[Term] = symbol | brackets
  def midPriority: Parser[Term] = repeat | lowPriority
  def highPriority: Parser[Term] = concat | midPriority

  lazy val maxPriority: PackratParser[Term] = or | highPriority


  def listToConcat(value: List[Term]): Term = value match {
    case head :: Nil => head
    case head :: tail => Concat(listToConcat(tail), head)
  }

  def apply(input: String): Option[Term] = {
    println(input.clean)
    parseAll(maxPriority, input.clean) match {
      case Success(result, _) => Some(result)
      case _ => None
    }
  }

  implicit class StringOps(value: String) {
    def clean: String = {
      val temp = value.replaceAll(" ", "")
      val starRegex = """\*+""".r
      starRegex.replaceAllIn(temp, "*")
    }
  }
}