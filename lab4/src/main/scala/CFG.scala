import scala.annotation.tailrec

sealed trait Element {
  val name: String
}

case class Term(name: String) extends Element

case class NonTerm(name: String) extends Element

case class Rule(name: String, elements: Seq[Element])

case class CFG(start: String, rules: Set[Rule])

object CFG {
  implicit class StringOps(value: String) {
    def toCFGEntity(isNonTerminal: Boolean): Element = {
      if (isNonTerminal) NonTerm(value)
      else Term(value)
    }
  }

  def apply(rawRules: Vector[String]): CFG = {
    val separated = rawRules.map(_.trim).flatMap { line =>
      val (from, to) = (line.split("->")(0).trim, line.split("->")(1).split("\\|").map(_.trim))
      to.map(rule => from + " -> " + rule)
    }

    val nonTerminals = separated.map(rule => rule.split("->").head.trim).toSet

    @tailrec
    def loop(rules: Vector[String], acc: Set[Rule], startSymbol: String = ""): CFG = {
      if (rules.isEmpty) CFG(startSymbol, acc)
      else {
        val splitted = rules.head.split("->").map(_.trim)
        val left = splitted.head.trim
        val right = splitted.tail.head.split(" ").map(_.trim).filterNot(_ == Epsilon.name).toVector
        val newRule = Rule(left, right.map(r => r.toCFGEntity(nonTerminals.contains(r)))) // TODO: bad reference to nt set

        loop(rules.tail, acc + newRule, if (startSymbol.isEmpty) left else startSymbol)
      }
    }


    loop(separated, Set.empty[Rule])
  }

  def fromFile(path: String): (String, String, CFG) = { // (w0, w1, cfg)
    val buffer = scala.io.Source.fromFile(path)
    val lines = buffer.getLines().toVector.filter(l => l.nonEmpty && !l.isBlank)
    buffer.close()

    (lines(0).filterNot(_.isWhitespace).trim + EndMarker.name,
      lines(1).filterNot(_.isWhitespace).trim + EndMarker.name,
      apply(lines.slice(2, lines.size)))
  }
}