sealed trait Element {
  val name: String
}

case class Term(name: String) extends Element

case class NonTerm(name: String) extends Element

case class Rule(name: String, elements: Seq[Element])

case class CFG(start: String, rules: Set[Rule])