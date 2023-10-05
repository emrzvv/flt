package model

case class Rule(left: Term, right: Term) {
  override def toString: String = s"${left.toString} = ${right.toString}\n"
}
