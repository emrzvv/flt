package model

sealed trait Term
case class Symbol(value: Char) extends Term { // a
}
case class Or(left: Term, right: Term) extends Term { // a|b
}
case class Concat(left: Term, right: Term) extends Term { // ab
}
case class Repeat(term: Term) extends Term { // a*
}
case class Optional(term: Term) extends Term { // a?
}

case object Eps extends Term

object Term {
    def prettyTree(term: Term): String = {
        pprint.tokenize(term).mkString
    }
}