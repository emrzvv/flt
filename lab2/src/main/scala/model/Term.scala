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

    def applySSNF(term: Term): Term = {
        term match {
            case s@Symbol(_) => s
            case Or(left, right) => Or(applySSNF(left), applySSNF(right))
            case Concat(left, right) => Concat(applySSNF(left), applySSNF(right))
            case Repeat(term) => Repeat(applySS(term))
        }
    }

    private def applySS(term: Term): Term = {
        term match {
            case s@Symbol(_) => s
            case Or(left, right) => Or(applySS(left), applySS(right))
            case Concat(left, right) => Concat(applySSNF(left), applySSNF(right))
            case Repeat(term) => applySS(term)
        }
    }
}