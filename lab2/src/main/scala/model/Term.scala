package model


sealed trait Term {
    val isBinary: Boolean
}
sealed trait Binary extends Term {
    var left: Term
    var right: Term
}
case class Symbol(value: Char) extends Term { // a
    override def toString: String = value.toString

    override val isBinary: Boolean = false
}
case class Or(var left: Term, var right: Term) extends Term with Binary { // a|b
//    override def toString: String = (left, right) match {
//        case (Symbol(l), Symbol(r)) => s"$l|$r"
//    }

    override val isBinary: Boolean = true
}
case class Concat(var left: Term, var right: Term) extends Term with Binary { // ab
    override val isBinary: Boolean = true
}
case class Repeat(var term: Term) extends Term { // a*
    override val isBinary: Boolean = false
}
// made it mutable to rebuild binary tree the most easiest way

case object Eps extends Term {
    override val isBinary: Boolean = false
}

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

    private def swapBinary[T <: Binary](root: T): Unit = {
        (root.left, root.right) match {
            case (left: T, right: T) =>
                val oldLeftOr = left
                root.left = right
                val newRootRight = root.left.asInstanceOf[T].right
                root.left.asInstanceOf[T].right = root.left.asInstanceOf[T].left // swap
                root.left.asInstanceOf[T].left = oldLeftOr
                root.right = newRootRight
                if (newRootRight.isBinary) transformToLeftAssociativity(root)
            case (left: Term, right: T) =>
                val oldLeftTerm = left
                root.left = root.right
                root.right = oldLeftTerm
            case _ => ()
        }
    }

    def transformToLeftAssociativity(toTransform: Term): Unit = {
        toTransform match {
            case or@Or(left, right) =>
                swapBinary(or)
                transformToLeftAssociativity(left)
                transformToLeftAssociativity(right)
            case concat@Concat(left, right) =>
                swapBinary(concat)
                transformToLeftAssociativity(left)
                transformToLeftAssociativity(right)
            case repeat@Repeat(term) =>
                transformToLeftAssociativity(term)
            case _ => ()
        }
    }

    def removeDuplicateOr(): Term = ??? // TODO: проходимся по левому поддереву, пока не закончится OR, собираем правые аргументы, удаляем

    def sortLexicographicOr(): Term = ??? // TODO: для этого надо переопределить toString адекватно, чтобы сравнивать аргументы
}