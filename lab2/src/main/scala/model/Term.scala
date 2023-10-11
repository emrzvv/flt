package model

import scala.annotation.tailrec


sealed trait Term { // TODO: описать toString для всех кейс-классов
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
case class Or(var left: Term, var right: Term, var isACIProcessed: Boolean = false) extends Term with Binary { // a|b
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
            case Or(left, right, _) => Or(applySSNF(left), applySSNF(right))
            case Concat(left, right) => Concat(applySSNF(left), applySSNF(right))
            case Repeat(term) => Repeat(applySS(term))
        }
    }

    private def applySS(term: Term): Term = {
        term match {
            case s@Symbol(_) => s
            case Or(left, right, _) => Or(applySS(left), applySS(right))
            case Concat(left, right) => Concat(applySSNF(left), applySSNF(right))
            case Repeat(term) => applySS(term)
        }
    }

    private def swapBinary[T <: Binary](root: T): Unit = {
        (root.left, root.right) match {
            case (left: T, right: T) =>
                val oldRootLeft = left
                root.left = right
                val newRootRight = root.left.asInstanceOf[T].right
                root.left.asInstanceOf[T].right = root.left.asInstanceOf[T].left // swap
                root.left.asInstanceOf[T].left = oldRootLeft
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
            case or@Or(left, right, _) =>
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

    def normalizeAlternatives(term: Term, isLeftChild: Boolean, parent: Option[Term] = None): Unit = { // process only when tree is left associative
        @tailrec
        def getAlternativeSubtree(current: Term, subtree: Vector[Or] = Vector.empty): Vector[Or] = {
            current match {
                case or@Or(left, right, _) => getAlternativeSubtree(left, subtree :+ or)
                case term@_ => subtree
            }
        }

        @tailrec
        def getAlternativeSubtreeArguments(current: Term, args: Vector[Term] = Vector.empty): Vector[Term] = current match {
            case or@Or(left, right, _) => getAlternativeSubtreeArguments(left, args :+ right)
            case term@_ => args :+ term
        }

        def createAlternativesWithArguments(args: Vector[Term]): Or = {
            if (args.size == 1) throw new Exception("incorrect alternatives input")
            else if (args.size == 2) Or(args(0), args(1), isACIProcessed = true)
            else Or(createAlternativesWithArguments(args.tail), args.head, isACIProcessed = true)
        }

        term match {
            case or@Or(left, right, isACIProcessed) if !isACIProcessed =>
                val args = getAlternativeSubtreeArguments(or)
                val processedArgs = args.distinctBy(_.toString).sortBy(_.toString) // TODO: узнать ещё раз, как сортировать лексикографически
                println(processedArgs)
                val formedAlternatives = createAlternativesWithArguments(processedArgs)
                println(Term.prettyTree(formedAlternatives))
                parent.foreach {
                    case b: Binary => if (isLeftChild) b.left = formedAlternatives else b.right = formedAlternatives
                    case r@Repeat(_) => r.term = formedAlternatives
                }
                normalizeAlternatives(formedAlternatives.left, isLeftChild = true, parent = Some(formedAlternatives))
                normalizeAlternatives(formedAlternatives.right, isLeftChild = false, parent = Some(formedAlternatives))
            case or@Or(left, right, _) =>
                normalizeAlternatives(left, isLeftChild = true, parent = Some(or))
                normalizeAlternatives(right, isLeftChild = false, parent = Some(or))
            case concat@Concat(left, right) =>
                normalizeAlternatives(left, isLeftChild = true, parent = Some(concat))
                normalizeAlternatives(right, isLeftChild = false, parent = Some(concat))
            case repeat@Repeat(inner) => normalizeAlternatives(inner, isLeftChild = false, parent = Some(repeat))
            case _ => ()
        }
    }
}