package model

import scala.annotation.{tailrec, unused}

sealed trait Term {
    val isBinary: Boolean
    def toRegex: String
    def toPrettyRegex: String
}
sealed trait Binary extends Term {
    var left: Term
    var right: Term
}
case class Symbol(value: Char) extends Term { // a
    override def toString: String = value.toString

    override val isBinary: Boolean = false

    override def toRegex: String = value.toString

    override def toPrettyRegex: String = value.toString
}
case class Or(var left: Term, var right: Term, var isACIProcessed: Boolean = false) extends Term with Binary { // a|b
    override val isBinary: Boolean = true

    override def toRegex: String = s"(${left.toRegex}|${right.toRegex})"

    override def toPrettyRegex: String = {
        def prettyArg(term: Term): String = term match {
            case s@Symbol(_) => s.toPrettyRegex
            case or@Or(_, _, _) => s"(${or.toPrettyRegex})"
            case concat@Concat(_, _) => concat.toPrettyRegex
            case repeat@Repeat(_) => s"${repeat.toPrettyRegex}"
            case Eps => Eps.toPrettyRegex
        }

        s"${prettyArg(left)}|${prettyArg(right)}"
    }
}
case class Concat(var left: Term, var right: Term) extends Term with Binary { // ab
    override val isBinary: Boolean = true

    override def toRegex: String = s"(${left.toRegex}${right.toRegex})"

    override def toPrettyRegex: String = {
        def prettyArg(term: Term): String = term match {
            case s@Symbol(_) => s.toPrettyRegex
            case or@Or(_, _, _) => s"(${or.toPrettyRegex})"
            case concat@Concat(_, _) => concat.toPrettyRegex
            case repeat@Repeat(_) => s"${repeat.toPrettyRegex}"
            case Eps => Eps.toPrettyRegex
        }

        s"${prettyArg(left)}${prettyArg(right)}"
    }
}
case class Repeat(var term: Term) extends Term { // a*
    override val isBinary: Boolean = false

    override def toRegex: String = s"(${term.toRegex})*"

    override def toPrettyRegex: String = term match {
        case s@Symbol(_) => s"${s.toPrettyRegex}*"
        case or@Or(_, _, _) => s"(${or.toPrettyRegex})*"
        case concat@Concat(_, _) => s"(${concat.toPrettyRegex})*"
        case repeat@Repeat(_) => s"(${repeat.toPrettyRegex})*"
        case Eps => Eps.toPrettyRegex
    }
}
// made it mutable to rebuild binary tree the most easiest way
case class RegexTree(var root: Term) extends Term {
    override val isBinary: Boolean = false

    override def toRegex: String = root.toRegex

    override def toPrettyRegex: String = root.toPrettyRegex
}
case object Eps extends Term { // нужен, потому что у меня нет идей, как можно легко обработать крайние случаи для dstr
    override val isBinary: Boolean = false

    override def toRegex: String = "ε"

    override def toPrettyRegex: String = "ε"
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

    @unused
    private def swapBinary[T <: Binary](root: T): Unit = { // TODO: можно ли вообще в рантайме сравнивать параметризованные типы корректно?
        (root.left, root.right) match {
            case (left: T, right: T) if root.getClass.isInstance(left.getClass) && root.getClass.isInstance(right.getClass) =>
                val oldRootLeft = left
                root.left = right
                val newRootRight = root.left.asInstanceOf[T].right
                root.left.asInstanceOf[T].right = root.left.asInstanceOf[T].left // swap
                root.left.asInstanceOf[T].left = oldRootLeft
                root.right = newRootRight
                if (newRootRight.isBinary) transformToLeftAssociativity(root)
            case (left: Term, right: T) if root.getClass.isInstance(right.getClass) =>
                println("SIMPLE SWAPPING")
                val oldLeftTerm = left
                root.left = root.right
                root.right = oldLeftTerm
            case _ =>
                println("NO SWAPPING")
                println(root.getClass)
                println(root.left.getClass)
                println(root.right.getClass)
                println(root.getClass.isInstance(root.right.getClass))
        }
        println(s"ROOT TRANSFORMED: ${root}")
    }

    private def swapAlternative(root: Or): Unit = {
        (root.left, root.right) match {
            case (left: Or, right: Or)  =>
                val oldRootLeft = left
                root.left = right
                val newRootRight = root.left.asInstanceOf[Or].right
                root.left.asInstanceOf[Or].right = root.left.asInstanceOf[Or].left // swap
                root.left.asInstanceOf[Or].left = oldRootLeft
                root.right = newRootRight
                if (newRootRight.isBinary) transformToLeftAssociativity(root)
            case (left: Term, right: Or) =>
                val oldLeftTerm = left
                root.left = root.right
                root.right = oldLeftTerm
            case _ => ()
        }
    }

    private def swapConcat(root: Concat): Unit = {
        (root.left, root.right) match {
            case (left: Concat, right: Concat) =>
                val oldRootLeft = left
                root.left = right
                val newRootRight = root.left.asInstanceOf[Concat].right
                root.left.asInstanceOf[Concat].right = root.left.asInstanceOf[Concat].left // swap
                root.left.asInstanceOf[Concat].left = oldRootLeft
                root.right = newRootRight
                if (newRootRight.isBinary) transformToLeftAssociativity(root)
            case (left: Term, right: Concat) =>
                val oldLeftTerm = left
                root.left = root.right
                root.right = oldLeftTerm
            case _ => ()
        }
    }

    def transformToLeftAssociativity(toTransform: Term): Unit = {
        toTransform match {
            case or@Or(left, right, _) =>
                swapAlternative(or)
                transformToLeftAssociativity(left)
                transformToLeftAssociativity(right)
            case concat@Concat(left, right) =>
                swapConcat(concat)
                transformToLeftAssociativity(left)
                transformToLeftAssociativity(right)
            case repeat@Repeat(term) =>
                transformToLeftAssociativity(term)
            case t@RegexTree(root) => transformToLeftAssociativity(root)
            case _ => ()
        }
    }

    def replaceChild(parent: Term, isLeftChild: Boolean, newChild: Term): Unit = {
        parent match {
            case b: Binary => if (isLeftChild) b.left = newChild else b.right = newChild
            case r@Repeat(_) => r.term = newChild
            case t@RegexTree(_) => t.root = newChild
        }
    }

    def normalizeAlternatives(term: Term, isLeftChild: Boolean, parent: Term): Unit = { // process only when tree is left associative
        @unused
        @tailrec
        def getAlternativeSubtree(current: Term, subtree: Vector[Or] = Vector.empty): Vector[Or] = {
            current match {
                case or@Or(left, right, _) => getAlternativeSubtree(left, subtree :+ or)
                case term@_ => subtree
            }
        }

        @tailrec
        def getAlternativeSubtreeArguments(current: Term, args: Vector[Term] = Vector.empty): Vector[Term] = current match {
            case or@Or(left: Or, right, _) => getAlternativeSubtreeArguments(left, args :+ right)
            case or@Or(left, right, _) => args :+ right :+ left
            case term@_ => args :+ term
        }

        def createAlternativesWithArguments(args: Vector[Term]): Or = {
            if (args.size == 1) throw new Exception("incorrect alternatives input")
            else if (args.size == 2) Or(args(1), args(0), isACIProcessed = true)
            else Or(createAlternativesWithArguments(args.tail), args.head, isACIProcessed = true)
        }

        term match {
            case or@Or(left, right, isACIProcessed) if !isACIProcessed =>
                val args = getAlternativeSubtreeArguments(or)
                val processedArgs = args.distinctBy(_.toString).sortBy(_.toString).reverse // TODO: узнать ещё раз, как сортировать лексикографически
                val formedAlternatives = createAlternativesWithArguments(processedArgs)
                replaceChild(parent, isLeftChild, newChild = formedAlternatives)
                normalizeAlternatives(formedAlternatives.left, isLeftChild = true, parent = formedAlternatives)
                normalizeAlternatives(formedAlternatives.right, isLeftChild = false, parent = formedAlternatives)
            case or@Or(left, right, _) =>
                normalizeAlternatives(left, isLeftChild = true, parent = or)
                normalizeAlternatives(right, isLeftChild = false, parent = or)
            case concat@Concat(left, right) =>
                normalizeAlternatives(left, isLeftChild = true, parent = concat)
                normalizeAlternatives(right, isLeftChild = false, parent = concat)
            case repeat@Repeat(inner) => normalizeAlternatives(inner, isLeftChild = false, parent = repeat)
            case _ => ()
        }
    }

    def applyDstr(term: Term, isLeftChild: Boolean, parent: Term): Unit = {
        @tailrec
        def getConcatSubtreeArguments(current: Term, args: Vector[Term] = Vector.empty): Vector[Term] = current match {
            case Concat(left: Concat, right) => getConcatSubtreeArguments(left, args :+ right)
            case Concat(left, right) => args :+ right :+ left
            case term@_ => args :+ term
        }

        def getCommonLeft(leftArgs: Vector[Term], rightArgs: Vector[Term]): Vector[Term] = {
            leftArgs.zip(rightArgs).takeWhile(tt => tt._1.toString == tt._2.toString).map(_._1)
        }

        def getCommonRight(leftArgs: Vector[Term], rightArgs: Vector[Term]): Vector[Term] = {
            getCommonLeft(leftArgs.reverse, rightArgs.reverse)
        }

        def createConcatWithArguments(args: Vector[Term]): Term = {
            println(s"${args.size} : ${args}")
            if (args.isEmpty) Eps
            else if (args.size == 1) throw new Exception("incorrect concat arguments")
            else if (args.size == 2) Concat(args(1), args(0))
            else Concat(createConcatWithArguments(args.tail), args.head)
        }

        def dstrl(leftArgs: Vector[Term], rightArgs: Vector[Term]) = {
            val commonLeft = getCommonLeft(leftArgs, rightArgs)
            println(s"COMMON LEFT: ${commonLeft}")
            val toTakeOut =
                if (commonLeft.size == 1) commonLeft.head
                else createConcatWithArguments(commonLeft.reverse)
            val newLeftArgs = leftArgs.drop(commonLeft.size)
            val newRightArgs = rightArgs.drop(commonLeft.size)
            println("TO LEFT OR")
            val toLeft = {
                if (newLeftArgs.isEmpty) Eps
                else if (newLeftArgs.size == 1) newLeftArgs.head
                else createConcatWithArguments(newLeftArgs.reverse)
            }

            val toRight = {
                if (newRightArgs.isEmpty) Eps
                else if (newRightArgs.size == 1) newRightArgs.head
                else createConcatWithArguments(newRightArgs.reverse)
            }
            Concat(toTakeOut, Or(toLeft, toRight, isACIProcessed = true))
        }

        def dstrr(leftArgs: Vector[Term], rightArgs: Vector[Term]) = {
            val commonRight = getCommonRight(leftArgs, rightArgs)
            val toTakeOut = {
                if (commonRight.size == 1) commonRight.head
                else createConcatWithArguments(commonRight.reverse)
            }

            val newLeftArgs = leftArgs.dropRight(commonRight.size)
            val newRightArgs = rightArgs.dropRight(commonRight.size)

            val toLeft = {
                if (newLeftArgs.isEmpty) Eps
                else if (newLeftArgs.size == 1) newLeftArgs.head
                else createConcatWithArguments(newLeftArgs.reverse)
            }

            val toRight = {
                if (newRightArgs.isEmpty) Eps
                else if (newRightArgs.size == 1) newRightArgs.head
                else createConcatWithArguments(newRightArgs.reverse)
            }
            Concat(Or(toLeft, toRight, isACIProcessed = true), toTakeOut)
        }

        term match {
            case or@Or(_, _, _) =>
                applyDstr(or.left, isLeftChild = true, parent = or)
                applyDstr(or.right, isLeftChild = false, parent = or) // может быть, после применённых dstr это уже не or
                (or.left, or.right) match { // ab|ac = a(b|c); ba|ca = (b|c)a
                    case (Concat(a, b), Concat(c, d)) =>
                        val leftConcatArgs = getConcatSubtreeArguments(or.left).reverse
                        val rightConcatArgs = getConcatSubtreeArguments(or.right).reverse
//                        println("------")
//                        println(leftConcatArgs)
//                        println(rightConcatArgs)
                        if (leftConcatArgs.head.toString == rightConcatArgs.head.toString) {
//                            println(s"DSTRL: ${or.left} ; ${or.right} ")
                            val newChild = dstrl(leftConcatArgs, rightConcatArgs)
                            replaceChild(parent, isLeftChild, newChild)
                            applyDstr(newChild, isLeftChild, parent)
                        } else if (leftConcatArgs.last.toString == rightConcatArgs.last.toString) {
//                            println(println(s"DSTRR: ${or.left} ; ${or.right} "))
                            val newChild = dstrr(leftConcatArgs, rightConcatArgs)
                            replaceChild(parent, isLeftChild, newChild)
                            applyDstr(newChild, isLeftChild, parent)
                        } else ()
//                        if (a.toString == c.toString) {
//                            val createdConcat = Concat(a, Or(b, d, isACIProcessed = true))
//                            replaceChild(parent, isLeftChild, newChild = createdConcat)
//                        } else if (b.toString == d.toString) {
//                            val createdConcat = Concat(Or(a, c, isACIProcessed = true), d)
//                            replaceChild(parent, isLeftChild, newChild = createdConcat)
//                        } else ()
                    case _ => ()
                }
            case concat@Concat(left, right) =>
                applyDstr(left, isLeftChild = true, concat)
                applyDstr(right, isLeftChild = false, concat)
            case repeat@Repeat(term) => applyDstr(term, isLeftChild = false, repeat)
            case Symbol(_) => ()
            case Eps => ()
        }
    }
}