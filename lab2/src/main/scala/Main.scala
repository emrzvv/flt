import model.{RegexTree, Term}

import scala.io.Source
import util.Control._

import java.io.{File, PrintWriter}
import scala.util.Try
object Main {
  def normalizeRegex(regex: String): String = {
    if (regex.isBlank || regex.isEmpty) "empty"
    else {
      val parsed: Option[Term] = Try(RegexParser(regex)).toOption.flatten
      parsed match {
        case Some(result) =>
          val tree = RegexTree(Term.applySSNF(result))
          Term.transformToLeftAssociativity(tree.root)
          Term.normalizeAlternatives(tree.root, isLeftChild = false, parent = tree)
          Term.applyDstr(tree.root, isLeftChild = false, parent = tree)
          tree.toPrettyRegex
        case None => "not parsed"
      }
    }
  }

  def main(args: Array[String]): Unit = {
      val init1 = "(((ab|c)|b*)*)*"
      val init2 = "abc****"
      val init3 = "a***b*c*****"
      val init4 = "abc(de)"
      val init5 = "(((a*)*)*)*|(b*)*"
      val init6 = "a|b|c|c|a"
      val init7 = "a|b|c"
      val init8 = "abcde"
      val init9 = "c*|d*"
      val init10 = "(k|f)|a|b|(c|d|(e|f))"
      val init11 = "(gh)ab(cd(ef))"
      val init12 = "xz|xy"
      val init13 = "yx|zx"
      val init14 = "x|(y|(w|z))"
      val init15 = "x|(y|z)"
      val init16 = "xz|xy|xw|xc|xa|xb|e"
      val init17 = "zx|yx|wx|cx|ax|bx|u"
      val init18 = "(((c|a(b|c)|b|a)|b)*|((acd)e)*)*"
      val init19 = "xyz|x(a|b)"
      val init20 = "a|a" // ???
      val init21 = "abbbba|adddda"
      val init22 = "(acde|agz|acdf|ab|ac)"
    val init23 = "xy|xw|xz"
    val init24 = "(abcdez|abcdz|abcz|abz|az)" // !!!
    val init25 = "a|ab|abc|abcd|abcde"
    val init26 = "((d|(((b*|b))*|da))|(c*)*)"
//      val res = RegexParser.apply(init26)
//      println(res.map(_.toString))
//
//      res match {
//        case Some(result) => {
//          println(Term.prettyTree(result))
//  //        println(Term.applySSNF(result))
//          val tree = RegexTree(Term.applySSNF(result))
//
//          println("PARSING RESULT WITH SSNF")
//          println(Term.prettyTree(tree.root))
//          println(tree.root.toPrettyRegex)
//
//          Term.transformToLeftAssociativity(tree.root)
//          println("APPLYING LA")
//          println(Term.prettyTree(tree.root))
//          println(tree.root.toPrettyRegex)
//
//          Term.normalizeAlternatives(tree.root, isLeftChild = false, parent = tree)
//          println("NORMALIZING ALT")
//          println(Term.prettyTree(tree.root))
//          println(tree.root.toPrettyRegex)
//
//          Term.applyDstr(tree.root, isLeftChild = false, parent = tree)
//          println("APPLYING DSTR")
//          println(Term.prettyTree(tree.root))
//
//          println(tree.toRegex)
//          println(tree.toPrettyRegex)
//        }
//        case None => println("didn't parse lol")
//      }

    val input = if (args.length >= 1) args(0) else throw new Exception("no test input")
      val output = if (args.length == 2) args(1) else "./normalized.txt"

      val writer = new PrintWriter(new File(output))
      using(Source.fromFile(input)) { source =>
        for (regex <- source.getLines()) {
          val toWrite = normalizeRegex(regex)
          writer.println(toWrite)
        }
      }
      writer.close()
  }
}