import model.UnfoldedExpression

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

object InequalitiesMaker {
  val strictDescending: mutable.ArrayBuffer[String] = ArrayBuffer.empty // <- combineWithOr

  def combineWithOr(expressions: Vector[String]): String = {
    "(or " + expressions.mkString(" ") + " )"
  }

  private def combineProductsToExpression(sum: Vector[Vector[String]]): String = {
    if (sum.size == 1) sum.map(s =>
      if (s.size == 1) s.mkString("")
      else "(* " + s.mkString(" ") + ")"
    ).mkString(" ")
    else "(+ " + sum.map(s =>
          if (s.size == 1) s.mkString("")
          else "(* " + s.mkString(" ") + ")"
        ).mkString(" ") + ")"
  }

  def make(left: UnfoldedExpression,
           right: UnfoldedExpression): Vector[String]  = {
    lazy val computateIntersection = {
      val intersectedKeySet = left.variablesCoefficientsMapping.keySet.intersect(right.variablesCoefficientsMapping.keySet)
      intersectedKeySet.map { key =>
        val (l, r) = (left.variablesCoefficientsMapping(key), right.variablesCoefficientsMapping(key))
        val leftSmtExpr = combineProductsToExpression(l)
        val rightSmtExpr = combineProductsToExpression(r)
        strictDescending += s"(> $leftSmtExpr $rightSmtExpr)"
        s"(assert >= $leftSmtExpr $rightSmtExpr)"
      }.toVector
//      left.variablesCoefficientsMapping.map {
//        case (k, v) => (k, (v, right.variablesCoefficientsMapping(k)))
//      }
//        .values
//        .map { tupled =>
//          val l = combineProductsToExpression(tupled._1)
//          val r = combineProductsToExpression(tupled._2)
//          strictDescending += s"(> $l $r)"
//          s"(assert >= $l $r)"
//        }
//        .toVector
    }

    // computate rightVars - leftVars, leftVars - rightVars

    lazy val computateFreeTerms = {
      val leftFreeTermsExpr = combineProductsToExpression(left.freeTerms)
      val rightFreeTermsExpr = combineProductsToExpression(right.freeTerms)
      strictDescending += s"(> $leftFreeTermsExpr $rightFreeTermsExpr)"
      s"(assert >= $leftFreeTermsExpr $rightFreeTermsExpr)"
    }

    computateIntersection :+ computateFreeTerms
//    if (left.variablesCoefficientsMapping.keySet == right.variablesCoefficientsMapping.keySet) {
//      left.variablesCoefficientsMapping.map {
//        case (k, v) => (k, (v, right.variablesCoefficientsMapping(k)))
//      }
//        .values
//        .map{ tupled =>
//          s"${combineProductsToString(tupled._1)} >= ${combineProductsToString(tupled._2)}"}
//        .toVector
//    } else if ((left.variablesCoefficientsMapping.keySet -- right.variablesCoefficientsMapping.keySet).nonEmpty) {
//      Vector("unsat")
//    } else {
//
//    }
  }
}
