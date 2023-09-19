import model.{Constructor, UnfoldedExpression}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

object InequalitiesMaker {
  def combineWithLogicExpr(logic: String, expressions: Vector[String]): String = {
    s"($logic " + expressions.mkString(" ") + ")"
  }

  def notStrictMonotonicity(constructors: Vector[Constructor]): String = {
    val expressions = constructors.flatMap { constr =>
      val varCoefs = (0 until constr.arity).map { i =>
        s"(>= ${constr.name}_$i 1)"
      }.toVector
      val freeCoef = s"(>= ${constr.name}_${constr.arity} 0)"
      varCoefs :+ freeCoef
    }
    combineWithLogicExpr("and", expressions)
  }

  def strictMonotonicity(constructors: Vector[Constructor]): String = {
    val expressions: Vector[String] = constructors.map { constr =>
      val varCoefsExpr = (0 until constr.arity).map { i =>
        s"(> ${constr.name}_$i 1)"
      }.toVector
      val freeCoefExpr = s"(> ${constr.name}_${constr.arity} 0)"

      val varsCoefsExprCombined = constr.arity match {
        case 2 => combineWithLogicExpr("and", varCoefsExpr)
        case 1 => varCoefsExpr.head
        case 0 => "false"
      }

      combineWithLogicExpr("or", Vector(varsCoefsExprCombined, freeCoefExpr))
    }

    combineWithLogicExpr("and", expressions)
  }

  private def combineProductsToExpression(sum: Vector[Vector[String]]): String = {
    if (sum.isEmpty) "0"
    else if (sum.size == 1) sum.map(s =>
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
    val strictDescending: mutable.ArrayBuffer[String] = ArrayBuffer.empty
    val strictDescendingFreeTerms: mutable.ArrayBuffer[String] = ArrayBuffer.empty

    val computeIntersection = {
      val intersectedKeySet = left.variablesCoefficientsMapping.keySet.intersect(right.variablesCoefficientsMapping.keySet)
      intersectedKeySet.map { key =>
        val (l, r) = (left.variablesCoefficientsMapping(key), right.variablesCoefficientsMapping(key))
        val leftSmtExpr = combineProductsToExpression(l)
        val rightSmtExpr = combineProductsToExpression(r)
        strictDescending += s"(> $leftSmtExpr $rightSmtExpr)"
        s"(>= $leftSmtExpr $rightSmtExpr)"
      }.toVector
    }

    val computeWhenLeftVarAbsent = { // if .nonEmpty -> unsat
      val keySet = right.variablesCoefficientsMapping.keySet -- left.variablesCoefficientsMapping.keySet
      keySet.map { key =>
        val r = right.variablesCoefficientsMapping(key)
        val rightSmtExpr = combineProductsToExpression(r)
        strictDescending += s"(> 0 $rightSmtExpr)"
        s"(>= 0 $rightSmtExpr"
      }.toVector
    }

    val computeWhenRightAbsent = {
      val keySet = left.variablesCoefficientsMapping.keySet -- right.variablesCoefficientsMapping.keySet
      keySet.map { key =>
        val l = left.variablesCoefficientsMapping(key)
        val leftSmtExpr = combineProductsToExpression(l)
        strictDescending += s"(> $leftSmtExpr 0)"
        s"(>= $leftSmtExpr)"
      }.toVector
    }

    val computeFreeTerms = {
      val leftFreeTermsExpr = combineProductsToExpression(left.freeTerms)
      val rightFreeTermsExpr = combineProductsToExpression(right.freeTerms)
      strictDescendingFreeTerms += s"(> $leftFreeTermsExpr $rightFreeTermsExpr)"
      s"(>= $leftFreeTermsExpr $rightFreeTermsExpr)"
    }

    val computeStrictDescendingExpr = {
      val varCoeffs = "(and " + strictDescending.mkString(" ") + ")"
      if (strictDescendingFreeTerms.isEmpty) varCoeffs
      else "(or " + varCoeffs + " " + strictDescendingFreeTerms.head + ")"
    }

    computeIntersection :++
      computeWhenLeftVarAbsent :++
      computeWhenRightAbsent :+
      computeFreeTerms :+
      computeStrictDescendingExpr
  }
}
