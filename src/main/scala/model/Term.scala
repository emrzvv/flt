package model

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

sealed trait Term {
  def name: String
}
case class Constructor(name: String, args: Vector[Term], arity: Int) extends Term {
  override def toString: String = {
    s"${name}(${args.foldLeft("")((acc, t) => acc + t.toString + ",").dropRight(1)})"
  }
}

case class Variable(name: String) extends Term {
  override def toString: String = name
}

case class UnfoldedExpression(variablesCoefficientsMapping: Map[String, Vector[Vector[String]]], // = (var -> sum of products)
                              freeTerms: Vector[Vector[String]]) // <- sum of products of free term

object Term {}

object Unfolder {
  val allConstructors: mutable.Set[Constructor] = mutable.Set.empty
  def unfold(root: Term): UnfoldedExpression = {
    val varCoefficients: mutable.Map[String, Vector[Vector[String]]] = mutable.Map.empty
    val freeTermsTotal: mutable.ArrayBuffer[Vector[String]] = mutable.ArrayBuffer.empty

    def loop(current: Term, coefficients: Vector[String], freeTerms: Vector[String]): Unit = {
      current match {
        case constr@Constructor(name, args, arity) => {
          //          if (arity == 0) {
          //            ??? // TODO дописать
          //          }
          allConstructors += constr.copy(args = Vector.empty)
          val freeTerm = s"${name}_${arity}"
          freeTermsTotal += (freeTerms :+ freeTerm)
          args.zipWithIndex.foreach { v =>
            val c = s"${name}_${v._2}"
            loop(v._1, coefficients :+ c, freeTerms :+ c)
          }
        }
        case v@Variable(name) => {
          val currentCoeffs = varCoefficients.getOrElse(name, Vector.empty)
          varCoefficients.put(name, currentCoeffs :+ coefficients)
        }
      }
    }

    loop(root, Vector.empty, Vector.empty)
    UnfoldedExpression(varCoefficients.toMap, freeTermsTotal.toVector)
  }
}
