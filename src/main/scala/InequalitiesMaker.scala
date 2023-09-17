import model.UnfoldedExpression

object InequalitiesMaker {
  private def combineProductsToString(sum: Vector[Vector[String]]): String = {
//    sum.foldLeft("") { (current, toProduct) =>
//      current + " + " + toProduct.foldLeft("")((currentProduct, value) => currentProduct + " * " + value)
//    }
    sum.map(_.mkString(" * ")).mkString(" + ")
  }

  def make(left: UnfoldedExpression,
           right: UnfoldedExpression): Vector[String]  = {
    if (left.variablesCoefficientsMapping.keySet == right.variablesCoefficientsMapping.keySet) {
      left.variablesCoefficientsMapping.map {
        case (k, v) => (k, (v, right.variablesCoefficientsMapping(k)))
      }
        .values
        .map{ tupled =>
          s"${combineProductsToString(tupled._1)} >= ${combineProductsToString(tupled._2)}"}
        .toVector
    } else {
      Vector("error")
    }
  }
}
