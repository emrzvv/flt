package grammar

import utils.CommonUtils.Eps

//sealed trait Rule
case class Rule(left: String, right: List[String] = List.empty, isEmpty: Boolean = false) {
  def isEpsRule: Boolean = right.length == 1 && right.head == Eps
}
//case object EmptyRule extends Rule


