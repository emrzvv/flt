import scala.annotation.tailrec

sealed trait FirstElement {
  val name: String
}

sealed trait FollowElement {
  val name: String
}

case class Terminal(name: String) extends FirstElement with FollowElement

case object Epsilon extends FirstElement {
  override val name: String = "&"
}

case object EndMarker extends FollowElement {
  override val name: String = "$"
}

case class Firsts(firsts: Map[String, Set[FirstElement]]) {

  def get(elements: List[Element],
          result: Set[String] = Set[String]()): (Boolean, Set[String]) = {
    elements match {
      case Term(t) :: l =>
        (false, result + t)
      case NonTerm(t) :: l =>
        if (firsts(t).contains(Epsilon)) {
          get(l, result ++ (firsts(t) - Epsilon).map(_.name))
        } else {
          (false, result ++ firsts(t).map(_.name))
        }
      case Nil =>
        (true, result)
    }
  }
}

object Firsts {

  @tailrec
  private def processRule(elements: List[Element],
                          firsts: Map[String, Set[FirstElement]],
                          current: Set[FirstElement]): Set[FirstElement] = {
    elements match {
      case Term(t) :: l => current + Terminal(t)
      case NonTerm(t) :: l =>
        if (firsts(t).contains(Epsilon)) {
          processRule(l, firsts, current ++ (firsts(t) - Epsilon))
        } else {
          current ++ firsts(t)
        }
      case Nil => current + Epsilon
    }
  }

  def apply(grammar: CFG): Firsts = {
    def processRules(initialFirsts: Map[String, Set[FirstElement]]) = {
      grammar.rules.foldLeft(initialFirsts) { case (firsts, rule) =>
        firsts.updated(rule.name, processRule(rule.elements.toList, firsts, firsts(rule.name)))
      }
    }
    Firsts(Iterations.whileChanging(processRules, Map[String, Set[FirstElement]]().withDefaultValue(Set())))
  }

}

object Follows {

  @tailrec
  private def processRule(name: String, elements: List[Element],
                          firsts: Firsts,
                          follows: Map[String, Set[FollowElement]]): Map[String, Set[FollowElement]] = {
    elements match {
      case NonTerm(t) :: l =>
        val (hasEpsilon, newFirsts) = firsts.get(l)
        val newFollows = follows(t) ++ newFirsts.map(Terminal) ++ (if (hasEpsilon) {
          follows(name)
        } else {
          Set()
        })
        processRule(name, l, firsts, follows.updated(t, newFollows))
      case _ :: l =>
        processRule(name, l, firsts, follows)
      case Nil =>
        follows
    }
  }

  def apply(grammar: CFG, firsts: Firsts): Map[String, Set[FollowElement]] = {
    def processRules(initialFollows: Map[String, Set[FollowElement]]) = {
      grammar.rules.foldLeft(initialFollows) { case (follows, rule) =>
        processRule(rule.name, rule.elements.toList, firsts, follows)
      }
    }
    Iterations.whileChanging(processRules,
      Map[String, Set[FollowElement]](grammar.start -> Set(EndMarker)).withDefaultValue(Set()))
  }
}