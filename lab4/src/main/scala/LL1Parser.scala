import grammar.Rule

import scala.collection.mutable

case class LL1Parser(table: Map[String, Map[String, Rule]],
                     terminals: Set[String],
                     var d: mutable.ArrayDeque[Node])
