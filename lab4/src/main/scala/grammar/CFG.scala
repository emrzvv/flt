package grammar

import utils.CommonUtils.{Eps, StringOps}

import scala.annotation.tailrec

case class CFG(equations: Map[String, Set[String]] = Map.empty,
               nonTerminals: Set[String] = Set.empty,
               terminals: Set[String] = Set.empty,
               first: Map[String, Set[String]] = Map.empty,
               follow: Map[String, Set[String]] = Map.empty,
               initialState: String = "") {
  def isNonTerminal(symbol: Char): Boolean = nonTerminals.contains(symbol.toString)

  def isTerminal(symbol: Char): Boolean = terminals.contains(symbol.toString)

  def prettyToString: String = ???
}

object CFG {
  private def isNonTerminal(symbol: Char): Boolean = 'A' <= symbol && symbol <= 'Z'

  def apply(raw: String): CFG = {
    CFG.apply(raw.split("\n").toVector)
  }

  def apply(rawEquations: Vector[String]): CFG = {

    @tailrec
    def init(equations: Vector[String], cfg: CFG = CFG()): CFG = {
      if (equations.isEmpty) cfg
      else {
        val splitted: Seq[String] = equations.head.split("->").map(_.trim)
        val from = splitted.head.trim
        val to = splitted.tail.head.split("\\|").map(_.trim)

        val _equations = cfg.equations + (from -> (cfg.equations.getOrElse(from, Set.empty) ++ to.toSet))
        val _nonTerminals = cfg.nonTerminals + from
        val _first  = cfg.first + (from -> Set.empty[String])
        val _follow = cfg.follow + (from -> Set.empty[String])
        val _terminals = cfg.terminals ++ to.flatMap(_.filterNot(isNonTerminal).map(_.toString)).toSet
        val _initialState = if (cfg.initialState.isEmpty) from else cfg.initialState

        init(equations.tail, CFG(_equations, _nonTerminals, _terminals, _first, _follow, _initialState))
      }
    }

    val initializedCFG: CFG = init(rawEquations)
    computeFirstSet(initializedCFG)
  }

  private def computeFirstSet(cfg: CFG): CFG = {
    @tailrec
    def loop(nonTerminals: Vector[String], first: Map[String, Set[String]] = Map.empty): Map[String, Set[String]] = {
      if (nonTerminals.isEmpty) {
        first
      } else {
        val currentNT = nonTerminals.head
        loop(nonTerminals.tail, first + (currentNT -> getFirst(currentNT)))
      }
    }

    def getFirst(current: String, firstSet: Set[String] = Set.empty): Set[String] = {
      val currentSymbol: Char = current.head

      if (cfg.isNonTerminal(currentSymbol)) {
        val addToFirstSet: Set[String] = cfg.equations(currentSymbol.toString).flatMap { production =>
          if (production.isEps) {
            if (production.length > 1) {
              getFirst(current.substring(1), firstSet)
            } else {
              Set(Eps)
            }
          } else {
            getFirst(production, firstSet)
          }
        }
        firstSet ++ addToFirstSet
      } else if (cfg.isTerminal(currentSymbol)) {
        firstSet + currentSymbol.toString
      } else {
        throw new Exception("error when constructing first")
      }
    }

    cfg.copy(first = loop(cfg.nonTerminals.toVector))
  }

  private def computeFollowSet(cfg: CFG): CFG = {
    ???
  }
}
