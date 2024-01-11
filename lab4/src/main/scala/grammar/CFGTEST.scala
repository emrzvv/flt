package grammar

import utils.CommonUtils.{EOL, Eps, StringOps}

import scala.annotation.tailrec

case class CFGTEST(equations: Map[String, Set[String]] = Map.empty,
                   nonTerminals: Set[String] = Set.empty,
                   terminals: Set[String] = Set.empty,
                   first: Map[String, Set[String]] = Map.empty,
                   follow: Map[String, Set[String]] = Map.empty,
                   initialState: String = "") {
  def isNonTerminal(symbol: Char): Boolean = nonTerminals.contains(symbol.toString)

  def isTerminal(symbol: Char): Boolean = terminals.contains(symbol.toString)

  def prettyToString: String = ???
}

object CFGTEST {
  private def isNonTerminal(symbol: Char): Boolean = 'A' <= symbol && symbol <= 'Z'

  def apply(raw: String): CFGTEST = {
    CFGTEST.apply(raw.split("\n").toVector)
  }

  def apply(rawEquations: Vector[String]): CFGTEST = {

    @tailrec
    def init(equations: Vector[String], cfg: CFGTEST = CFGTEST()): CFGTEST = {
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
//        val _mTableFirstRule =
//          for {
//            nonTerminal <- _nonTerminals
//            terminal <- _terminals
//          } yield

        init(equations.tail, CFGTEST(_equations, _nonTerminals, _terminals, _first, _follow, _initialState))
      }
    }

    val initializedCFG: CFGTEST = init(rawEquations) // TODO: normalize grammar
//    computeFollowSet(computeFirstSet(initializedCFG))
    computeFirstSet(initializedCFG)
  }

  private def computeFirstSet(cfg: CFGTEST): CFGTEST = {
    @tailrec
    def loop(nonTerminals: Vector[String], first: Map[String, Set[String]] = Map.empty): Map[String, Set[String]] = {
      if (nonTerminals.isEmpty) {
        first
      } else {
        val currentNT = nonTerminals.head
        val firstOfCurrentNT = if (!first.contains(currentNT)) {
          getFirst(currentNT, Set.empty[String])
        } else Set.empty[String]

        val productions = cfg.equations(currentNT)
        val finalFirst = if (hasToDeleteEps(currentNT, productions.toVector)) {
          firstOfCurrentNT - Eps
        } else firstOfCurrentNT
        loop(nonTerminals.tail, first + (currentNT -> finalFirst))
      }
    }

    @tailrec
    def hasToDeleteEps(nonTerminal: String, productions: Vector[String], hasEpsilonOnProd: Boolean = false, hasTerminal: Boolean = false): Boolean = {
      if (productions.isEmpty) {
        if (cfg.first(nonTerminal).contains(Eps) && !hasEpsilonOnProd && hasTerminal) true
        else false
      } else {
        val currentProduction = productions.head
        val _hasEpsilonOnProd = if (currentProduction.contains(Eps)) true else hasEpsilonOnProd
        val _hasTerminal = if (currentProduction.exists(cfg.isTerminal)) true else hasTerminal
        hasToDeleteEps(nonTerminal, productions.tail, _hasEpsilonOnProd, _hasTerminal)
      }
    }

    def recFirst(nonTerminal: String, lastGESet: Set[String]): Set[String] = {
      if (cfg.first.getOrElse(nonTerminal, Set.empty[String]).nonEmpty) cfg.first(nonTerminal)
      else {
        val productions = cfg.equations(nonTerminal)

      }

      def processProduction(production: String, currentLastGESet: Set[String]) = {
        if (currentLastGESet.contains(production)) ()
        else {
          production.map { c =>
            val symbol = c.toString
            if (cfg.isNonTerminal(c)) {
              if (cfg.first.contains(symbol)) {
                ???
              } else {
                val updatedLastGE = currentLastGESet + production

              }
            }
          }
        }
      }
      ???
    }

    def recFirstLoop(productions: Vector[String]) = ???

    def getFirst(current: String, lastGESet: Set[String] = Set.empty): Set[String] = {
      val currentSymbol: Char = current.head

      if (cfg.isNonTerminal(currentSymbol)) {
        val productions = cfg.equations(currentSymbol.toString)
        productions.flatMap { production =>
          if (lastGESet.contains(production)) Set.empty[String]
          else if (production.isEps) {
            if (production.length > 1) {
              getFirst(current.substring(1))
            } else {
              Set(Eps)
            }
          } else {
            val decomposition: Vector[String] = production.map(_.toString).toVector
            println(decomposition)
            println(s"NONTERMINAL: ${currentSymbol}")
            val possibleFirst = getFirst(decomposition(0))
            val condition1: Set[String] = if (possibleFirst.contains(Eps) && decomposition.length > 1) { // TODO: case when decomposition == 1?
              (possibleFirst - Eps) ++ getFirst(decomposition(1))
            } else possibleFirst
            val condition2: Set[String] = if (decomposition.map(d => getFirst(d)).forall(_.contains(Eps))) { // TODO: memoization
              Set(Eps)
            } else Set.empty[String]

            condition1 ++ condition2
          }
        }
      } else if (cfg.isTerminal(currentSymbol)) {
        Set(currentSymbol.toString)
      } else {
        throw new Exception(s"error when constructing first set: unknown symbol `${current}``")
      }
    }

    cfg.copy(first = loop(cfg.nonTerminals.toVector))
  }

  private def computeFollowSet(cfg: CFGTEST): CFGTEST = {

    @tailrec
    def loop(nonTerminals: Vector[String], follow: Map[String, Set[String]] = Map.empty): Map[String, Set[String]] = {
       if (nonTerminals.isEmpty) {
         follow
       } else {
         val currentNT = nonTerminals.head
         loop(nonTerminals.tail, follow ++ getFollows(currentNT))
       }
    }

    def getFollows(current: String): Map[String, Set[String]] = {
      if (current.length > 1) {
        Map.empty // TODO: logging
      } else {
        val currentNT = current.head
        val productions = cfg.equations(current)
        if (cfg.initialState == current) { // first rule
          Map(current -> Set(EOL))
        } else {
          val foundFollows = productions.flatMap { production =>
             finder(currentNT.toString, production)
          }
          println(foundFollows)
          foundFollows reduce (_ ++ _)
        }
      }
    }

    def finder(from: String, production: String): Seq[Map[String, Set[String]]] = {
      val nonTerminalsPositions = production.zipWithIndex.filter(ci => isNonTerminal(ci._1)) // get idxs of all NT in production
      val foundFollows1 = nonTerminalsPositions.map { ci =>
        val (alpha, beta, nonTerminal) = (production.take(ci._2), production.drop(ci._2 + 1), ci._1.toString)
        (alpha, beta, nonTerminal)
      }.filter(x => x._2.nonEmpty).map {
        case (_: String, beta: String, nonTerminal: String) =>
          if (cfg.isNonTerminal(beta.head)) {
            val betaFirst = cfg.first.getOrElse(beta.head.toString, Set.empty[String])
            val currentFollow = cfg.follow.getOrElse(nonTerminal, Set.empty[String])
            if (betaFirst.contains(Eps)) {
              Map(nonTerminal -> (currentFollow ++ (betaFirst - Eps) ++ cfg.follow.getOrElse(from, Set.empty[String])))
            } else {
              Map(nonTerminal -> (currentFollow ++ betaFirst))
            }
          } else {
            val currentFollow = cfg.follow.getOrElse(nonTerminal, Set.empty[String])
            Map(nonTerminal -> (currentFollow + beta.head.toString))
          }
      }
      val foundFollows2 = nonTerminalsPositions.filter(_._2 == production.length -1).map { ci =>
        Map(ci._1.toString -> cfg.follow.getOrElse(from, Set.empty[String]))
      }
      foundFollows1 ++: foundFollows2
    }

    cfg.copy(follow = loop(cfg.nonTerminals.toVector))
  }
}
