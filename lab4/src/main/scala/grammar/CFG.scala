package grammar

import utils.CommonUtils.{EOL, Eps}

import scala.annotation.tailrec
import scala.collection.mutable

case class CFG(
              startSymbol: String = "",
              rules: List[Rule] = List.empty,
              nonTerminals: Set[String] = Set.empty,
              terminals: Set[String] = Set.empty
              ) {
  def isNonTerminal(symbol: String): Boolean = nonTerminals.contains(symbol)
  def isTerminal(symbol: String): Boolean = terminals.contains(symbol)

  lazy val predictSet: Map[String, List[(Rule, String)]] = CFG.computePredictSet(this)
  lazy val followSet: Map[String, Set[String]] = CFG.computeFollowSet(this)
}

object CFG {
  def apply(rawGrammar: String): CFG = {
    val newLineRules = rawGrammar.split("\n").map(_.trim)
    val rules = newLineRules.flatMap { line =>
      val (from, to) = (line.split("->")(0).trim, line.split("->")(1).split("\\|").map(_.trim))
      to.map(rule => from + " -> " + rule)
    }.toVector
    println(rules)
    apply(rules)
  }

  def apply(rawRules: Vector[String]): CFG = {

    @tailrec
    def init(rules: Vector[String], cfg: CFG = CFG()): CFG = {
      if (rules.isEmpty) cfg
      else {
        val splitted: Seq[String] = rules.head.split("->").map(_.trim)
        val left = splitted.head.trim
        val right = splitted.tail.head.split(" ").map(_.trim).toList

        val _rules = Rule(left, right) +: cfg.rules
        val _startSymbol = if (cfg.startSymbol.isEmpty) left else cfg.startSymbol

        init(rules.tail, CFG(_startSymbol, _rules))
      }
    }

    def fillNonTerminals(cfg: CFG): CFG = {
      val nonTerminals = cfg.rules.map { _.left}.toSet
      cfg.copy(nonTerminals = nonTerminals)
    }

    def fillTerminals(cfg: CFG): CFG = {
      val terminals = cfg.rules.flatMap { _.right.filterNot(cfg.isNonTerminal) }.toSet + EOL
      cfg.copy(terminals = terminals)
    }

    val initializedCFG = init(rawRules)
    fillTerminals(fillNonTerminals(initializedCFG))
  }

  def computePredictSet(cfg: CFG): Map[String, List[(Rule, String)]] = {
    @tailrec
    def addTerminals(terminals: List[String], result: Map[String, List[(Rule, String)]]): Map[String, List[(Rule, String)]] = {
      if (terminals.isEmpty) {
        result
      } else {
        val l = List((Rule("", List.empty, isEmpty = true), terminals.head))
        addTerminals(terminals.tail, result + (terminals.head -> l))
      }
    }

    @tailrec
    def addRhs(rules: List[Rule], result: Map[String, List[(Rule, String)]]): Map[String, List[(Rule, String)]] = {
      if (rules.isEmpty) {
        result
      } else {
        val rule = rules.head
        val newResult = if (!result.contains(rule.left)) {
          result + (rule.left -> List.empty[(Rule, String)])
        } else result
        val currentCol = newResult(rule.left)
        val colToAdd = if (!rule.isEpsRule) {
          val e = (rule, rule.right.head)
          if (!currentCol.contains(e)) {
            e +: currentCol
          } else currentCol
        } else {
          val e = (rule, Eps)
          if (!currentCol.contains(e)) {
            e +: currentCol
          } else currentCol
        }
        addRhs(rules.tail, newResult + (rule.left -> colToAdd))
      }
    }

    @tailrec
    def resolveFirstForNonTerminals(result: Map[String, List[(Rule, String)]]): Map[String, List[(Rule, String)]] = {
      val updatedResult = result.foldLeft(result) {
        case (acc, (key, value)) => value.foldLeft(acc) {
          case (innerAcc, (rule, symbol)) =>
            if (cfg.isNonTerminal(symbol)) {
              val updatedList = (innerAcc.getOrElse(key, List.empty).toSet - ((rule, symbol))).toList // cringe
              val additionalList = (result.getOrElse(symbol, List.empty))
              innerAcc.updated(key, updatedList.concat(additionalList))
            } else {
              innerAcc
            }
        }
      }
      if (updatedResult == result) {
        updatedResult
      } else {
        resolveFirstForNonTerminals(updatedResult)
      }
    }

    val r1 = addTerminals(cfg.terminals.toList, Map.empty)
    val r2 = addRhs(cfg.rules, r1)
    val r3 = resolveFirstForNonTerminals(r2)
    r3
  }

  def computeFollowSet(cfg: CFG): Map[String, Set[String]] = {
    val predict: Map[String, List[(Rule, String)]] = cfg.predictSet

    def addFollows(result: Map[String, Set[String]], target: String, follows: Set[String]): Map[String, Set[String]] = {
      result + (target -> (result.getOrElse(target, Set()) ++ follows))
    }

    def transformId(id: String): String = { // S => S'
      val transformedId =
        Iterator.iterate(id)(_ + "'").find(!(cfg.terminals ++ cfg.nonTerminals).contains(_)).get
      transformedId
    }

    val rules = Rule(transformId(cfg.startSymbol), right = List(cfg.startSymbol, EOL)) +: cfg.rules
    val result = Map.empty[String, Set[String]]
    val updatedResult = rules.foldLeft(result) { (acc, rule) =>
      val followsToAdd = rule.right.sliding(2).flatMap {
        case List(r, target) if cfg.isNonTerminal(target) =>
          predict(r).flatMap {
            case (r, s) if (s != Eps) => Set(s)
            case (r, s) if (s == Eps) => Set(r.left)
          }
        case _ => Set.empty[String]
      }.toSet

      val intermediateResult =
        if (followsToAdd.nonEmpty) addFollows(acc, rule.right.last, Set(rule.left))
        else acc

      if (cfg.isNonTerminal(rule.right.last)) {
        addFollows(intermediateResult, rule.right.last, Set(rule.left))
      } else {
        intermediateResult
      }
    }

    Iterator
      .iterate(updatedResult) { currentResult =>
        currentResult.map { case (key, value) =>
          key -> value.flatMap(item => if (cfg.isNonTerminal(item)) currentResult.getOrElse(item, Set()) else Set(item))
        }
      }
      .sliding(2)
      .find(pair => pair.head == pair.last)
      .getOrElse(Seq(updatedResult))
      .lastOption
      .getOrElse(Map.empty)
  }
}
