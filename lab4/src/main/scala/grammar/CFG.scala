package grammar

import utils.CommonUtils.{EOL, Eps}

import scala.annotation.tailrec
import scala.collection.mutable
import scala.util.control.Breaks.break

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

  lazy val terms: Set[String] = nonTerminals ++ terminals
}

object CFG {
  def apply(rawGrammar: String): CFG = {
    val newLineRules = rawGrammar.split("\n").map(_.trim)
    val rules = newLineRules.flatMap { line =>
      val (from, to) = (line.split("->")(0).trim, line.split("->")(1).split("\\|").map(_.trim))
      to.map(rule => from + " -> " + rule)
    }.toVector
    apply(rules)
  }

  def apply(rawRules: Vector[String]): CFG = {

    @tailrec
    def init(rules: Vector[String], cfg: CFG = CFG()): CFG = {
      if (rules.isEmpty) {
        cfg
      } else {
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

  private def transformId(cfg: CFG, id: String): String = {
    var iid = id
    var i = 1
    val symbols = cfg.terms
    var it = true
    var s = ""
    while (it) {
      s = iid + "'"
      if (!symbols.contains(s)) {
        it = false
      } else {
        i += 1
        iid = id + i.toString
      }
    }
    s
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

    val rules = Rule(transformId(cfg, cfg.startSymbol), right = List(cfg.startSymbol, EOL)) +: cfg.rules
    val result = mutable.Map.empty[String, mutable.Set[String]]
    var col = mutable.Set.empty[String]
    rules.foreach { rule =>

      if (!rule.isEpsRule) {
        var jc = rule.right.size
        var j = 1
        while (j < jc) {
          var r = rule.right.toVector(j)
          var target = rule.right.toVector(j - 1)

          if (cfg.isNonTerminal(target)) {
            if (!result.contains(target)) {
              col = mutable.Set[String]()
              result += (target -> col)
            } else {
              col = result(target)
            }
            predict.getOrElse(r, List.empty).foreach { f =>
              if (!col.contains(f._2)) {
                col += f._2
              } else if (!col.contains(f._1.left)) {
                col += f._1.left
              } else ()
            }
          }

          j += 1
        }

        var rr = rule.right.toVector(jc - 1)
        if (cfg.isNonTerminal(rr)) {
          if (!result.contains(rr)) {
            col = mutable.Set[String]()
            result += (rr -> col)
          } else {
            col = result(rr)
          }
          if (!col.contains(rule.left)) {
            col += rule.left
          } else ()
        }
      } else {
        if (!result.contains(rule.left)) {
          col = mutable.Set[String]()
          result += (rule.left -> col)
        } else {
          col = result(rule.left)
        }
        if (!col.contains(rule.left)) {
          col += rule.left
        } else ()
      }
    }
    var done = false
    while (!done) {
      done = true
      result.foreach { kv =>
        var found = false
        kv._2.foreach { item =>
          if (!found && cfg.isNonTerminal(item)) {
            done = false
            kv._2 -= item
            result.getOrElse(item, mutable.Set.empty).foreach { f =>
              kv._2 += f
            }
            found = true
          }
        }
      }
    }
    result.map(kv => (kv._1, kv._2.toSet - Eps)).toMap
  }

  def toParseTable(cfg: CFG): Map[String, Map[String, Rule]] = {
    var result = mutable.Map[String, mutable.Map[String, Rule]]()
    cfg.nonTerminals.foreach { nt =>
      var d = mutable.Map[String, Rule]()
      cfg.predictSet(nt).foreach { f =>
        if (f._2 != Eps) {
          d += (f._2 -> f._1)
        } else {
          var ff = cfg.followSet(nt)
          ff.foreach { fe =>
            d += (fe -> f._1)
          }
        }
      }
      result += (nt -> d)
    }
    result.map(kv => (kv._1, kv._2.toMap)).toMap
  }
}
