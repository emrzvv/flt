package parser

import model.{Constructor, Rule, Term, Trs, Variable}

import scala.annotation.tailrec
import scala.util.matching.Regex

object Parser {
  private def extractVariables(data: String): Set[String] = {
    data
      .replaceAll("""\s*variables\s*=\s*""", "")
      .split("""\s*,\s*""")
      .toSet
  }


  private def extractTrs(rawTrs: Vector[String], variables: Set[String]): Trs = {
    @tailrec
    def getEndOfArg(data: String, balance: Int, pos: Int): Int = {
      if (data.isEmpty) {
        if (balance == 0) pos
        else throw new Exception("no end of argument. incorrect braces input")
      }
      else {
        data.head match {
          case '(' => getEndOfArg(data.tail, balance + 1, pos + 1)
          case ')' =>
            if (balance - 1 == 0) {
              pos
            }
            else {
              getEndOfArg(data.tail, balance - 1, pos + 1)
            }
          case _ => if (balance == 0 && pos == 1) pos else getEndOfArg(data.tail, balance, pos + 1)
        }
      }
    }

    @tailrec
    def getInnerArguments(data: String, arguments: Vector[String], arity: Int, currentArity: Int): Vector[String] = {
      if (arity == currentArity || data.isEmpty) arguments
      else {
        val endOfCurrentArgument = getEndOfArg(data, 0, 0)
        getInnerArguments(data.slice(endOfCurrentArgument + 1, data.length)
          .trim
          .stripPrefix(",")
          .trim,
          arguments :+ data.slice(0, endOfCurrentArgument + 1),
          arity, currentArity + 1)
      }
    }

    def getConstructorArity(data: String): Int = {
      @tailrec
      def loop(current: String, balance: Int, comaAmount: Int = 0): Int = {
        if (current.isEmpty) {
          if (balance == 0) comaAmount
          else throw new Exception("no end of constructor. incorrect braces input")
        }
        else current.head match {
          case '(' => loop(current.tail, balance + 1, comaAmount)
          case ')' => if (balance - 1 == 0) comaAmount else loop(current.tail, balance - 1, comaAmount)
          case ',' if balance == 1 => loop(current.tail, balance, comaAmount + 1)
          case _ => loop(current.tail, balance, comaAmount)
        }
      }
      if (data.length == 1 && data(0).isLetter) 0
      else loop(data, 0) + 1
    }

    def parseTerm(rawTerm: String, constructorsArity: Map[String, Int] = Map.empty): Term = {
      val toIdentify = rawTerm(0).toString.trim
      if (constructorsArity.contains(toIdentify)) {
        val arity = constructorsArity(toIdentify)
        val innerRawTerms = rawTerm.slice(2, rawTerm.length - 1)
        Constructor(toIdentify,
          getInnerArguments(innerRawTerms, Vector.empty, arity, 0).map(rt => parseTerm(rt, constructorsArity)),
          arity)
      }
      else if (variables.contains(toIdentify)) {
        Variable(toIdentify)
      }
      else { // suppose unknown term is a constructor. need to find its' arity
        parseTerm(rawTerm, constructorsArity + (toIdentify -> getConstructorArity(rawTerm.trim)))
      }
    }

    def customBraceStrip(rule: String): String = {
      if (rule.head == '(' && rule(rule.length - 1) == ')') {
        rule.stripPrefix("(").stripSuffix(")")
      }
      else {
        rule
      }
    }

    def createTrs(rawTrs: List[String], constructorsArity: Map[String, Int])(trs: Trs): Trs = {
      rawTrs match {
        case Nil => trs
        case headRule :: tailRules =>
          val splittedRawRule = customBraceStrip(headRule).split("""\s*:?:?=\s""")
          ???
      }
    }

    new Trs(rawTrs.map(rawRule => {
      val splittedRawRule = customBraceStrip(rawRule).split("""\s*:?:?=\s""")
      Rule(parseTerm(splittedRawRule(0)), parseTerm(splittedRawRule(1)))
    }).toVector) // нужно оптимизировать, сохраняя найденную арность конструкторов. как вариант, просто сделать мутабельную мапу constructorsArity.
  }

  def parse(rawInput: String): (Trs, Set[String]) = {
    val splitted =
      rawInput
        .split("\n")
        .filter(_.nonEmpty)
        .map(_.trim)
        .toVector

    val variablesRegex: Regex = """variables\s*=\s*(\s|\w|,)*""".r
    if (variablesRegex.findAllIn(splitted.head).length != 1) {
      throw new Exception("parsing error")
    }
    val variablesData: Set[String] = extractVariables(splitted.head)

    val trs: Trs = extractTrs(splitted.tail, variablesData)
    (trs, variablesData)
  }
}
