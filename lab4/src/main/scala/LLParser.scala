import scala.annotation.tailrec
import scala.collection.mutable

object LLParser {

  def apply(grammar: CFG): LLParser = {
    val firsts = Firsts(grammar)

    val follows = Follows(grammar, firsts)

    val table = grammar.rules.flatMap(rule => {
      val (hasEpsilon, ruleFirsts) = firsts.get(rule.elements.toList)
      val linesFromFirsts: Set[(String, (String, Rule))] = ruleFirsts.map(f => {
        rule.name -> (f, rule)
      })
      val linesFromFollows = if (hasEpsilon) {
        follows(rule.name).map(f => {
          rule.name -> (f.name, rule)
        })
      } else {
        Seq()
      }
      linesFromFirsts ++ linesFromFollows
    }).groupBy(_._1).view.mapValues(_.map(_._2).toMap) // check for conflicts in mapping. if conflict - cfg is not ll(1)

    new LLParser(grammar.start, table.toMap)
  }
}

case class Node(value: Element,
                parent: Option[Node] = None,
                children: mutable.ArrayBuffer[Node] = mutable.ArrayBuffer(),
                var position: Int = -1,
                var index: Int = -1,
                isCopied: Boolean = false) {
  def addChild(child: Node): Unit = children += child

  def rightSibling(): Option[Node] = {
    if (parent.isEmpty) {
      None
    } else if (parent.exists(_.children.length - 1 == this.index)) {
      parent.flatMap(_.rightSibling())
    } else {
      parent.map(_.children(this.index + 1))
    }
  }

  @tailrec
  final def pushPosition(pos: Int): Unit = {
    this.position = pos
    if (this.parent.exists(_.position != -1)) {
      this.parent.get.pushPosition(pos)
    }
  }

  def deduceNodePosition(): Int = {
    if (this.position != -1) this.position
    else {
      val rightSibling = this.rightSibling()
      rightSibling match {
        case Some(rs) =>
          val result = rs.deduceNodePosition()
          this.pushPosition(result)
          result
        case None => -1
      }
    }
  }
}

object Node {
  def printTree(node: Node, depth: Int = 0): Unit = {
    println("  " * depth + node.value + s"index: ${node.index}; position: ${node.position}")
    for (child <- node.children.reverse) {
      printTree(child, depth + 1)
    }
  }
}

class LLParser(start: String,
               val table: Map[String, Map[String, Rule]]) {

  def parseToTree(input: List[String], lastParsedPos: Int): Option[Node] = {
    val deque = mutable.ArrayDeque[Node]()
    val inputBuffer = mutable.Queue(input: _*)
    val rootNode = Node(NonTerm(start))

//    stack.push(Term(EndMarker.name))
    deque += rootNode

    val q = mutable.ArrayDeque[Node]()
    var i = 1
    while (deque.nonEmpty && i <= input.length) {
      println(s"[DEQUE]: ${deque.map(_.value.name)}")
      println(inputBuffer)
      val currentNode = deque.removeHead()
      currentNode.value match {
        case Term(value) =>
          if (value != Epsilon.name) {
            currentNode.pushPosition(i + lastParsedPos)
            i += 1
            if (value == EndMarker.name) {
              return Some(rootNode)
            }
            println(s"adding term: ${value}")
            inputBuffer.dequeue()
          } else if (currentNode.parent.exists(_.children.length == 1)) {
            q += currentNode
          }
        case NonTerm(value) =>
          println(s"${currentNode.value.name} -- ${inputBuffer.head}")
          val nextStack = table(currentNode.value.name).getOrElse(inputBuffer.head, Rule("UNKNOWN", Seq.empty))

          if (nextStack.name == "UNKNOWN") {
            throw new Exception("word is not in the language")
          }

          val buffer = mutable.ArrayBuffer[Node]()

          for (symbol <- nextStack.elements.reverse) {
            val newNode = Node(symbol, Some(currentNode), index = i)

            // newNode.index = i
            buffer += newNode
            deque.prepend(newNode)
          }

          for (b <- buffer.reverse) {
            currentNode.children += b
          }
//          val parentNode = Node(currentSymbol.name)
//          println(s"[CURRENT PARENT]: ${currentParent}")
//          println(s"PARENT NODE: ${parentNode}")
//          println(s"RULE: $rule")
//
//          if (rule.elements.isEmpty) { // eps
//            parentNode.addChild(Node(Epsilon.name))
//          } else {
//            for (symbol <- rule.elements.reverse) {
//              stack.push(symbol)
//            }
//          }
//
//          println(s"NEW STACK: ${stack}")
//          println()
//
//          currentParent.addChild(parentNode)
//          var finish = false
//          while (stack.nonEmpty && !finish) {
//            stack.top match {
//              case Term(value) => {
//                finish = true
//                if (value == Epsilon.name) {
//                  val epsNode = Node(stack.pop().name)
//                  parentNode.addChild(epsNode)
//                  currentParent.addChild(parentNode)
//                } else {
//                  currentParent.addChild(parentNode)
//                  currentParent = parentNode
//                }
//              }
//              case NonTerm(value) =>
//                val ntNode = Node(stack.pop().name)
//                currentParent.addChild(ntNode)
//                currentParent = ntNode
//            }
//          }
      }
    }

    while (q.nonEmpty) {
      q.removeHead().deduceNodePosition()
    }

    Some(rootNode)
  }

  def incrementalParseToTree(w0: List[String], T0: Node, w1: List[String], isGreedy: Boolean = false): Node = {
    val deque = mutable.ArrayDeque[Node]()

    val prefixLength = getCommonPrefixLength(w0.toVector, w1.toVector)
    val suffixLength = getCommonPrefixLength(w0.toVector.reverse, w1.toVector.reverse)

    println(s"PREFIX LENGTH: ${prefixLength}")
    println(s"SUFFIX LENGTH: ${suffixLength}")

    val T1: Node = if (prefixLength == 0) {
      Node(NonTerm(start))
    } else {
      copyTree(prefixLength, T0, T0.parent, deque)
    }

    T1
  }

  private def getCommonPrefixLength(w0: Vector[String], w1: Vector[String]): Int = {
    w0.zip(w1).takeWhile(w => w._1 == w._2).length
  }

  private def copyTree(toPos: Int, from: Node, parent: Option[Node], deque: mutable.ArrayDeque[Node]): Node = {
    val newNode = Node(
      value = from.value,
      parent = parent,
      children = mutable.ArrayBuffer[Node](),
      position = from.position,
      index = from.index,
      isCopied = true
    )

    if (from.position > toPos || from.position == -1) {
      newNode.position = -1
      deque += newNode
      newNode
    } else {
      for (child <- from.children) {
        newNode.children += copyTree(toPos, child, Some(newNode), deque)
      }
      newNode
    }
  }
}



