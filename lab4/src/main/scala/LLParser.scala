import java.io.{File, FileOutputStream, PrintWriter}
import java.util.UUID
import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

object LLParser {

  def apply(grammar: CFG): LLParser = {
    val firsts = Firsts(grammar)

    val follows = Follows(grammar, firsts)

    val t: Set[(String, (String, Rule))] = grammar.rules.flatMap(rule => {
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
    })

    for ((_, rules) <- t.groupBy(_._1)) {
      val unique = rules.map(s => (s._1, s._2._1))
      if (unique.size != rules.size) throw new Exception("grammar is not LL(1)")
    }

    val table = t.groupBy(_._1).view.mapValues(_.map(_._2).toMap)

    new LLParser(grammar.start, table.toMap)
  }
}

case class Node(value: Element,
                var parent: Option[Node] = None,
                children: mutable.ArrayBuffer[Node] = mutable.ArrayBuffer(),
                var position: Int = -1,
                var index: Int = -1,
                isCopied: Boolean = false,
                uuid: UUID = UUID.randomUUID()) {
  def addChild(child: Node): Unit = children += child

  def rightSibling(): Option[Node] = {
    parent.flatMap { parentNode =>
      val siblings = parentNode.children
      if (siblings.nonEmpty && index < siblings.length - 1) {
        Some(siblings(index + 1))
      } else {
        parentNode.rightSibling()
      }
    }
  }

  def deduceNodePosition(): Int = {
    if (this.position != -1) this.position
    else {
      val rightSibling = this.rightSibling()
      rightSibling match {
        case Some(rs) =>
//          println(s"right sibling to ${this.value.name} (${position}, ${index}) is ${rs.value.name} (${rs.position}, ${rs.index})")
          val result = rs.deduceNodePosition()
          Node.pushPosition(this, result)
          result
        case None => -1
      }
    }
  }

  private def isEpsNonTermNode: Boolean = {
    children.size == 1 && children(0).value.name == Epsilon.name
  }

  def getNodeByPosition(pos: Int): Option[Node] = {
    if (this.position == pos) Some(this)
    else if (this.position > pos) None
    else {
      children
        .view
        .flatMap(_.getNodeByPosition(pos))
        .find(result => !result.isEpsNonTermNode)
    }
  }

  def reducePosition(pos: Int): Unit = {
    if (this.position != -1) {
      this.position -= pos
      this.children.foreach(child => child.reducePosition(pos))
    }
  }
}

object Node {
  def printTree(node: Node, depth: Int = 0): Unit = {
    println("  " * depth + node.value + s" index: ${node.index}; position: ${node.position}; copied: ${node.isCopied}")
    for (child <- node.children) {
      printTree(child, depth + 1)
    }
  }

  def pushPosition(node: Node, pos: Int): Unit = {
    var current = node
    current.position = pos
    while (current.parent.isDefined && current.parent.get.position == -1) {
      current = current.parent.get
      current.position = pos
    }
  }

  implicit class StringOps(value: String) {
    def quoted = s"\"$value\""
  }

  private def toGraphvizHelper(node: Node): String = {
    val color = if (node.isCopied) ", color=green" else ""
    val currentDefinition = s"${node.uuid.toString.quoted} [label=\"${node.value.name}\n(${node.position}, ${node.index})\"${color}];\n"
    val edges = node.children.map(child => s"${node.uuid.toString.quoted} -- ${child.uuid.toString.quoted}").mkString("\n")
    currentDefinition + edges + "\n" + node.children.map(child => toGraphvizHelper(child)).mkString("\n")
  }

  def toGraphviz(node: Node, expression: String, output: String): Unit = {
    val pw = new PrintWriter(new FileOutputStream(new File(output)))
    val result = toGraphvizHelper(node)
    pw.write(
      s"""
         |graph \"\"
         |{
         |  fontname="Helvetica,Arial,sans-serif"
         |  node [fontname="Helvetica,Arial,sans-serif"]
         |  edge [fontname="Helvetica,Arial,sans-serif"]
         |  label="${expression}"
         |  ${result}
         |}
         |""".stripMargin)
    pw.close()
  }
}

class LLParser(start: String,
               val table: Map[String, Map[String, Rule]]) {
  private def deduceEpsNodesPositions(nodes: mutable.ArrayDeque[Node]): Unit = {
//    nodes.foreach(n => println(n.value, n.parent.get.value, n.parent.get.parent.get.position, n.position, n.index))
    while (nodes.nonEmpty) {
      val removed = nodes.removeHead()
//      println(removed.value, removed.parent.get.value, removed.parent.get.parent.get.position, removed.position, removed.index)
//      println(removed.deduceNodePosition())
    }
  }


  def parseToTreeDefault(input: List[String]): Option[Node] = {
    val root = Node(NonTerm(start), index = 0)
    val deque = mutable.ArrayDeque[Node]()
    deque += root

    parseToTree(input, 0, Int.MaxValue, deque)
    Some(root)
  }

  def parseToTree(input: List[String], lastParsedPos: Int, parseLength: Int = Int.MaxValue, deque: mutable.ArrayDeque[Node] = mutable.ArrayDeque()): Unit = {
    val inputBuffer = mutable.Queue(input: _*)

    val epsNodes = mutable.ArrayDeque[Node]()
    var i = 1
    while (deque.nonEmpty && i <= parseLength) {
//      println("=========")
//      println(s"[DEQUE]: ${deque.map(_.value.name)}")
//      println(s"[INPUT]: ${inputBuffer}")
      val currentNode = deque.removeHead()
      currentNode.value match {
        case Term(value) =>
          if (value != Epsilon.name) {
            Node.pushPosition(currentNode, i + lastParsedPos)
            i += 1
            if (value == EndMarker.name) {
              deduceEpsNodesPositions(epsNodes)
            }
//            println(s"adding term: ${value}")
            inputBuffer.dequeue()
          } else if (currentNode.parent.exists(_.children.length == 1)) {
//            println("adding eps")
            epsNodes += currentNode
          }
        case NonTerm(value) =>
//          println(s"${currentNode.value.name} -- ${inputBuffer.head}")
          val nextStack = table(currentNode.value.name).getOrElse(inputBuffer.head, Rule("UNKNOWN", Seq.empty))

          if (nextStack.name == "UNKNOWN") {
            throw new Exception("word is not in the language")
          }

          val updatedNextStack = if (nextStack.elements.isEmpty) {
            nextStack.copy(elements = Seq(Term(Epsilon.name)))
          } else nextStack

          val buffer = mutable.ArrayBuffer[Node]()

          for ((symbol, j) <- updatedNextStack.elements.zipWithIndex.reverse) {
            val newNode = Node(symbol, Some(currentNode), index = j)
            buffer += newNode
            deque.prepend(newNode)
          }

          for (b <- buffer.reverse) {
            currentNode.children += b
          }
      }
    }
    deduceEpsNodesPositions(epsNodes)
  }

  def incrementalParseToTree(w0Input: List[String], T0: Node, w1Input: List[String]): Node = {
    @tailrec
    def loop(NmNode: Option[Node], NmPosition: Int, NmRootPosition: Int, amountToParse: Int, lastParsedPos: Int)
            (w1: mutable.ArrayBuffer[String])
            (T1: Node, T0: Node)
            (deque: mutable.ArrayDeque[Node]): Node = {
//      println("INCREMENTAL PARSING:")
//      println(s"${w1.toList}, $lastParsedPos, $amountToParse, ${deque.map(_.value.name)}")
      parseToTree(w1.toList, lastParsedPos, amountToParse, deque)

      val updatedLastParsedPos = lastParsedPos + amountToParse

      if (amountToParse >= w1.size) {
        T1
      } else {
        val updatedW1 = w1.slice(amountToParse, w1.size)
        val NmRootOpt = T1.getNodeByPosition(NmRootPosition)
        NmRootOpt match {
          case None =>
            throw new Exception("w1 is not in the language")
          case Some(_NmRoot: Node) =>
//            println("NMROOT")
//            println(Node.printTree(_NmRoot))
//            println()
            if (NmNode.exists(_.value.name == _NmRoot.value.name)) {
//              println(s"COPYING!!!")
              val NmCopy = copyTree(Int.MaxValue, NmNode.get).get
              _NmRoot.parent.get.children(_NmRoot.index) = NmCopy
              NmCopy.parent = _NmRoot.parent
              NmCopy.index = _NmRoot.index

              val prevNmPosition = NmNode.get.position

              val positionToReduce = NmCopy.position - _NmRoot.position
              NmCopy.reducePosition(positionToReduce)

              val updatedNmNode = NmNode.flatMap(_.rightSibling())

              val (updatedNmPos, updatedRootPos, updatedAmountToParse) = updatedNmNode match {
                case Some(rs) => (NmPosition + amountToParse, NmRootPosition + amountToParse, rs.position - prevNmPosition)
                case None => (NmPosition + amountToParse, NmRootPosition + amountToParse, w1.size)
              }

              loop(
                NmNode = updatedNmNode,
                NmPosition = updatedNmPos,
                NmRootPosition = updatedRootPos,
                amountToParse = updatedAmountToParse,
                lastParsedPos = updatedLastParsedPos
              )(updatedW1)(T1, T0)(deque)
            } else {
              loop(
                NmNode = T0.getNodeByPosition(NmPosition + 1),
                NmPosition = NmPosition + 1,
                NmRootPosition = NmRootPosition + 1,
                amountToParse = 1,
                lastParsedPos = updatedLastParsedPos)(updatedW1)(T1, T0)(deque)
            }
        }
      }
    }

    val deque = mutable.ArrayDeque[Node]()

    var w0: mutable.ArrayBuffer[String] = ArrayBuffer.from(w0Input).dropRight(1)
    var w1: mutable.ArrayBuffer[String] = ArrayBuffer.from(w1Input).dropRight(1)

    val prefixLength = getCommonPrefixLength(w0.toVector, w1.toVector)
    val suffixLength = getCommonPrefixLength(w0.toVector.reverse, w1.toVector.reverse)

//    println(s"PREFIX LENGTH: ${prefixLength}")
//    println(s"SUFFIX LENGTH: ${suffixLength}")

    val T1: Node = if (prefixLength == 0) {
      val temp = Node(NonTerm(start))
      deque += temp
      temp
    } else {
      copyTree(prefixLength, T0, deque).get
    }

//    deque.foreach(n => println(n.value))
    val NmPosition = w0.size - suffixLength + 1
    val NmNode = T0.getNodeByPosition(NmPosition)

    val NmRootPosition = w1.size - suffixLength + 1
    val amountToParse = w1.size - suffixLength - prefixLength + 1

    w1 = w1.slice(prefixLength, w1.length)
    w1 += EndMarker.name

//    println(w1.size, prefixLength, w1.size - suffixLength - prefixLength + 1)

    loop(
      NmNode = NmNode,
      NmPosition = NmPosition,
      NmRootPosition = NmRootPosition,
      amountToParse = amountToParse,
      lastParsedPos = prefixLength
    )(w1)(T1, T0)(deque)
  }

  private def getCommonPrefixLength(w0: Vector[String], w1: Vector[String]): Int = {
    w0.zip(w1).takeWhile(w => w._1 == w._2).length
  }

  private def copyTree(toPos: Int, from: Node, deque: mutable.ArrayDeque[Node] = mutable.ArrayDeque.empty): Option[Node] = {
    if (toPos <= 0) None
    else Some(copyTreeLoop(toPos, from, None, deque))
  }

  private def copyTreeLoop(toPos: Int, from: Node, parent: Option[Node], deque: mutable.ArrayDeque[Node] = mutable.ArrayDeque.empty): Node = {
    val newNode = Node(
      value = from.value,
      parent = parent,
      children = mutable.ArrayBuffer[Node](),
      position = from.position,
      index = from.index,
      isCopied = true,
      uuid = from.uuid
    )

    if (from.position > toPos || from.position == -1) {
      newNode.position = -1
      deque += newNode
      newNode
    } else {
      for (child <- from.children) {
        newNode.children += copyTreeLoop(toPos, child, Some(newNode), deque)
      }
      newNode
    }
  }
}



