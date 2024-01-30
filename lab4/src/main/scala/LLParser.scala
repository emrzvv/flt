import java.io.{File, FileOutputStream, PrintWriter}
import java.util.UUID
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
          println(s"right sibling to ${this.value.name} (${position}, ${index}) is ${rs.value.name} (${rs.position}, ${rs.index})")
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
}

object Node {
  def printTree(node: Node, depth: Int = 0): Unit = {
    println("  " * depth + node.value + s"index: ${node.index}; position: ${node.position}")
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
    val currentDefinition = s"${node.uuid.toString.quoted} [label=\"${node.value.name}\"];\n"
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
    nodes.foreach(n => println(n.value, n.parent.get.value, n.parent.get.parent.get.position, n.position, n.index))
    while (nodes.nonEmpty) {
      val removed = nodes.removeHead()
      println(removed.value, removed.parent.get.value, removed.parent.get.parent.get.position, removed.position, removed.index)
      println(removed.deduceNodePosition())
    }
  }


  def parseToTree(input: List[String], lastParsedPos: Int, parseLength: Int = Int.MaxValue): Option[Node] = {
    val deque = mutable.ArrayDeque[Node]()
    val inputBuffer = mutable.Queue(input: _*)
    val rootNode = Node(NonTerm(start), index = 0)

    deque += rootNode

    val epsNodes = mutable.ArrayDeque[Node]()
    var i = 1
    while (deque.nonEmpty && i <= parseLength) {
      println("=========")
      println(s"[DEQUE]: ${deque.map(_.value.name)}")
      println(s"[INPUT]: ${inputBuffer}")
      Node.printTree(rootNode)
      val currentNode = deque.removeHead()
      currentNode.value match {
        case Term(value) =>
          if (value != Epsilon.name) {
            Node.pushPosition(currentNode, i + lastParsedPos)
            i += 1
            if (value == EndMarker.name) {
              deduceEpsNodesPositions(epsNodes)
              return Some(rootNode)
            }
            println(s"adding term: ${value}")
            inputBuffer.dequeue()
          } else if (currentNode.parent.exists(_.children.length == 1)) {
            println("adding eps")
            epsNodes += currentNode
          }
        case NonTerm(value) =>
          println(s"${currentNode.value.name} -- ${inputBuffer.head}")
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

    val NmPos = w0.size - suffixLength + 1
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



