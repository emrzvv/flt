package garbage

import scala.annotation.tailrec
import scala.collection.mutable

case class Tree(root: Node)

object Tree {
  def apply(parser: LL1Parser, input: String): Node = {
    @tailrec
    def propogatePosition(current: Node, pos: Int): Unit = {
      current.pos = pos
      if (current.parent.exists(_.pos == -1)) {
        propogatePosition(current.parent.get, pos)
      }
    }

    def deduceNodePosition(node: Node): Int = {
      if (node.pos != -1) {
        node.pos
      } else {
        val right = node.getRightSibling()
        right match {
          case Some(r) =>
            val res = deduceNodePosition(r)
            propogatePosition(node, res)
            res
          case None => -1
        }
      }
    }

    def build(w: String, lastParsedPos: Int, n: Int, deque: mutable.ArrayDeque[Node]): Unit = {
      parser.d = deque
      val currentDeque = mutable.ArrayDeque[Node]()

      val wNormalized = w.filterNot(_.isWhitespace).mkString("") + EOL

      var wCurrentPos = 0
      var i = 1

      while (i <= n && parser.d.nonEmpty) {
        println(s"i: ${i}")
        val front = parser.d.removeHead()
//        println(s"FRONT: ${front}")
        if (parser.terminals.contains(front.name)) {
          if (front.name != Eps) {
            propogatePosition(front, i + lastParsedPos)
            i += 1
            if (front.name == EOL) {
              return
            } else {
              wCurrentPos += 1
            }
          } else if (front.parent.exists(_.children.length == 1)) {
            currentDeque += front
          } else ()
        } else {
          val next = parser.table(front.name)(wNormalized(wCurrentPos).toString).right
          println(s"[NEXT]: ${next}")
          if (next.isEmpty) {
            throw new Exception("w is not in language")
          } else {
//            val buffer = mutable.ArrayBuffer[Node]()

//            next.reverse.zipWithIndex.foreach { it =>
//              val node = Node(it._1, Some(front))
//              node.index = it._2
//              buffer += node
//              parser.d.prepend(node)
//            }
//
//            buffer.reverse.foreach { it =>
//              front.children += it
//            }
            println("CONSTRUCTING BUFFER")
            val buffer = next.reverse.map { it =>
              val node = Node(it, Some(front))
              node.index = i
              parser.d.prepend(node)
              node
            }
            println(parser.d)
            println(s"buf size: ${buffer.size}")
            front.children ++= buffer.reverse
          }
        }
      }

      while (currentDeque.nonEmpty) {
        deduceNodePosition(currentDeque.removeHead())
      }
    }

    val root = Node("E'", None)
    val d = mutable.ArrayDeque[Node]()
    d += root

    build(input, 0, 9999, d)
    root
  }
}


