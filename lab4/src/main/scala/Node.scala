import scala.collection.mutable.ArrayBuffer

case class Node(name: String,
                parent: Option[Node],
                children: ArrayBuffer[Node],
                var pos: Int,
                var index: Int,
                inherited: Boolean) {
  def getRightSibling(): Option[Node] = {
    if (this.parent.isEmpty) None
    else if (this.parent.exists(_.children.length - 1 == this.index)) {
      this.parent.flatMap(_.getRightSibling())
    } else {
      this.parent.map(_.children(this.index + 1))
    }
  }
}

object Node {
  def apply(name: String, parent: Option[Node]): Node = {
    new Node(
      name = name,
      parent = parent,
      children = ArrayBuffer.empty,
      pos = -1,
      index = -1,
      inherited = false)
  }
}
