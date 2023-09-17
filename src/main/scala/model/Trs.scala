package model

class Trs(val rules: Vector[Rule]) { // TODO: поменять на Set
  override def toString: String = {
    rules.map(rule => rule.toString).foldLeft("\n")((acc, r) => acc + r)
  }
}
