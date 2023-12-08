import grammar.CFG

object Main {
  def main(args: Array[String]): Unit = {
    val rules = Vector(
      "A -> BCD",
      "B -> b|u|i|o",
      "B -> Eb",
      " B -> F",
      "B ->&",
      "C -> c",
      "C ->&",
      "D -> d",
      "D ->&",
      "E -> e",
      "E ->&",
      " F -> C",
      "F -> f",
      " F ->&"
    )
    val rules2 = "S -> aA | bB | dC\nA -> aA | bA | d\nB -> bB | d\nC -> d"
    val rules3 = "S -> AB | aS\nA -> eAc | d | &\nB -> cB | b | &"
    val rules4 = "E->A\nE->L\nA->n\nA->id\nL->(S)\nS->E,S\nS->E"
    val rules5 = "E  -> TQ\nQ -> +T Q|&\nT  -> F D\nD -> *F D | &\nF  -> (E) | i"
    val rules6 = "S -> ACB | Cbb | Ba\nA -> da | BC\nB -> g | &\nC -> h | &"
    val cfg = CFG(rules5)
    println(cfg.toString)
    pprint.pprintln(cfg)
//    println("liNe" take 2, "liNe" drop (2 + 1))
  }
}