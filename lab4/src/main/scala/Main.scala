import grammar.{CFG, CFGTEST}
import utils.Control.using

import scala.io.Source

object Main {
  def main(args: Array[String]): Unit = {
    if (args.length > 0) {
      val input = args(0)
      using(Source.fromFile(input)) { source =>
        val rawGrammar = source.getLines().mkString("\n")
        val cfg = CFG(rawGrammar)

        pprint.pprintln(cfg)
        pprint.pprintln(cfg.predictSet)
        pprint.pprintln(cfg.followSet)
      }
    }

//    val rules = Vector(
//      "A -> BCD",
//      "B -> b|u|i|o",
//      "B -> Eb",
//      " B -> F",
//      "B ->&",
//      "C -> c",
//      "C ->&",
//      "D -> d",
//      "D ->&",
//      "E -> e",
//      "E ->&",
//      " F -> C",
//      "F -> f",
//      " F ->&"
//    )
//    val rules2 = "S -> aA | bB | dC\nA -> aA | bA | d\nB -> bB | d\nC -> d"
//    val rules3 = "S -> AB | aS\nA -> eAc | d | &\nB -> cB | b | &"
//    val rules4 = "E->A\nE->L\nA->n\nA->id\nL->(S)\nS->E,S\nS->E"
//    val rules5 = "E  -> TQ\n" +
//      "Q -> +T Q|&\n" +
//      "T  -> F D\n" +
//      "D -> *F D | &\n" +
//      "F  -> (E) | i"
//    val rules6 = "S -> ACB | Cbb | Ba\nA -> da | BC\nB -> g | &\nC -> h | &"
//    val rules7 = Vector(
//      "E -> T E'",
//      "E' -> + T E'",
//      "E' -> &",
//      "T -> F T'",
//      "T' -> * F T'",
//      "T' -> &",
//      "F -> ( E )",
//      "F -> int"
//    )
//    val cfg = CFG(rules2)
//
//    pprint.pprintln(cfg)
//    pprint.pprintln(cfg.predictSet)
//    pprint.pprintln(cfg.followSet)
  }
}