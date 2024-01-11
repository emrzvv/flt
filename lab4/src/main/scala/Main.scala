import grammar.CFG.toParseTable
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
        pprint.pprintln(toParseTable(cfg))
      }
    }

  }
}