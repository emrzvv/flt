import model.{Trs, Unfolder}

import java.io.{File, PrintWriter}

object SmtWriter {
  implicit class StringOps(value: String) {
    def toAssert: String = "(assert " + value + ")"
    def toDeclareFun: String = "(declare-fun " + value + " () Int)"
  }

  def transformToSmt2(trs: Trs): String = {
    val filename = "expressions.smt2"
    val file = new File(s"./$filename")
    val pw = new PrintWriter(file)

    val unfolded = trs.rules.flatMap(rule => InequalitiesMaker.make(Unfolder.unfold(rule.left), Unfolder.unfold(rule.right))).map(_.toAssert)
    val funs = Unfolder.allConstructors.flatMap(constr => (0 to constr.arity).map(i => s"${constr.name}_$i".toDeclareFun))

    pw.println("(set-logic QF_NIA)")
    funs.foreach(pw.println)
    pw.println()
    unfolded.foreach(pw.println)
    pw.println()
    pw.println(InequalitiesMaker.notStrictMonotonicity(Unfolder.allConstructors.toVector).toAssert)
    pw.println(InequalitiesMaker.strictMonotonicity(Unfolder.allConstructors.toVector).toAssert)
    pw.println("(check-sat)")
    pw.println("(get-model)")
    pw.println("(exit)")
    pw.close()
    filename
  }
}
