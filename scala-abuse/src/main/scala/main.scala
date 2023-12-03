import scala.annotation.tailrec

@main
def main(): Unit = {
  println("Hello world!")
}

object Abuse:
  case class State(x: Int)

  def foo[T](state: State)(body: => T): Option[T] =
    Option.when(state.x == 0)(body)

  var bar = 0
  val state = State(0)

  /*
    перед `:` на 27й строке должна быть закрывающая скобка.
    компилятор ругается на эту строку, но выявляет там type error, а не syntax error.
    в качестве syntax error он указывает 30ю строку, что неверно.
   */

  def app: Function1[Int, Unit] =
    new Function1[Int, Unit]:
      def apply(x: Int): Unit =
        foo(state):
          foo(state.copy(x = 5) : // пропущена `)` перед `:`
            println("a")
            bar = 2
