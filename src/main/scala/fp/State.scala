package fp

import scala.language.{higherKinds, implicitConversions, postfixOps}

case class State[S, A](run:S => (S, A)) {
  def exec(s: S): S = run(s)._1
  def eval(s: S): A = run(s)._2

  def pure[B](a: B): State[S, B] = State(s => (s, a))
  def flatMap[B](f: A => State[S, B]): State[S, B] =
    State[S, B] { s =>
      val (ss, a) = run(s)
      f(a).run(ss)
    }

  def map[B](f: A => B): State[S, B] = State { s =>
    val (nS, a) = run(s)
    (nS, f(a))
  }
}

object State {
  def get[S]: State[S, S] = State(s => (s, s))
}
