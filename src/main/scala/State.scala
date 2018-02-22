
case class State[S, A](run:S => (S, A)) {
  def exec(s: S): S = run(s)._1
  def eval(s: S): A = run(s)._2

  def pure[B](a: B): State[S, B] = State(s => (s, a))
  def flatMap[B](afb: A => State[S, B]): State[S, B] =
    State[S, B] { s =>
      val (ss, a) = run(s)
      afb(a).run(ss)
    }

  def map[B](ab: A => B): State[S, B] = State { s =>
    val (nS, a) = run(s)
    (nS, ab(a))
  }
}

object State {
  def get[S]: State[S, S] = State(s => (s, s))
}