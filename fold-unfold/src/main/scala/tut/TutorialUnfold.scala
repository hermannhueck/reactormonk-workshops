package tut

object TutorialUnfold extends App {

  //import cats._
  //import cats.implicits._

  def unfold[A, B](initial: A)(fun: A => Option[(A, B)]): Stream[B] =
    fun(initial).map { case (a, b) =>
      b #:: unfold(a)(fun)
    }.getOrElse(Stream.empty)

  def unfold0[A](initial: A)(fun: A => Option[A]): Stream[A] =
    unfold(initial)({ state =>
      fun(state).map ({ s => (s, s) })
    })

  def unfoldE[A, B](initial: A)(fun: A => Stream[Either[A, B]]): Stream[B] = {
    fun(initial).flatMap {
      case Left(a) => unfoldE(a)(fun)
      case Right(b) => Stream(b)
    }
  }

  println
  //## Leap Years
  def leapYears(from: Int): Stream[Int] = {
    unfold0(from)(year => Some(year + 1)).filter { year =>
      (year % 4 == 0) && (!(year % 100 == 0) || (year % 400 == 0))
    }
  }
  // divisible by 4 / not divisble by 100 / divisible by 400

  println(leapYears(1990).take(8).toList)

  println
  //## Hailstone
  def hailstone(from: Int): Stream[Int] = {
    unfold0(from)({
      case 1 => None
      case x if x % 2 == 0 => Some(x/2)
      case x => Some(x * 3 + 1)
    })
  }
  // if even, n/2, if odd, n * 3 + 1, abort on 1

  println(hailstone(12).toList)

  println
  // ## Fibonacci
  val fibonacci: Stream[Int] = unfold((1, 1))({
    case (prev, current) =>
      Some(((current, prev + current), prev))
  })
  // n(i+2) = n(i+1) + n(i)

  println(fibonacci.take(10).toList)
}