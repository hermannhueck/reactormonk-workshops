package tut

object TutorialFold extends App {

  import cats._
  import cats.implicits._

  trait Fold[I, O] {
    type M
    def tally(i: I): M
    def summarize(m: M): O
    def monoid: Monoid[M]
  }

  object Fold {
    def runList[I, O](fold: Fold[I, O], data: List[I]) = {
      fold.summarize(
        data.map(i => fold.tally(i))
          .foldRight(fold.monoid.empty)((x, y) => fold.monoid.combine(x, y)) // or combineAll
      )
    }
  }

  object sumfold {
    val sum = new Fold[Int, Int] {
      type M = Int
      def tally(i: Int): M = i
      def summarize(sum: M): Int = sum
      def monoid: Monoid[Int] = new Monoid[M] {
        def empty: M = 0
        def combine(a: M, b: M): M = a + b
      }
    }
  }

  object maxfold {
    val sum = new Fold[Int, Option[Int]] {
      type M = Option[Int]
      def tally(i: Int): M = Some(i)
      def summarize(sum: M): M = sum
      def monoid: Monoid[M] = new Monoid[M] {
        def empty: M = None
        def combine(a: M, b: M): M = (a, b) match {
          case (Some(x), Some(y)) => Some(math.max(x, y))
          case (optX, None) => optX
          case (None, optY) => optY
          case _ => None
        }
      }
    }
  }

  object averagefold {
    val sum = new Fold[Double, Option[(Double, Int)]] {
      type M = Option[(Double, Int)]
      def tally(d: Double): M = Some((d, 1))
      def summarize(sum: M): Option[(Double, Int)] = sum.map { case (d, count) => (d/count, 1) }
      def monoid: Monoid[M] = new Monoid[M] {
        def empty: M = Option.empty[(Double, Int)]
        def combine(a: M, b: M): M = (a, b) match {
          case (Some((x, c1)), Some((y, c2))) => Some((x+y, c1+c2))
          case (optX, None) => optX
          case (None, optY) => optY
          case _ => None
        }
      }
    }
  }

  object wordcount {
    val wc = new Fold[String, Map[String, Int]] {
      type M = Map[String, Int]
      def tally(s: String): M = Map(s -> s.split("\\W").length)
      def summarize(sum: M): M = sum
      def monoid: Monoid[M] = new Monoid[M] {
        def empty: M = Map.empty
        def combine(a: M, b: M): M = a ++ b
      }
    }
  }

}