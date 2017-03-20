package freer

import cats.implicits._
import freer.ReaderEffect._

class ReaderEffectSpec extends org.specs2.mutable.Specification {

  def replicate[A](n: Int, a: A): List[A] = List.fill(n)(a)

  val addGet: Int => It[Int, Int] =
    x => ask[Int].flatMap((i: Int) => pure(i + x))

  val addN: Int => It[Int, Int] =
    n => {
      val zero: Int => It[Int, Int] = pure
      val functions: List[Int => It[Int, Int]] = replicate(n, addGet)
      val r = functions.foldLeft(zero)(_ >>> _)
      r(0)
    }

  "runReader provides same value multiple times" in {
    runReader(1, addN(3)) must_=== 3
  }

  "feedReader provides values from list" in {
    feedReader(1 :: 2 :: 3 :: Nil, addN(3)) must_=== 6
  }
}
