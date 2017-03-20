package freer

import cats.implicits._
import freer.ReaderEffect._
import org.typelevel.discipline.specs2.Discipline
import cats.laws.discipline._
import org.specs2.Specification

class ReaderEffectSpec extends Specification with Discipline { def is = s2"""
  runReader provides same value multiple times    $testRunReader
  feedReader provides values from list            $testFeedReader
  """

  def replicate[A](n: Int, a: A): List[A] = List.fill(n)(a)

  val addGet: Int => It[Int, Int] =
    x => ask[Int].flatMap((i: Int) => pure(i + x))

  val addN: Int => It[Int, Int] =
    n => {
      val zero: Int => It[Int, Int] = pure
      val r = replicate(n, addGet).foldLeft(zero)(_ >>> _)
      r(0)
    }

  def testRunReader = runReader(1, addN(3)) must_=== 3
  def testFeedReader = feedReader(1 :: 2 :: 3 :: Nil, addN(3)) must_=== 6

//  checkAll("It", MonadTests[It[Int, ?]].monad[Int, Int, Int])
}
