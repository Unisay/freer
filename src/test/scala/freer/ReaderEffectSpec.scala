package freer

import cats.kernel.Eq
import cats.implicits._
import org.scalacheck.Arbitrary.{arbitrary => arb}
import freer.ReaderEffect._
import org.typelevel.discipline.specs2.Discipline
import cats.laws.discipline._
import org.scalacheck.Gen.lzy
import org.scalacheck._
import org.specs2.Specification

class ReaderEffectSpec extends Specification with Discipline { def is = s2"""
  runReader provides same value multiple times    testRunReader
  feedReader provides values from list            testFeedReader
  It monad is lawful                              $testLawfulMonad
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

  implicit def eqIt[B: Eq]: Eq[It[Int, B]] = Eq.instance {
    case (Pure(b1), Pure(b2)) => b1 === b2
    case (Get(k1), Get(k2)) => k1(42) === k2(42)
    case _ => false
  }
  implicit def arbIt: Arbitrary[It[Int, Int]] = Arbitrary {
    Gen.choose(1, 3).map(addN)
//    arb[B].map(pure[A, B])
/*    Gen.frequency(
      100 -> arb[B].map(pure[A, B]),
      10 -> lzy(arb[It[A, B]].map((it: It[A, B]) => get((_: A) => it)))
    )*/
  }

  def testLawfulMonad = checkAll("It", FunctorTests[It[Int, ?]].functor[Int, Int, Int])
}
