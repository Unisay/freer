package freer

import cats._
import cats.implicits._

import scala.annotation.tailrec
import scala.language.higherKinds


object ReaderEffect {

  sealed trait It[I, A]
  final case class Pure[I, A](a: A) extends It[I, A]
  final case class Get[I, A](k: I => It[I, A]) extends It[I, A]

  def pure[I, A](a: A): It[I, A] = Pure(a)
  def get[I, A](k: I => It[I, A]): It[I, A] = Get(k)
  def ask[I]: It[I, I] = get(pure)

  @tailrec
  def runReader[I, A](i: I, it: It[I, A]): A = it match {
    case Pure(a) => a
    case Get(k) => runReader(i, k(i))
  }

  @tailrec
  def feedReader[I, A](is: List[I], it: It[I, A]): A = it match {
    case Pure(a) => a
    case Get(_) if is.isEmpty => sys.error("end of stream")
    case Get(k) => feedReader(is.tail, k(is.head))
  }

  implicit def monadIt[I]: Monad[It[I, ?]] = new Monad[It[I, ?]] {
    def pure[A](a: A): It[I, A] = pure(a)
    def flatMap[A, B](it: It[I, A])(k: A => It[I, B]): It[I, B] = it match {
      case Pure(a) => k(a)
      case Get(k1) => get(k1 >>> k)
    }
    @tailrec
    def tailRecM[A, B](a: A)(f: A => It[I, Either[A, B]]): It[I, B] = f(a) match {
      case Pure(Right(b)) => pure(b)
      case Pure(Left(a1)) => tailRecM(a1)(f)
      case Get(k) => ???
    }
  }

  implicit class KleisliComposition[A, B, C, M[_]: Monad](val f: A => M[B]) {
    def >>>(g: B => M[C]): A => M[C] = f(_).flatMap(g)
  }

}
