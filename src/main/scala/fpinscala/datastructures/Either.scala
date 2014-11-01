package fpinscala.datastructures

sealed trait Either[+E, +A] {

  def map[B](f: A => B): Either[E, B] = this flatMap { a => Right(f(a)) }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
    case Left(e) => Left(e)
    case Right(v) => f(v)
  }

  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
    case Left(_) => b
    case _ => this
  }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
    for { x <- this; y <- b } yield f(x, y)

}
case class Left[+E](value: E) extends Either[E, Nothing]
case class Right[+A](value: A) extends Either[Nothing, A]

object Either {

  def mean(xs: IndexedSeq[Double]): Either[String, Double] =
    if (xs.isEmpty)
      Left("mean of empty list!")
    else
      Right(xs.sum / xs.length)

  def safeDiv(x: Int, y: Int): Either[Exception, Int] =
    try Right(x / y)
    catch { case e: Exception => Left(e) }

  def Try[A](a: => A): Either[Exception, A] =
    try Right(a)
    catch { case e: Exception => Left(e) }

  def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
    List.foldLeft(as, Right(Nil): Either[E, List[B]])((b,a) => f(a) match {
      case e@Left(_) => e
      case Right(v) => b map (Cons(v,_))
    }) map List.reverse

  def sequence[E, A](es: List[Either[E,A]]): Either[E, List[A]] =
    traverse(es)(identity)
}
