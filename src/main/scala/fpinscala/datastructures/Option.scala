package fpinscala.datastructures

sealed trait Option[+A] {

  def map[B](f: A => B): Option[B] = this match {
    case None => None
    case Some(a) => Some(f(a))
  }

  def flatMap[B](f: A => Option[B]): Option[B] = this map f getOrElse None

  def getOrElse[B >: A](default: => B): B = this match {
    case None => default
    case Some(a) => a
  }

  def orElse[B >: A](ob: => Option[B]): Option[B] = this match {
    case None => ob
    case _ => this
  }

  def filter(f: A => Boolean): Option[A] = map(f) match {
    case Some(true) => this
    case _ => None
  }
}

case object None extends Option[Nothing]
case class Some[+A](get: A) extends Option[A]

object Option {

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  def variance(xs: Seq[Double]): Option[Double] =
    for {
      m <- mean(xs)
      v <- mean(xs map (x => math.pow(x - m, 2.0)))
    } yield v

  def lift[A,B](f: A => B): Option[A] => Option[B] = _ map f

  def Try[A](a: => A): Option[A] =
    try Some(a)
    catch { case e: Exception => None }

  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A,B) => C): Option[C] =
    for {
      x <- a
      y <- b
    } yield f(x, y)

  def sequence[A](as: List[Option[A]]): Option[List[A]] =
    traverse(as)(identity)

  def traverse[A,B](as: List[A])(f: A => Option[B]): Option[List[B]] =
    List.foldLeft(as, Some(Nil): Option[List[B]])((b,a) => f(a) match {
      case None => None
      case Some(x) => b map (Cons(x,_))
    }) map List.reverse
}