package fpinscala.datastructures

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {

  def tail[A](xs: List[A]): Option[List[A]] = xs match {
    case Nil => None
    case Cons(_,ys) => Some(ys)
  }

  def drop[A](l: List[A], n: Int): List[A] =
    if (n <= 0)
      l
    else {
      val tl = tail(l)
      tl match {
        case None => Nil
        case Some(tls) => drop(tls, n - 1)
      }
    }

  def dropWhile[A](l: List[A])(f: A => Boolean): List[A] =
    l match {
      case Nil => Nil
      case Cons(x,xs) =>
        if (f(x))
          dropWhile(xs)(f)
        else
          l
  }

  def setHead[A](x: A, xs: List[A]): List[A] = tail(xs) match {
    case None => Cons(x, Nil)
    case Some(ys) => Cons(x, ys)
  }

  def init[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(x,Nil) => Nil
    case Cons(x,xs) => Cons(x,init(xs))
  }

  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x,xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(x,xs) => x * product(xs)
  }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B =
    as match {
      case Nil => z
      case Cons(x,xs) => f(x, foldRight(xs, z)(f))
    }

  def foldLeft[A,B](as: List[A], z: B)(f: (B, A) => B): B = {
    def aux(as: List[A], acc: B): B =
      as match {
        case Nil => acc
        case Cons(x,xs) => aux(xs, f(acc, x))
      }

    aux(as, z)
  }

  def sumLF(ns: List[Int]): Int = foldLeft(ns, 0)(_ + _)
  def productLF(ns: List[Double]): Double = foldLeft(ns, 1.0)(_ * _)
  def lengthLF[A](ns: List[A]): Int = foldLeft(ns, 0)((x,y) => x + 1)

  def append[A](xs: List[A], ys: List[A]): List[A] = xs match {
    case Nil => ys
    case Cons(x,xs) => Cons(x, append(xs, ys))
  }

  def reverse[A](xs: List[A]): List[A] =
    foldRight(xs, Nil: List[A])((a,b) => append(b,List(a)))
  def reverseLF[A](xs: List[A]): List[A] =
    foldLeft(xs, Nil: List[A])((b,a) => append(List(a),b))


  def flatten[A](xss: List[List[A]]): List[A] = xss match {
    case Nil => Nil
    case Cons(Nil,yss) => flatten(yss)
    case Cons(Cons(x,xs),yss) => Cons(x, flatten(Cons(xs, yss)))
  }

//  def foldLeftRF[A,B](as: List[A], z: B)(f: (B, A) => B): B =

//  def foldRightLF[A,B](as: List[A], z: B)(f: (A, B) => B): B =


  def sum2(ns: List[Int]): Int = foldRight(ns, 0)(_ + _)
  def product2(ns: List[Double]): Double = foldRight(ns, 1.0)(_ * _)
  def product2b(ns: List[Double]): Double = foldRight(ns, 1.0)((x,y) =>
    if (x == 0.0)
      0.0
    else
      x * y)

  def length[A](as: List[A]): Int = foldRight(as, 0)((x,y) => 1 + y)

  def map[A,B](xs: List[A])(f: A => B): List[B] = xs match {
    case Nil => Nil
    case Cons(x,ys) => Cons(f(x), map(ys)(f))
  }

  def addOneToEach(xs: List[Int]): List[Int] = map(xs)(_ + 1)
  def doublesToStrings(xs: List[Double]): List[String] = map(xs)(_.toString)

  def filter[A](as: List[A])(f: A => Boolean): List[A] =
    foldRight(as, Nil: List[A])((a,b) => if (f(a)) Cons(a,b) else b)

  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] = as match {
    case Nil => Nil
    case Cons(x,ys) => append(f(x),flatMap(ys)(f))
  }

  def filterFM[A](as: List[A])(f: A => Boolean): List[A] =
    flatMap(as)(a => if (f(a)) List(a) else Nil)

  def zipWith[A,B,C](as: List[A], bs: List[B])(f: (A,B) => C): List[C] =
    as match {
      case Nil => Nil
      case Cons(a, aas) => bs match {
        case Nil => Nil
        case Cons(b, bbs) => Cons(f(a, b), zipWith(aas, bbs)(f))
      }
    }

  def take[A](as: List[A], n: Int): List[A] = if (n <= 0) Nil else as match {
    case Nil => Nil
    case Cons(a,aas) => Cons(a, take(aas, n - 1))
  }

  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean =
    sup match {
      case Nil => sup == sub
      case Cons(x,xs) =>
        if (take(sup, length(sub)) == sub) true
        else hasSubsequence(xs, sub)
    }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
}
