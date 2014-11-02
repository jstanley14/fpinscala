package fpinscala.datastructures.Stream

sealed trait Stream[+A] {

  def toList: List[A] = this match {
    case Empty => List.empty[A]
    case Cons(h, tl) => h() :: tl().toList
  }

  def take(n: Int): Stream[A] = this match {
    case Empty => this
    case Cons(h, tl) => if (n <= 0) Empty
                        else Cons(h, () => tl() take (n - 1))
  }

  def drop(n: Int): Stream[A] = this match {
    case Empty => this
    case Cons(h, tl) => if (n <= 0) this
                        else tl() drop (n - 1)
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Empty => this
    case Cons(h, tl) => if (p(h())) Cons(h, () => tl() takeWhile p)
                        else Empty
  }

  def exists(p: A => Boolean): Boolean = this match {
    case Empty => false
    case Cons(h, tl) => p(h()) || tl().exists(p)
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Empty => z
    case Cons(h, t) => f(h(), t().foldRight(z)(f))
  }

  def exists2(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b)

  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((a, b) => p(a) && b)

  def takeWhile2(p: A => Boolean): Stream[A] =
    foldRight(Empty: Stream[A])((a, b) => if (p(a)) Stream.cons(a, b) else Empty)

  def headOption: Option[A] = foldRight(None: Option[A])((a, _) => Some(a))

  def map[B](f: A => B): Stream[B] =
    foldRight(Empty: Stream[B])((a, b) => Stream.cons(f(a), b))

  def filter(p: A => Boolean): Stream[A] =
    foldRight(Empty: Stream[A])((a, b) => if (p(a)) Stream.cons(a, b) else b)

  def append[B >: A](ys: Stream[B]): Stream[B] =
    foldRight(ys)(Stream.cons(_, _))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(Empty: Stream[B])((a, b) => f(a) append b)

  def find(p: A => Boolean): Option[A] = filter(p).headOption

  def map2[B](f: A => B): Stream[B] =
    Stream.unfold(this)(s => s.headOption.map(x => (f(x), s drop 1)))

  def take2(n: Int): Stream[A] = Stream.unfold((this, n)) { case (xs, n) =>
    if (n <= 0) None
    else xs.headOption map (x => (x, (xs drop 1, n - 1)))
  }

  def takeWhile3(p: A => Boolean): Stream[A] =
    Stream.unfold(this)(xs => xs.headOption flatMap (x => if (p(x)) Some(x, xs drop 1)
                                                          else None))

  def zipWith[B,C](bs: Stream[B])(f: (A, B) => C): Stream[C] =
    Stream.unfold((this, bs)) {
      case (Cons(a,as), Cons(b,bs)) => Some(f(a(), b()), (as(), bs()))
      case _ => None
    }

  def zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] =
    Stream.unfold((this, s2)) {
      case (Cons(a,as), Cons(b,bs)) => Some((Some(a()), Some(b())), (as(), bs()))
      case (Cons(a,as), Empty) => Some((Some(a()), Option.empty[B]), (as(), Stream.empty[B]))
      case (Empty, Cons(b,bs)) => Some((Option.empty[A], Some(b())), (Stream.empty[A], bs()))
      case _ => None
    }

  def startsWith[A](s: Stream[A]): Boolean =
    zipAll(s) takeWhile3(_._2.nonEmpty) forAll {
      case (x, y) => x == y
    }

  def tails: Stream[Stream[A]] = Stream.unfold(this) {
    case s@Cons(x,xs) => Some(s, xs())
    case _ => None
  }

  def hasSubsequence[A](s: Stream[A]): Boolean =
    tails exists (_ startsWith s)

  def scanRight[B](z: B)(f: (A, => B) => B): Stream[B] =
    foldRight((z, Stream(z))) {
      case (a, (b, acc)) =>
        val next = f(a, b)
        (next, Stream.cons(next, acc))
    }._2
}

case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, tl: () => Stream[A]) extends Stream[A]

object Stream {

  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))

  def constant[A](a: A): Stream[A] = unfold(a)(Some(_,a))

  def from(n: Int): Stream[Int] = unfold(n)(n => Some(n, n + 1))

  def ones: Stream[Int] = constant(1)

  def fibs: Stream[Int] =
    Stream(0, 1) append unfold((0,1)) { case (fst, snd) =>
      val nxt = fst + snd
      Some(nxt, (snd, nxt)) }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case Some((a,s)) => cons(a, unfold(s)(f))
    case None => Empty
  }

}
