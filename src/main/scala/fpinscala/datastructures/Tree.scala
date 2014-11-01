package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {

  def size[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(l,r) => size(l) + size(r) + 1
  }

  def maximum(t: Tree[Int]): Int = t match {
    case Leaf(v) => v
    case Branch(l,r) => maximum(l) max maximum(r)
  }

  def depth[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(l,r) => (depth(l) max depth(r)) + 1
  }

  def map[A,B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Leaf(a) => Leaf(f(a))
    case Branch(l,r) => Branch(map(l)(f), map(r)(f))
  }

  def fold[A,B](t: Tree[A])(f: A => B)(g: (B,B) => B): B = t match {
    case Leaf(a) => f(a)
    case Branch(l,r) => g(fold(l)(f)(g), fold(r)(f)(g))
  }

  def otherFold[A,B](f: A => B)(g: (B,B) => B)(t: Tree[A]): B = {
    val of = otherFold(f)(g) _
    t match {
      case Leaf(a) => f(a)
      case Branch(l,r) => g(of(l), of(r))
    }
  }

  def sizeF[A](t: Tree[A]): Int = fold(t)(_ => 1)(_ + _ + 1)
  def maximumF(t: Tree[Int]): Int = fold(t)(identity)(_ max _)
  def depthF[A](t: Tree[A]): Int = fold(t)(_ => 0)(_ max _ + 1)
  def mapF[A,B](t: Tree[A])(f: A => B): Tree[B] =
    fold(t)(a => Leaf(f(a)): Tree[B])(Branch(_,_))


//  def fold[A,B](t: Tree[A], z: B)(f: (A,B) => B): B = t match {
//    case Leaf(a) => f(a, z)
//    case Branch(l,r) => fold(l, fold(r, z)(f))(f)
//  }
//
//  def foldNoInitial[A,B](t: Tree[A])(f: (A,B) => B)(g: A => B): B = t match {
//    case Leaf(a) => g(a)
//    case Branch(l,r) => fold(l, foldNoInitial(r)(f)(g))(f)
//  }
//
//  def sizeF[A](t: Tree[A]): Int = fold(t, 1)((a,b) => 1 + b)
//  def maximumF(t: Tree[Int]): Int = foldNoInitial(t)((a: Int, b: Int) => a max b)(x => x)
//
//
//  def toDepths[A](t: Tree[A], depth: Int): Tree[Int] =
//    t match {
//      case Leaf(_) => Leaf(depth)
//      case Branch(l,r) => Branch(toDepths(l, depth + 1), toDepths(r, depth + 1))
//  }
//
//  def depthF2[A](t: Tree[A]): Int = fold(t)((a,b) => 1 +)
//
//  def depthF[A](t: Tree[A]): Int = maximumF(toDepths(t, 1))
//
//  def mapF[A,B](t: Tree[A])(f: A => B): Tree[B] =
//    foldNoInitial(t)((a: A, b: Tree[B]) => Branch(Leaf(f(a)),b): Tree[B])(x => Leaf(f(x)))
}
