package fpinscala.exercises.datastructures

enum Tree[+A]:
  case Leaf(value: A)
  case Branch(left: Tree[A], right: Tree[A])

  def size: Int = this match
    case Leaf(_) => 1
    case Branch(l, r) => 1 + l.size + r.size

  def depth: Int = this match {
    case Leaf(_) => 0
    case Branch(l, r) => 1 + l.depth.max(r.depth)
  }

  def map[B](f: A => B): Tree[B] = this match {
    case Leaf(a) => Leaf(f(a))
    case Branch(l, r) => Branch(l.map(f), r.map(f))
  }

  def fold[B](f: A => B, g: (B,B) => B): B = this match {
    case Leaf(a) => f(a)
    case Branch(l, r) => g(l.fold(f, g), r.fold(f, g))
  }
  
  def sizeViaFold: Int =
   fold[Int](_ => 1, (bl, br) => 1 + bl + br)
  
  def depthViaFold: Int =
   fold[Int](_ => 0, (bl, br) => 1 + bl.max(br))
  
  def mapViaFold[B](f: A => B): Tree[B] =
   fold[Tree[B]](a => Leaf(f(a)), (bl, br) => Branch(bl, br))

object Tree:

  def size[A](t: Tree[A]): Int = t match
    case Leaf(_) => 1
    case Branch(l,r) => 1 + size(l) + size(r)

  extension (t: Tree[Int]) def firstPositive: Int = 
    t.fold[Int](identity, (bl, br) => if bl > 0 then bl else br)

  extension (t: Tree[Int]) def maximum: Int = t match {
    case Leaf(x) => x
    case Branch(l, r) => l.maximum.max(r.maximum)
  }

  extension (t: Tree[Int]) def maximumViaFold: Int =
   t.fold[Int](identity, _ max _)
