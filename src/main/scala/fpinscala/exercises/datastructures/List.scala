package fpinscala.exercises.datastructures

/** `List` data type, parameterized on a type, `A`. */
enum List[+A]:
  /** A `List` data constructor representing the empty list. */
  case Nil
  /** Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
    which may be `Nil` or another `Cons`.
   */
  case Cons(head: A, tail: List[A])

object List: // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.

  def product(doubles: List[Double]): Double = doubles match
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if as.isEmpty then Nil
    else Cons(as.head, apply(as.tail*))

  @annotation.nowarn // Scala gives a hint here via a warning, so let's disable that
  val result = List(1,2,3,4,5) match
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))

  def foldRight[A,B](as: List[A], acc: B, f: (A, B) => B): B = // Utility functions
    foldRightOriginal(as, acc, f)

  def foldRightOriginal[A,B](as: List[A], acc: B, f: (A, B) => B): B =
    as match
      case Nil => acc
      case Cons(x, xs) => f(x, foldRight(xs, acc, f))

  def foldRightViaFoldLeft[A, B](as: List[A], acc: B, f: (A, B) => B): B =
    foldLeft(reverse(as), acc, (b, a) => f(a, b))

  def sumViaFoldRight(ns: List[Int]): Int =
    foldRight(ns, 0, (x,y) => x + y)

  def productViaFoldRight(ns: List[Double]): Double =
    foldRight(ns, 1.0, _ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar

  def tail[A](l: List[A]): List[A] = l match {
    case Nil => sys.error("cannot invoke tail on Nil")
    case Cons(_, t) => t
  }

  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Nil => sys.error("cannot invoke setHead on Nil")
    case Cons(_, t) => Cons(h, t)
  }

  def drop[A](l: List[A], n: Int): List[A] = {
    @annotation.tailrec
    def loop(l: List[A], n: Int): List[A] = l match {
      case Cons(_, t) if n >= 1 => loop(t, n - 1)
      case _ => l
    }

    loop(l, n)
  }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = {
    @annotation.tailrec
    def loop(l: List[A]): List[A] = l match {
      case Cons(h, t) if f(h) => loop(t)
      case _ => l
    }

    loop(l)
  }

  def init[A](l: List[A]): List[A] = l match {
    case Nil => sys.error("cannot invoke init on Nil")
    case _ => reverse(drop(reverse(l), 1))
  }

  def length[A](l: List[A]): Int = foldRight(l, 1, (_, counter) => counter + 1)

  def foldLeft[A,B](l: List[A], acc: B, f: (B, A) => B): B = {
    @annotation.tailrec
    def loop(l: List[A], acc: B): B = l match {
      case Nil => acc
      case Cons(h, t) => loop(t, f(acc, h)) 
    }

    loop(l, acc)
  }

  def sumViaFoldLeft(ns: List[Int]): Int = foldLeft(ns, 0, _ + _)

  def productViaFoldLeft(ns: List[Double]): Double = foldLeft(ns, 1, _ * _)

  def lengthViaFoldLeft[A](l: List[A]): Int = foldLeft(l, 0, (b, _) => b + 1)

  def reverse[A](l: List[A]): List[A] = foldLeft[A, List[A]](l, Nil, (as, a) => Cons(a, as))

  def appendViaFoldRight[A](l: List[A], r: List[A]): List[A] = foldRight(l, r, (a, as) => Cons(a, as))

  def concat[A](l: List[List[A]]): List[A] =
    foldRight[List[A], List[A]](l, Nil, (as, acc) => appendViaFoldRight(as, acc))

  def incrementEach(l: List[Int]): List[Int] =
    foldRight[Int, List[Int]](l, Nil, (i, is) => Cons(i + 1, is))

  def doubleToString(l: List[Double]): List[String] =
    foldRight[Double, List[String]](l, Nil, (d, acc) => Cons(d.toString, acc))

  def map[A,B](l: List[A], f: A => B): List[B] =
    foldRight[A, List[B]](l, Nil, (a, acc) => Cons(f(a), acc))

  def filter[A](as: List[A], f: A => Boolean): List[A] =
    foldRight[A, List[A]](as, Nil, (a, acc) => if f(a) then Cons(a, acc) else acc)

  def flatMap[A,B](as: List[A], f: A => List[B]): List[B] = concat(map(as, f))

  def filterViaFlatMap[A](as: List[A], f: A => Boolean): List[A] =
    flatMap(as, a => if f(a) then Cons(a, Nil) else Nil)

  def addPairwise(a: List[Int], b: List[Int]): List[Int] = {
    @annotation.tailrec
    def loop(as: List[Int], bs: List[Int], acc: List[Int]): List[Int] = (as, bs) match {
      case (Cons(a, at), Cons(b, bt)) => loop(at, bt, Cons(a + b, acc))
      case _ => acc
    }

    reverse(loop(a, b, Nil))
  }

  // def zipWith - TODO determine signature

  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = ???
