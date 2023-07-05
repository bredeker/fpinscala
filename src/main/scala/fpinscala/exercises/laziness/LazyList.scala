package fpinscala.exercises.laziness

enum LazyList[+A]:
  case Empty
  case Cons(h: () => A, t: () => LazyList[A])

  def toList: List[A] = {
    @annotation.tailrec
    def loop(as: LazyList[A], acc: List[A]): List[A] = as match {
      case Empty => acc.reverse
      case Cons(h, t) => loop(t(), h() :: acc)
    }

    loop(this, Nil)
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z

  def exists(p: A => Boolean): Boolean = 
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the lazy list. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)

  def take(n: Int): LazyList[A] = this match {
    case Cons(h, t) if n > 1 => LazyList.cons(h(), t().take(n - 1))
    case Cons(h, _) if n == 1 => LazyList.cons(h(), Empty)
    case _ => Empty
  }

  def drop(n: Int): LazyList[A] = this match {
    case Cons(_, t) if n > 0 => t().drop(n - 1)
    case other => other // Either Empty or n is <= 0
  }

  def takeWhile(p: A => Boolean): LazyList[A] = this match {
    case Empty => Empty
    case Cons(h, t) =>
      val head = h()
      if p(head) then LazyList.cons(head, t().takeWhile(p)) else Empty
  }

  def forAll(p: A => Boolean): Boolean = foldRight(true)((a, b) => p(a) && b)

  def headOption: Option[A] = foldRight(Option.empty)((a, _) => Option(a))

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.

  def map[B](f: A => B): LazyList[B] =
    foldRight(LazyList.empty[B])((a, b) => LazyList.cons(f(a), b))

  def filter(f: A => Boolean): LazyList[A] =
    foldRight(LazyList.empty[A])((a, b) => if f(a) then LazyList.cons(a, b) else b)
  
  def append[B >: A](other: => LazyList[B]): LazyList[B] = foldRight(other)(LazyList.cons)

  def flatMap[B >: A](f: A => LazyList[B]): LazyList[B] =
    map(f).foldRight(LazyList.empty[B])((bs, acc) => bs.append(acc))
  
  def mapViaUnfold[B](f: A => B): LazyList[B] = LazyList.unfold(this) { as =>
    as match {
      case Empty => None
      case Cons(h, t) => Option((f(h()), t())) 
    }
  }

  def takeViaUnfold(n: Int): LazyList[A] = LazyList.unfold((this, n)) { case (as, n) =>
    as match {
      case Cons(h, t) if n >= 1 => Option((h(), (t(), n - 1)))
      case _ => None
    }
  }

  def takeWhileViaUnfold(p: A => Boolean): LazyList[A] = LazyList.unfold(this) { as =>
    as match {
      case Cons(h, t) =>
        val materialized = h()
        if p(materialized) then Option((materialized, t())) else None
      case _ => None
    }
  }

  def zipWith[B, C](that: LazyList[B], f: (A, B) => C): LazyList[C] =
    LazyList.unfold((this, that)) {
      case (Cons(ah, at), Cons(bh, bt)) => Option(f(ah(), bh()), (at(), bt()))
      case _ => None
    }
  
  def zipAll[B](that: LazyList[B]): LazyList[(Option[A], Option[B])] =
    LazyList.unfold((this, that)) {
      case (Cons(ah, at), Cons(bh, bt)) => Option((Option(ah()), Option(bh())), (at(), bt()))
      case (Cons(ah, at), _) => Option((Option(ah()), None), (at(), Empty))
      case (_, Cons(bh, bt)) => Option((None, Option(bh())), (Empty, bt()))
      case _ => None
    }


  def startsWith[B](s: LazyList[B]): Boolean = (this, s) match {
    case (Empty, Cons(_, _)) => false
    case (_, Empty) => true
    case (Cons(ht, tt), Cons(ho, to)) => (ht() == ho()) && tt().startsWith(to())
  }

  def tails: LazyList[LazyList[A]] = LazyList.unfold(Option(this)) {
    case None => None
    case Some(Empty) => Option((Empty, None))
    case Some(as @ Cons(h, t)) => Option((as, Option(t())))
  }

  def hasSubsequence[A](l: LazyList[A]): Boolean = tails.exists(_.startsWith(l))

  def scanRight[B](z: => B)(f: (A, => B) => B): LazyList[B] = {
    def initialState: (B, LazyList[B]) = {
      val init = z
      (init, LazyList(init))
    }
    val (_, result) = foldRight(initialState) { (a, acc) =>
      lazy val (b, bs) = acc
      val next = f(a, b)
      (next, LazyList.cons(next, bs))
    }
    result
  }

  override def toString: String = this match {
    case Empty => "LazyList()"
    case Cons(_, _) => "LazyList(<not evaluated>)"
  }


object LazyList:
  def cons[A](hd: => A, tl: => LazyList[A]): LazyList[A] = 
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)

  def empty[A]: LazyList[A] = Empty

  def apply[A](as: A*): LazyList[A] =
    if as.isEmpty then empty 
    else cons(as.head, apply(as.tail*))

  val ones: LazyList[Int] = LazyList.cons(1, ones)

  def continually[A](a: A): LazyList[A] = cons(a, continually(a))

  def from(n: Int): LazyList[Int] = cons(n, from(n + 1))

  lazy val fibs: LazyList[Int] = {
    def loop(n1: => Int, n2: => Int): LazyList[Int] = {
      lazy val i = n1
      lazy val j = n2
      Cons(() => i, () => loop(j, i + j))
    }

    loop(0, 1)
  }

  def unfold[A, S](state: S)(f: S => Option[(A, S)]): LazyList[A] =
    f(state).fold(empty) { case (a, s) => cons(a, unfold(s)(f)) }

  lazy val fibsViaUnfold: LazyList[Int] = unfold((0, 1)) { case (i, j) =>
    Option((i, (j, i + j)))
  }

  def fromViaUnfold(n: Int): LazyList[Int] = unfold(n)(i => Option((i, i + 1)))

  def continuallyViaUnfold[A](a: A): LazyList[A] = {
    val constantState = Option((a, a))
    unfold(a)(_ => constantState)
  }

  lazy val onesViaUnfold: LazyList[Int] = {
    val constantState = Option((1, 1))
    unfold(1)(_ => constantState)
  }
