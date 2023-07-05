package fpinscala.exercises.state


trait RNG:
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.

object RNG:
  // NB - this was called SimpleRNG in the book text

  case class Simple(seed: Long) extends RNG:
    def nextInt: (Int, RNG) =
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng =>
      val (a, rng2) = s(rng)
      (f(a), rng2)

  def nonNegativeInt(rng: RNG): (Int, RNG) = map(int)(_ & Int.MaxValue)(rng)

  def double(rng: RNG): (Double, RNG) =
    map(nonNegativeInt)(rawResult => (rawResult % Int.MaxValue - 1).toDouble / Int.MaxValue)(rng)

  def intDouble(rng: RNG): ((Int,Double), RNG) = both(int, double)(rng)

  def doubleInt(rng: RNG): ((Double,Int), RNG) = both(double, int)(rng)

  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val (d1, rng2) = double(rng)
    val (d2, rng3) = double(rng2)
    val (d3, rng4) = double(rng3)
    ((d1, d2, d3), rng4)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = sequence(List.fill(count)(int))(rng)

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = rng => {
    val (resultA, rng2) = ra(rng)
    val (resultB, rng3) = rb(rng2)
    (f(resultA, resultB), rng3)
  }

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] = map2(ra, rb)((_, _))

  def sequence[A](rs: List[Rand[A]]): Rand[List[A]] = {
    @annotation.tailrec
    def loop(rs: List[Rand[A]], acc: (List[A], RNG)): (List[A], RNG) = rs match {
      case Nil => (acc._1.reverse, acc._2)
      case h :: t =>
        val (nextResult, nextRNG) = h(acc._2)
        loop(t, (nextResult :: acc._1, nextRNG))
    }

    rng => loop(rs, (Nil, rng))
  }

  def flatMap[A, B](r: Rand[A])(f: A => Rand[B]): Rand[B] = rng => {
    val (a, rng2) = r(rng)
    f(a)(rng2)
  }

  def nonNegativeLessThan(n: Int): Rand[Int] =
    flatMap(nonNegativeInt) { i =>
      val mod = i % n
      if i + (n - 1) - mod >= 0 then rng => (mod, rng)
      else nonNegativeLessThan(n)
    }

  def mapViaFlatMap[A, B](r: Rand[A])(f: A => B): Rand[B] =
    flatMap(r)(a => rng => (f(a), rng))

  def map2ViaFlatMap[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra)(a => map(rb)(f(a, _)))

opaque type State[S, +A] = S => (A, S)

object State:
  extension [S, A](underlying: State[S, A])
    def run(s: S): (A, S) = underlying(s)

    def map[B](f: A => B): State[S, B] = flatMap(a => s => (f(a), s))

    def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
      flatMap(a => sb.map(f(a, _)))

    def flatMap[B](f: A => State[S, B]): State[S, B] = s => {
      val (a, s2) = run(s)
      f(a)(s2)
    }

  def apply[S, A](f: S => (A, S)): State[S, A] = f

  def unit[S, A](a: A): State[S, A] = s => (a, s)

  def sequence[S, A](as: List[State[S, A]]): State[S, List[A]] = traverse(as)(identity)

  def traverse[S, A, B](as: List[A])(f: A => State[S, B]): State[S, List[B]] =
    as.foldLeft[State[S, List[B]]](unit(Nil))( { case (acc, a) => acc.map2(f(a))((bs, b) => b :: bs) }).map(_.reverse)

enum Input:
  case Coin, Turn

case class Machine(locked: Boolean, candies: Int, coins: Int)

object Candy:
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] =
    State.traverse(inputs)(dispatch).flatMap { _ => State(s => ((s.coins, s.candies), s)) }

  private def dispatch(input: Input): State[Machine, Unit] = {
    import Input.*

    State((s: Machine) => {
      val newState = input match {
        case _ if s.candies == 0 => s
        case Coin if s.locked => s.copy(locked = false, coins = s.coins + 1)
        case Turn if !s.locked =>
          s.copy(locked = true, candies = s.candies - 1)
        case _ => s
      }
      ((), newState)
    })
  }
