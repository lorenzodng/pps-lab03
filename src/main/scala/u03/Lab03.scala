package u03

import u03.Optionals.Optional.*

// Task 1, 2, svolto da solo
object Sequences:

  enum Sequence[E]:
    case Cons(head: E, tail: Sequence[E])
    case Nil()

  enum Person:
    case Student(name: String, year: Int)
    case Teacher(name: String, course: String)

  object Sequence:

    def sum(l: Sequence[Int]): Int = l match
      case Cons(h, t) => h + sum(t)
      case _ => 0

    def map[A, B](l: Sequence[A])(mapper: A => B): Sequence[B] = l match
      case Cons(h, t) => Cons(mapper(h), map(t)(mapper))
      case Nil() => Nil()

    def filter[A](l1: Sequence[A])(pred: A => Boolean): Sequence[A] = l1 match
      case Cons(h, t) if pred(h) => Cons(h, filter(t)(pred))
      case Cons(_, t) => filter(t)(pred)
      case Nil() => Nil()

    /*
     * Skip the first n elements of the sequence
     * E.g., [10, 20, 30], 2 => [30]
     * E.g., [10, 20, 30], 3 => []
     * E.g., [10, 20, 30], 0 => [10, 20, 30]
     * E.g., [], 2 => []
     */
    def skip[A](s: Sequence[A])(n: Int): Sequence[A] = s match
      case Cons(h, t) if n > 0 => skip(t)(n - 1)
      case Cons(h, t) => Cons(h, skip(t)(n))
      case _ => Nil()

    /*
     * Zip two sequences
     * E.g., [10, 20, 30], [40, 50] => [(10, 40), (20, 50)]
     * E.g., [10], [] => []
     * E.g., [], [] => []
     */
    def zip[A, B](first: Sequence[A], second: Sequence[B]): Sequence[(A, B)] = (first, second) match
      case (Cons(hf, tf), Cons(hs, ts)) => Cons((hf, hs), zip(tf, ts))
      case _ => Nil()

    /*
     * Concatenate two sequences
     * E.g., [10, 20, 30], [40, 50, 60] => [10, 20, 30, 40, 50, 60]
     * E.g., [10], [] => [10]
     * E.g., [], [] => []
     */
    def concat[A](s1: Sequence[A], s2: Sequence[A]): Sequence[A] = s1 match
      case Cons(h1, t1) => Cons(h1, concat(t1, s2))
      case Nil() => s2

    /*
     * Reverse the sequence
     * E.g., [10, 20, 30] => [30, 20, 10]
     * E.g., [10] => [10]
     * E.g., [] => []
     */
    def reverse[A](s: Sequence[A]): Sequence[A] = s match
      case Cons(h, t) => concat(reverse(t), Cons(h, Nil())) //10 20 30 - 10, Nil() - 20, 10, Nil() - 30, 20, 10, Nil()
      case _ => Nil()

    /*
     * Map the elements of the sequence to a new sequence and flatten the result
     * E.g., [10, 20, 30], calling with mapper(v => [v, v + 1]) returns [10, 11, 20, 21, 30, 31]
     * E.g., [10, 20, 30], calling with mapper(v => [v]) returns [10, 20, 30]
     * E.g., [10, 20, 30], calling with mapper(v => Nil()) returns []
     */
    def flatMap[A, B](s: Sequence[A])(mapper: A => Sequence[B]): Sequence[B] = s match
      case Cons(h, t) => concat(mapper(h), flatMap(t)(mapper))
      case _ => Nil()

    // 1.
    def courses(people: Sequence[Person]): Sequence[String] = map(filter(people)(person => person match
      case Person.Teacher(_, _) => true
      case _ => false
    ))(person => person match
      case Person.Teacher(_, course) => course
      case _ => ""
    )

    // 2.
    def foldLeft[A](s: Sequence[A])(default: A)(op: (A, A) => A): A = s match
      case Cons(h, t) => foldLeft(t)(op(default, h))(op)
      case _ => default

    // 3.
    def coursesNumber(people: Sequence[Person]): Int = foldLeft(map(filter(people)(person => person match
      case Person.Teacher(_, _) => true
      case _ => false
    ))(person => person match
      case Person.Teacher(name, course) => 1
      case _ => 0
    ))(0)(_ + _)

  end Sequence
end Sequences

// Task 3, svolto da solo
object Streams:

  import Sequences.*

  enum Stream[A]:
    private case Empty()
    private case Cons(head: () => A, tail: () => Stream[A])

  object Stream:

    def empty[A](): Stream[A] = Empty()

    def cons[A](hd: => A, tl: => Stream[A]): Stream[A] =
      lazy val head = hd
      lazy val tail = tl
      Cons(() => head, () => tail)

    def toList[A](stream: Stream[A]): Sequence[A] = stream match
      case Cons(h, t) => Sequence.Cons(h(), toList(t()))
      case _ => Sequence.Nil()

    def map[A, B](stream: Stream[A])(f: A => B): Stream[B] = stream match
      case Cons(head, tail) => cons(f(head()), map(tail())(f))
      case _ => Empty()

    def filter[A](stream: Stream[A])(pred: A => Boolean): Stream[A] = stream match
      case Cons(head, tail) if (pred(head())) => cons(head(), filter(tail())(pred))
      case Cons(head, tail) => filter(tail())(pred)
      case _ => Empty()

    def take[A](stream: Stream[A])(n: Int): Stream[A] = (stream, n) match
      case (Cons(head, tail), n) if n > 0 => cons(head(), take(tail())(n - 1))
      case _ => Empty()

    def iterate[A](init: => A)(next: A => A): Stream[A] =
      cons(init, iterate(next(init))(next))

    def interleave[A](stream1: Stream[A], stream2: Stream[A]): Stream[A] = (stream1, stream2) match
      case (Cons(h1, t1), Cons(h2, t2)) => cons(h1(), cons(h2(), interleave(t1(), t2())))
      case (Cons(h1, t1), _) => cons(h1(), interleave(t1(), empty()))
      case (_, Cons(h2, t2)) => cons(h2(), interleave(empty(), t2()))
      case _ => empty()

    // 6.
    def takeWhile[A](stream: Stream[A])(pred: A => Boolean): Stream[A] = stream match
      case Cons(head, tail) if pred(head()) => cons(head(), takeWhile(tail())(pred))
      case _ => empty()

    // 7.
    def fill[A](n: Int)(k: A): Stream[A] = n match
      case n if n != 0 => cons(k, fill(n - 1)(k))
      case _ => empty()

    // 8.
    val fibonacci: Stream[Int] =
      def computeFib(acc1: Int, acc2: Int): Stream[Int] =
        cons(acc1, computeFib(acc1 + acc2, acc1))

      computeFib(0, 1)

  end Stream
end Streams
