package u03

import org.junit.*
import org.junit.Assert.*
import u03.Sequences.*
import Sequence.{Cons, *}

// Task 1,2 tests
class SequenceTest:

  val sequence: Sequence[Int] = Cons(10, Cons(20, Cons(30, Nil())))

  @Test def testSum() =
    assertEquals(0, sum(Nil()))
    assertEquals(60, sum(sequence))

  @Test def testMap() =
    assertEquals(Cons(11, Cons(21, Cons(31, Nil()))), map(sequence)(_ + 1))
    assertEquals(Cons("10", Cons("20", Cons("30", Nil()))), map(sequence)(_ + ""))

  @Test def testFilter() =
    assertEquals(Cons(20, Cons(30, Nil())), filter(sequence)(_ >= 20))
    assertEquals(Cons(10, Cons(30, Nil())), filter(sequence)(_ != 20))

  @Test def testSkip() =
    assertEquals(Cons(30, Nil()), skip(sequence)(2))
    assertEquals(Nil(), skip(sequence)(3))
    assertEquals(Cons(10, Cons(20, Cons(30, Nil()))), skip(sequence)(0))
    assertEquals(Nil(), skip(Nil())(2))

  @Test def testZip() =
    val l2: Sequence[String] = Cons("10", Cons("20", Cons("30", Nil())))
    assertEquals(Cons((10, "10"), Cons((20, "20"), Cons((30, "30"), Nil()))), zip(sequence, l2))
    assertEquals(Nil(), zip(sequence, Nil()))
    assertEquals(Nil(), zip(Nil(), l2))
    assertEquals(Nil(), zip(Nil(), Nil()))

  @Test def testConcat() =
    val l2: Sequence[Int] = Cons(40, Cons(50, Nil()))
    assertEquals(Cons(10, Cons(20, Cons(30, Cons(40, Cons(50, Nil()))))), concat(sequence, l2))
    assertEquals(Cons(40, Cons(50, Nil())), concat(Nil(), l2))

  @Test def testReverse() =
    assertEquals(Cons(30, Cons(20, Cons(10, Nil()))), reverse(sequence))
    assertEquals(Nil(), reverse(Nil()))

  @Test def testFlatMap() =
    assertEquals(Cons(11, Cons(21, Cons(31, Nil()))), flatMap(sequence)(v => Cons(v + 1, Nil())))
    assertEquals(Nil(), flatMap(Nil())(v => Cons(v, Nil())))

  @Test def testCourses() =
    val people = Cons(Person.Teacher("Marco", "Math"), Cons(Person.Student("Luisa", 2), Cons(Person.Teacher("Bob", "Physics"), Nil())))
    val expectedSequence = Cons("Math", Cons("Physics", Nil()))
    assertEquals(expectedSequence, courses(people))

  @Test def testFoldLeft() =
    val sequence = Cons(10, Cons(20, Cons(30, Nil())))
    val expectedValue= -60
    assertEquals(expectedValue, foldLeft(sequence)(0)(_ - _))

  @Test def testCoursesNumber() =
    val people = Cons(Person.Teacher("Marco", "Math"), Cons(Person.Student("Luisa", 2), Cons(Person.Teacher("Bob", "Physics"), Nil())))
    val expectedValue= 2
    assertEquals(expectedValue, coursesNumber(people))

end SequenceTest

// Task 3 tests
class StreamTest:
  import u03.Streams.*
  import Stream.*

  @Test def testIterate(): Unit =
    val str1 = Stream.iterate(0)(_ + 1) // {0,1,2,3,..}
    assertEquals(Cons(0, Cons(1, Cons(2, Cons(3, Nil())))), toList(Stream.take(str1)(4)))

  @Test def testMap(): Unit =
    val str1 = Stream.iterate(0)(_ + 1) // {0,1,2,3,..}
    val str2 = Stream.map(str1)(_ + 1) // {1,2,3,4,..}
    assertEquals(Cons(1, Cons(2, Cons(3, Cons(4, Nil())))), toList(Stream.take(str2)(4)))

  @Test def testFilter(): Unit =
    val str1 = Stream.iterate(0)(_ + 1) // {0,1,2,3,..}
    val str2 = Stream.filter(str1)(x => x % 2 == 1) // {1,3,5,7,..}
    assertEquals(Cons(1, Cons(3, Cons(5, Cons(7, Nil())))), toList(Stream.take(str2)(4)))

  @Test def takeWhile(): Unit =
    val str1 = Stream.iterate(0)(_ + 1)
    val str2 = Stream.takeWhile(str1)(_ < 5)
    assertEquals(Cons(0, Cons(1, Cons(2, Cons(3, Cons(4, Nil()))))), Stream.toList(str2))

  @Test def testFill(): Unit =
    val elementsNumber= 3
    val elements= "a"
    val expectedSequence = Cons("a", Cons("a", Cons("a", Nil())))
    assertEquals(expectedSequence, toList(fill(elementsNumber)(elements)))

  @Test def testFibonacci(): Unit =
    val elementsNumber = 6
    val expectedSequence = Cons(0, Cons(1, Cons(1, Cons(2, Cons(3, Cons(5, Nil()))))))
    assertEquals(expectedSequence, toList(take(fibonacci)(elementsNumber)))

end StreamTest

