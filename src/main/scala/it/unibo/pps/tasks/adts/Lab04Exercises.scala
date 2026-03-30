package it.unibo.pps.tasks.adts

/* Lab04 exercises objects:
 * - Ex1ComplexNumbers
 * - SchoolModel
 * - Ex3Stacks
 * - Ex4Summables
 * - Ex5Traversable
 * - Ex6TryModel
 */

/*  Exercise 1:
 *  Complete the implementation of ComplexADT trait below, so that it passes
 *  the test in ComplexTest.
 */
object Ex1ComplexNumbers:

  trait ComplexADT:
    type Complex

    def complex(re: Double, im: Double): Complex
    extension (complex: Complex)
      def re(): Double
      def im(): Double
      def sum(other: Complex): Complex
      def subtract(other: Complex): Complex
      def asString(): String

  object BasicComplexADT extends ComplexADT:

    // Change assignment below: should probably define a case class and use it?
    type Complex = (Double, Double)
    def complex(re: Double, im: Double): Complex = (re, im)

    extension (complex: Complex)

      def re(): Double = complex match
        case (re, im) => re

      def im(): Double = complex match
        case (re, im) => im

      def sum(other: Complex): Complex =
        (complex.re() + other.re(), complex.im() + other.im())

      def subtract(other: Complex): Complex =
        (complex.re() - other.re(), complex.im() - other.im())

      def asString(): String = complex match
        case (0, 0)   => "0.0"
        case (0, im)  => im + "i"
        case (re, 0)  => re + ""
        case (re, im) => re + (if im < 0 then " - " + im * (-1) else " + " + im) + "i"

/*  Exercise 2:
 *  Implement the below trait, and write a meaningful test.
 *  Suggestions:
 *  - reuse Sequences and Optionals as imported above
 *  - For other suggestions look directly to the methods and their description
 */
object SchoolModel:
  import it.unibo.pps.u03.Sequences.Sequence
  import Sequence.*
  import scala.annotation.tailrec

  trait SchoolModule:
    type School
    type Teacher
    type Course

    /**
     * This a factory method for create a teacher from a name
     * e.g.,
     * teacher("John") // => Teacher("John")
     * Note!! The internal representation of a teacher may vary, decide what is the best for you
     * @param name the name of the teacher
     * @return the teacher created
     */
    def teacher(name: String): Teacher
    /**
     * This a factory method for create a course from a name
     * e.g.,
     * course("Math") // => Course("Math")
     * Note!! The internal representation of a course may vary, decide what is the best for you
     * @param name the name of the course
     * @return the course created
     *  */
    def course(name: String): Course
    /**
     * This method should return an empty school, namely a school without any teacher and course
     * e.g.,
     * emptySchool // => School(courses = Nil(), teachers = Nil(), teacherToCourses = Nil())
     * NOTE!! The above is just an example, the internal representation may vary, decide what is the best for you
     * You can store just the teacherToCourses, or having a case class for the school, or whatever you think is the best
     * @return the empty school
     */
    def emptySchool: School

    extension (school: School)
      /**
       * This method should return the list of courses
       * e.g.,
       * emptySchool.courses // => Nil()
       * emptySchool.setTeacherToCourse(teacher("John"), course("Math")).courses // => Cons("Math", Nil())
       * emptySchool
       *  .setTeacherToCourse(teacher("John"), course("Math"))
       *  .setTeacherToCourse(teacher("John"), course("Italian")).courses // => Cons("Math", Cons("Italian", Nil()))
       * Note!! If there are duplicates, just return them once
       * @return the list of courses
       */
      def courses: Sequence[String]
      /**
       * This method should return the list of teachers
       * e.g.,
       * emptySchool.teachers // => Nil()
       * emptySchool.setTeacherToCourse(teacher("John"), course("Math")).teachers // => Cons("John", Nil())
       * val john = teacher("John")
       * emptySchool
       *  .setTeacherToCourse(john, course("Math"))
       *  .setTeacherToCourse(john, course("Italian")).teachers // => Cons("John", Nil())
       * Note!! If there are duplicates, just return them once
       * @return the list of teachers
       */
      def teachers: Sequence[String]
      /**
       * This method should return a new school with the teacher assigned to the course
       * e.g.,
       * emptySchool
       *   .setTeacherToCourse(teacher("John"), course("Math")) // => School(courses = Cons("Math", Nil()), teachers = Cons("John", Nil()), teacherToCourses = Cons(("John", "Math"), Nil()))
       *  */
      def setTeacherToCourse(teacher: Teacher, course: Course): School
      /**
       * This method should return the list of courses assigned to a teacher
       * e.g.,
       * emptySchool.coursesOfATeacher(teacher("John")) // => Nil()
       * emptySchool
       *   .setTeacherToCourse(teacher("John"), course("Math"))
       *   .coursesOfATeacher(teacher("John")) // => Cons("Math", Nil())
       * emptySchool
       *   .setTeacherToCourse(teacher("John"), course("Math"))
       *   .setTeacherToCourse(teacher("John"), course("Italian"))
       *   .coursesOfATeacher(teacher("John")) // => Cons("Math", Cons("Italian", Nil()))
       * @return the list of courses assigned to a teacher
       */
      def coursesOfATeacher(teacher: Teacher): Sequence[Course]
      /**
       * This method should return true if the teacher is present in the school
       * e.g.,
       * emptySchool.hasTeacher("John") // => false
       * emptySchool
       *  .setTeacherToCourse(teacher("John"), course("Math"))
       *  .hasTeacher("John") // => true
       *
       */
      def hasTeacher(name: String): Boolean
      /**
       * This method should return true if the course is present in the school
       * e.g.,
       * emptySchool.hasCourse("Math") // => false
       * emptySchool
       *  .setTeacherToCourse(teacher("John"), course("Math"))
       *  .hasCourse("Math") // => true
       *
       */
      def hasCourse(name: String): Boolean

  object BasicSchoolModule extends SchoolModule:
    override type School = Sequence[(Teacher, Course)]
    override type Teacher = String
    override type Course = String
    final val TEACHER: Int = 0
    final val COURSE: Int = 1

    def teacher(name: String): Teacher = name
    def course(name: String): Course = name
    def emptySchool: School = Nil()

    extension (school: School)

      def teachers: Sequence[String] =
        innerSeq(school, Nil(), TEACHER)

      def courses: Sequence[String] =
        innerSeq(school, Nil(), COURSE)

      def setTeacherToCourse(teacher: Teacher, course: Course): School =
        Cons((teacher, course), school)

      def coursesOfATeacher(teacher: Teacher): Sequence[Course] =
        @tailrec
        def inner(school: School, name: String, seq: Sequence[String]): Sequence[String] = school match
          case Cons((teacher, course), tail) if teacher == name => inner(tail, name, Cons(course, seq))
          case Cons((teacher, course), tail)                    => inner(tail, name, seq)
          case _                                                => distinct(seq)
        inner(school, teacher, Nil())

      def hasTeacher(name: String): Boolean =
        innerHas(school, name, TEACHER)

      def hasCourse(name: String): Boolean =
        innerHas(school, name, COURSE)

    def innerSeq(school: School, seq: Sequence[String], tupIndex: Int): Sequence[String] = school match
      case Cons(tup, tail) => innerSeq(tail, Cons(tup(tupIndex).asInstanceOf[String], seq), tupIndex)
      case _               => distinct(seq)

    def innerHas(school: School, name: String, tupIndex: Int): Boolean = school match
      case Cons(tup, tail) => tup(tupIndex) == name || innerHas(tail, name, tupIndex)
      case _               => false

/*  Exercise 3:
 *  Implement a Stack ADT
 *  Suggestion:
 *  - push adds an element and returns the new stack
 *  - pop returns:
 *  -- empty optional is stack is empty
 *  -- a pair of top of the stack and the new stack after removal if not empty
 */
object Ex3Stacks:
  import it.unibo.pps.u03.Sequences.Sequence, Sequence.*
  import it.unibo.pps.u03.Optionals.Optional, Optional.*

  trait StackADT:
    type Stack[A]
    def empty[A]: Stack[A] // factory
    extension [A](stack: Stack[A])
      def push(a: A): Stack[A]
      def pop(): Optional[(A, Stack[A])]
      def asSequence(): Sequence[A]

  object StackImpl extends StackADT:
    type Stack[A] = Sequence[A]
    def empty[A]: Stack[A] = Nil()

    extension [A](stack: Stack[A])

      def push(a: A): Stack[A] =
        Cons(a, stack)

      def pop(): Optional[(A, Stack[A])] = stack match
        case Cons(h, t) => Just((h, t))
        case _ => Empty()

      def asSequence(): Sequence[A] = stack match
        case Cons(h, t) => Cons(h, t)
        case _ => Nil()

/*  Exercise 4:
 *  - Complete the implementation of ad-hoc polymorphic sumAll, using summable.sum and summable.zero
 *  - Write givens also for Summable[Double], Summable[String]
 *  - Uncomment in the main and check if everything works
 */
object Ex4Summables:
  import it.unibo.pps.u03.Sequences.Sequence, Sequence.*

  def sumAllInt(seq: Sequence[Int]): Int = seq match
    case Cons(h, t) => h + sumAllInt(t)
    case _ => 0

  trait Summable[A]:
    def sum(a1: A, a2: A): A
    def zero: A

  def sumAll[A: Summable](seq: Sequence[A]):A =
    val summable = summon[Summable[A]]
    seq match
      case Cons(h,t) => summable.sum(h,sumAll(t))
      case _         => summable.zero

  given Summable[Int] with
    def sum(a1: Int, a2: Int): Int = a1 + a2
    def zero: Int = 0

  // write givens for Summable[Double] and Summable[String]
  given Summable[Double] with
    def sum(a1: Double, a2: Double): Double = a1 + a2
    def zero: Double = 0.0

  given Summable[String] with
    def sum(a1: String, a2: String): String = a1 + a2
    def zero: String = ""

  @main def trySummables =
    val si = Cons(10, Cons(20, Cons(30, Nil())))
    println:
      sumAllInt(si) // 60

    /* uncomment from here*/

    println:
      sumAll(si) // 60

    val sd = Cons(10.0, Cons(20.0, Cons(30.0, Nil())))
    println:
      sumAll(sd) // 60.0

    val ss = Cons("10", Cons("20", Cons("30", Nil())))
    println:
      sumAll(ss) // "102030"

/*  Exercise 5:
 *  - Generalise by ad-hoc polymorphism logAll, such that:
 *  -- it can be called on Sequences but also on Optional, or others...
 *  -- it does not necessarily call log, but any function with analogous type
 *  - Hint: introduce a type class Traversable[T[_]]], capturing the ability of calling a
 *    "consumer function" on all elements (with type A) of a datastructure T[A]
 *    Note Traversable is a 2-kinded trait (similar to Filterable, or Monad)
 *  - Write givens for Traversable[Optional] and Traversable[Sequence]
 *  - Show you can use the generalisation of logAll to:
 *  -- log all elements of an Optional, or of a Traversable
 *  -- println(_) all elements of an Optional, or of a Traversable
 */
object Ex5Traversable:
  import it.unibo.pps.u03.Sequences.Sequence
  import Sequence.*
  import it.unibo.pps.u03.Optionals.Optional
  import it.unibo.pps.u03.Optionals.Optional.*

  def log[A](a: A): Unit = println("The next element is: " + a)

  trait Traversable[T[_]]:
    def foreach[A](container: T[A])(consumer: A => Unit): Unit

  def consumeAll[T[_]: Traversable, A](container: T[A])(consumer: A => Unit): Unit =
    val traversable = summon[Traversable[T]]
    traversable.foreach(container)(consumer)

  def logAll[T[_]: Traversable, A](container: T[A]): Unit =
    consumeAll(container)(log)

  given Traversable[Sequence] with
    def foreach[A](container: Sequence[A])(consumer: A => Unit): Unit =
      container match
        case Cons(h, t) =>
          consumer(h)
          foreach(t)(consumer)
        case _ => ()

  given Traversable[Optional] with
    def foreach[A](container: Optional[A])(consumer: A => Unit): Unit =
      container match
        case Just(a) => consumer(a)
        case Empty() => ()

  /* Per Sequence : se c'è una testa h, applica consumer (h) poi continua ricorsivamente sulla coda t
     se è vuota, finisce
     Per Optional : Just(a) → applica consumer (a)
     Empty() → non fa nulla.
   */

  @main def tryTraversable =
    val seq = Cons(1, Cons(2, Cons(3, Nil())))
    logAll(seq)
    consumeAll(seq)(println)

    val o1 = Just(42)
    logAll(o1)
    consumeAll(o1)(println)

    val o2 = Empty()
    logAll(o2)
    consumeAll(o2)(println)

/**
 * Exercise 6:
 This module contains the implementation of a Try monad, which is a monad that
 represents a computation that may fail.
 Try to follow these steps:
 - Look at the implementation of Try, that is similar to the one of Optional
 - Try go define the Monad instance for Try
 - flatMap should consider only the Success case
 - in case of Failure, it should return the exception (fail fast)
 - Verify that the main works as expected
 */
