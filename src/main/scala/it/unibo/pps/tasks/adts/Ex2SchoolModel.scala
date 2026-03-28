package it.unibo.pps.tasks.adts

import it.unibo.pps.u03.Sequences.Sequence, Sequence.*
//import it.unibo.pps.u03.extensionmethods.Sequences.Sequence, Sequence.*

/*  Exercise 2: 
 *  Implement the below trait, and write a meaningful test.
 *  Suggestions:
 *  - reuse Sequences and Optionals as imported above
 *  - For other suggestions look directly to the methods and their description
 */
object SchoolModel:

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

      //def asSequence(): Sequence[]

  object BasicSchoolModule extends SchoolModule:
    override type School = Sequence[(Teacher, Course)]
    override type Teacher = String
    override type Course = String

    def teacher(name: String): Teacher = name
    def course(name: String): Course = name
    def emptySchool: School = Nil()

    extension (school: School)

      def courses: Sequence[String] =
        def inner(school: School, seq: Sequence[String]): Sequence[String] = school match
          case Cons((teacher, course), tail) => inner(tail, Cons(course, seq))
          case _                             => distinct(seq)
        inner(school, Nil())

      def teachers: Sequence[String] =
        def inner(school: School, seq: Sequence[String]): Sequence[String] = school match
          case Cons((teacher, course), tail) => inner(tail, Cons(teacher, seq))
          case _                             => distinct(seq)
        inner (school, Nil ())

      def setTeacherToCourse(teacher: Teacher, course: Course): School =
        Cons((teacher, course), school)

      def coursesOfATeacher(teacher: Teacher): Sequence[Course] =
        def inner(school: School, name: String, seq: Sequence[String]): Sequence[String] = school match
          case Cons((teacher, course), tail) if teacher == name => inner(tail, name, Cons(course, seq))
          case Cons((teacher, course), tail)                    => inner(tail, name, seq)
          case _                                                => distinct(seq)
        inner(school, teacher, Nil())

      def hasTeacher(name: String): Boolean =
        def inner(school: School, name: String): Boolean = school match
          case Cons((teacher, course), tail) => teacher == name || inner(tail, name)
          case Nil()                         => false
        inner(school, name)

      def hasCourse(name: String): Boolean =
        def inner(school: School, name: String): Boolean = school match
          case Cons((teacher, course), tail) => course == name || inner(tail, name)
          case Nil()                         => false
        inner(school, name)

@main def examples(): Unit =
  println("nop")

/*
  import SchoolModel.BasicSchoolModule.*

  val school = emptySchool
  println(school.teachers) // Nil()
  println(school.courses) // Nil()
  println("John: " + school.hasTeacher("John")) // false
  println(school.hasCourse("Math")) // false
  val john = teacher("John")
  val math = course("Math")
  val italian = course("Italian")
  val school2 = school.setTeacherToCourse(john, math)
  println(school2.teachers) // Cons("John", Nil())
  println(school2.courses) // Cons("Math", Nil())
  println(school2.hasTeacher("John")) // true
  println(school2.hasCourse("Math")) // true
  println(school2.hasCourse("Italian")) // false
  val school3 = school2.setTeacherToCourse(john, italian)
  println(school3.teachers) // Cons("John", Nil())
  println(school3.courses) // Cons("Math", Cons("Italian", Nil()))
  println(school3.hasTeacher("John")) // true
  println(school3.hasCourse("Math")) // true
  println(school3.hasCourse("Italian")) // true
  println(school3.coursesOfATeacher(john)) // Cons("Math", Cons("Italian", Nil()))
*/