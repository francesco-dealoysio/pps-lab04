package it.unibo.pps.tasks.adts

//import it.unibo.pps.u03.extensionmethods.Sequences.Sequence, Sequence.*

/*  Exercise 2: 
 *  Implement the below trait, and write a meaningful test.
 *  Suggestions:
 *  - reuse Sequences and Optionals as imported above
 *  - For other suggestions look directly to the methods and their description
 */
object _SchoolModel:
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
      case Nil()           => false

  @main def examples =
    println("nop")