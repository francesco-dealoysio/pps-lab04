package it.unibo.pps.tasks.adts

import org.junit.*
import org.junit.Assert.*

/* Lab04 test classes:
 * - ComplexTest
 * - SchoolModelTest
 * - StackTest
 */

class ComplexTest:
  import Ex1ComplexNumbers.*

  // Choice of implementation to test
  val complexADT: ComplexADT = BasicComplexADT
  import complexADT.*

  // From now, everything is independent of specific implementation of Complex

  @Test def testReal() =
    assertEquals(10, complex(10, 20).re(), 0)

  @Test def testImaginary() =
    assertEquals(20, complex(10, 20).im(), 0)

  @Test def testSum() =
    assertEquals(complex(11, 22), complex(10, 20) sum complex(1, 2))

  @Test def testSubtract() =
    assertEquals(complex(9, 18), complex(10, 20) subtract complex(1, 2))

  @Test def testAsString() =
    assertEquals("10.0 + 5.0i", complex(10.0, 5.0).asString())

  @Test def optionalTestAdvancedAsString() =
    assertEquals("0.0", complex(0.0, 0.0).asString())
    assertEquals("10.0", complex(10.0, 0).asString())
    assertEquals("10.0 + 5.0i", complex(10.0, 5.0).asString())
    assertEquals("10.0 - 5.0i", complex(10.0, -5.0).asString())
    assertEquals("5.0i", complex(0, 5.0).asString())
    assertEquals("-5.0i", complex(0, -5.0).asString())

class SchoolModelTest:
  import SchoolModel.*
  import it.unibo.pps.u03.Sequences.Sequence
  import Sequence.*
  import BasicSchoolModule.*

  @Test def testCreateTeacher() =
    assertEquals("John", teacher("John"))

  @Test def testCreateCourse() =
    assertEquals("Math", course("Math"))

  @Test def testEmptySchool() =
    assertEquals(Sequence.Nil(), emptySchool)

  @Test def testCourses() =
    var school = emptySchool.setTeacherToCourse(teacher("Mark"), course("English"))
    school = school.setTeacherToCourse(teacher("John"), course("Math"))
    school = school.setTeacherToCourse(teacher("Frank"), course("Italian"))
    school = school.setTeacherToCourse(teacher("Daniel"), course("Italian"))
    assertEquals(Cons("English", Cons("Math", Cons("Italian", Nil()))), courses(school))

  @Test def testNoCourses() =
    val school = emptySchool
    assertEquals(Nil(), courses(school))

  @Test def testHasCourse() =
    var school = emptySchool.setTeacherToCourse(teacher("Mark"), course("English"))
    school = school.setTeacherToCourse(teacher("John"), course("Math"))
    school = school.setTeacherToCourse(teacher("John"), course("Latin"))
    school = school.setTeacherToCourse(teacher("Frank"), course("Italian"))
    assertTrue(school.hasCourse("Math"))
    assertFalse(school.hasCourse("Logic"))

  @Test def testEmptySchoolHasCourse() =
    var school = emptySchool
    assertFalse(school.hasCourse("John"))

  @Test def testTeachers() =
    var school = emptySchool.setTeacherToCourse(teacher("Mark"), course("English"))
    school = school.setTeacherToCourse(teacher("John"), course("Math"))
    school = school.setTeacherToCourse(teacher("John"), course("Latin"))
    school = school.setTeacherToCourse(teacher("Frank"), course("Italian"))
    assertEquals(Cons("Mark", Cons("John", Cons("Frank", Nil()))), teachers(school))

  @Test def testNoTeachers() =
    val school = emptySchool
    assertEquals(Nil(), teachers(school))

  @Test def testHasTeacher() =
    var school = emptySchool.setTeacherToCourse(teacher("Mark"), course("English"))
    school = school.setTeacherToCourse(teacher("John"), course("Math"))
    school = school.setTeacherToCourse(teacher("John"), course("Latin"))
    school = school.setTeacherToCourse(teacher("Frank"), course("Italian"))
    assertTrue(school.hasTeacher("John"))
    assertFalse(school.hasTeacher("Daniel"))

  @Test def testEmptySchoolHasTeacher() =
    var school = emptySchool
    assertFalse(school.hasTeacher("John"))

  @Test def testSetTeacherToCourse() =
    val school = emptySchool
    val john = teacher("John")
    val math = course("Math")
    assertEquals(Cons(("John", "Math"), Nil()), school.setTeacherToCourse(john, math))

  @Test def testMultipleSetTeacherToCourse() =
    val john = teacher("John")
    val math = course("Math")
    val school = emptySchool.setTeacherToCourse(john, math).setTeacherToCourse(teacher("Mark"), course("English"))
    assertEquals(Cons(("Mark","English"),Cons(("John","Math"),Nil())), school)

  @Test def testCoursesOfATeacher() =
    var school = emptySchool.setTeacherToCourse(teacher("Mark"), course("English"))
    school = school.setTeacherToCourse(teacher("John"), course("Math"))
    school = school.setTeacherToCourse(teacher("John"), course("Latin"))
    school = school.setTeacherToCourse(teacher("Frank"), course("Italian"))
    assertEquals(Cons("Math", Cons("Latin", Nil())), school.coursesOfATeacher("John"))
    assertEquals(Nil(), school.coursesOfATeacher("Daniel"))
    assertEquals(Nil(), emptySchool.coursesOfATeacher("Daniel"))

class Stacktest:
  import Ex3Stacks.StackImpl
  import it.unibo.pps.u03.Sequences.Sequence
  import it.unibo.pps.u03.Optionals.Optional
  import stack.*

  val stack = StackImpl

  @Test def testEmptyStackHasNoElements() =
    assertEquals(Sequence.Nil(), empty[Int].asSequence())

  @Test def testPushAddsElementToStack() =
    assertEquals(Sequence.Cons(10, Sequence.Nil()), empty[Int].push(10).asSequence())

  @Test def testPopOnEmptyStackReturnsEmpty() =
    assertEquals(Optional.Empty(), empty[Int].pop())

  @Test def testPopOnStackWithOneElementReturnsElementAndEmptyStack() =
    assertEquals(Optional.Just((10, empty[Int])), empty[Int].push(10).pop())

  @Test def testPushMultipleElementsAndVerifyOrder() =
    val stack = empty[Int].push(10).push(20).push(30)
    assertEquals(Sequence.Cons(30, Sequence.Cons(20, Sequence.Cons(10, Sequence.Nil()))), stack.asSequence())

  @Test def testPopMultipleElementsMaintainsOrder() =
    val stack = empty[Int].push(10).push(20)
    val popResult = stack.pop()
    assertEquals(Optional.Just((20, empty[Int].push(10))), popResult)