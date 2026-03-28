package it.unibo.pps.tasks.adts

import org.junit.*
import org.junit.Assert.*

import SchoolModel.*
import it.unibo.pps.u03.Sequences.Sequence
import Sequence.*
import BasicSchoolModule.*

class SchoolModelTest:

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