package com.knoldus.kip.service

import com.knoldus.kip.RamDatabase
import com.knoldus.kip.models._
import org.scalatest.FunSuite


class PrincipalTest extends FunSuite with Principal {


  val MAX_MARKS100 = 100

  val subjectMCADM1: Subject = Subject(1, "Discrete Mathematics", 35, MAX_MARKS100)
  val subjectMCACO1: Subject = Subject(1, "Computer Organization", 79, MAX_MARKS100)
  val subjectMCABN1: Subject = Subject(1, "Basic Networking", 81, MAX_MARKS100)

  val subjectMCADM2: Subject = Subject(1, "Discrete Mathematics", 55, MAX_MARKS100)
  val subjectMCACO2: Subject = Subject(1, "Computer Organization", 29, MAX_MARKS100)
  val subjectMCABN2: Subject = Subject(1, "Basic Networking", 61, MAX_MARKS100)

  val subjectBTechCG1: Subject = Subject(2, "Computer Graphics", 59, MAX_MARKS100)
  val subjectBTechCA1: Subject = Subject(2, "Computer Architecture", 91, MAX_MARKS100)
  val subjectBTechAN1: Subject = Subject(2, "Advanced Networking", 38, MAX_MARKS100)

  val subjectBTechCG2: Subject = Subject(2, "Computer Graphics", 79, MAX_MARKS100)
  val subjectBTechCA2: Subject = Subject(2, "Computer Architecture", 91, MAX_MARKS100)
  val subjectBTechAN2: Subject = Subject(2, "Advanced Networking", 88, MAX_MARKS100)

  val subjectListMcaWithOneFailure1: List[Subject] = List(subjectMCADM1, subjectMCACO1, subjectMCABN1)
  val subjectListMcaWithOneFailure2: List[Subject] = List(subjectMCADM2, subjectMCACO2, subjectMCABN2)
  val subjectListBtechWithOneFailure1: List[Subject] = List(subjectBTechCG1, subjectBTechCA1, subjectBTechAN1)
  val subjectListBtechWithZeroFailure1: List[Subject] = List(subjectBTechCG2, subjectBTechCA2, subjectBTechAN2)

  val courseAndSubjectsMCA1: Course = Course(1, "MCA", "compuer application", subjectListMcaWithOneFailure1)
  val courseAndSubjectsMCA2: Course = Course(1, "MCA", "compuer application", subjectListMcaWithOneFailure2)

  val courseAndSubjectsBTech1: Course = Course(2, "CSE", "engineering", subjectListBtechWithOneFailure1)
  val courseAndSubjectsBTech2: Course = Course(2, "CSE", "engineering", subjectListBtechWithZeroFailure1)


  val student1: Student = Student(1, "Ashish", Option("Kumar"), "Tomer", 131, None, 'm', 1032017071102L, Some("Noida"))
  val student2: Student = Student(1, "Gaurav", None, "Mishra", 133, Some(26), 'm', 1082017071102L, None)
  val student3: Student = Student(2, "Harshit", Some("Kumar"), "Daga", 132, Some(25), 'm', 1032017071103L, Some("Lucknow"))
  val student4: Student = Student(2, "Girish", None, "Bharti", 128, None, 'm', 1082017071102L, None)


  val scorecardAshish: Scorecard = Scorecard(student1, subjectListMcaWithOneFailure1)
  val scorecardGaurav: Scorecard = Scorecard(student2, subjectListMcaWithOneFailure2)
  val scorecardHarshit: Scorecard = Scorecard(student3, subjectListBtechWithOneFailure1)
  val scorecardGirish: Scorecard = Scorecard(student4, subjectListBtechWithZeroFailure1)

  val coursePerformance1: CoursePerformance = CoursePerformance(1, 2017, courseAndSubjectsMCA1,
    List(scorecardAshish, scorecardGaurav))

  val coursePerformance2: CoursePerformance = CoursePerformance(2, 2017, courseAndSubjectsBTech2,
    List(scorecardGirish, scorecardHarshit))

  test("testing findOutIfCSE") {

    assert(findOutIfCSE(2) == coursePerformance2)

    intercept[java.lang.Exception] {
      findOutIfCSE(1)
    }
  }


  test("testing findOutIfAnyCourse") {

    assert(findOutIfAnyCourse(1, "MCA") == coursePerformance1)

    intercept[java.lang.Exception] {
      findOutIfAnyCourse(2, "MCA")
    }
  }


  test("testing expression"){
    assert(expression(student1).equals("Shut up"))
    assert(expression(scorecardAshish).equals("Hmmm .... "))
    assert(expression(subjectBTechAN1).equals("aha "))
    assert(expression(courseAndSubjectsBTech1).equals("!!! ???"))

  }

  test("expression revisited"){
    assert(expression(student1).equals("Shut up"))
    assert(expression(scorecardAshish).equals("Hmmm .... "))
    assert(expression(subjectBTechAN1).equals("aha "))
    assert(expression(courseAndSubjectsBTech1).equals("!!! ???"))
  }
}