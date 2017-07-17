package com.knoldus.kip.service

import com.knoldus.kip.RamDatabase
import com.knoldus.kip.models._

trait Principal {

  def findOutIfCSE(id: Int): CoursePerformance = {
    val coursePerformance : Option[CoursePerformance] = RamDatabase.getById(id)
    val courseName : Option[String] = coursePerformance.map(_.course.name)
    courseName match {
      case Some("CSE") => coursePerformance.get
      case None => throw new Exception(s"No CSE Course with this id")
    }
  }

  def findOutIfAnyCourse(id: Int, courseName: String): CoursePerformance = {
    val coursePerformance : Option[CoursePerformance] = RamDatabase.getById(id)
    val course : Option[String] = coursePerformance.map(_.course.name)
    course match {
      case Some(`courseName`) => coursePerformance.get
      case Some(a) => throw new Exception(s"$courseName not associated with id = $id")
      case None => throw new Exception(s"No course with id $id")
    }
  }

  def expression(mod: Any): String = {
    mod match {
      case Student(id, firstName, middleName,lastName, rollNumber, age, gender, enrollmentNumber,address) => "Shut up"
      case Scorecard(id,student,subjects,total,percentage,grade) => "Hmmm .... "
      case Subject(id,name,maxMarks,obtainedMarks) => "aha "
      case _ => "!!! ???"
    }
  }

  def checkScoreboard(scorecards: List[Scorecard]): List[String] = {
    for{sc <- scorecards
        sub <- sc.subjects}
      yield {sc.student.firstName + " " + sub.name + " " + sub.obtainedMarks}

  }

  def expressionRevisited: PartialFunction[ModelIdentifier, String] = {
    case Student(id, firstName, middleName,lastName, rollNumber, age, gender, enrollmentNumber,address) => "Shut up"
    case Scorecard(id,student,subjects,total,percentage,grade) => "Hmmm .... "
    case Subject(id,name,maxMarks,obtainedMarks) => "aha "
    case _ => "!!! ???"
  }

}
