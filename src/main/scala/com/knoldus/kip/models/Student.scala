package com.knoldus.kip.models

import java.time.Year

case class Student(id: Int, firstName: String, middleName:Option[String],lastName:String,
                   rollNumber:Int, age: Option[Int], gender: Char, enrollmentNumber:Long,
                   address: Option[String] ) extends ModelIdentifier {

  def getAddress: String = address.fold("N/A"){_.toString}
  def getMiddleName: String = middleName.map(_.split(" ")(0)).getOrElse("")

}

case class Subject(id: Int,name:String, maxMarks: Float, obtainedMarks: Float) extends ModelIdentifier
case class Course(id: Int, name: String, category: String, subjects: List[Subject]) extends ModelIdentifier


case class Scorecard(id:Int, student: Student, subjects: List[Subject],total: Float, percentage: Float, grade: String) extends ModelIdentifier {

  def getSubjectsWithHighestScore: List[Subject] ={
    val highestScore = subjects.map(_.obtainedMarks).max
    subjects.filter(_.obtainedMarks == highestScore)
  }
  def getSubjectsWithLowestScore: List[Subject] ={
    val lowestScore = subjects.map(_.obtainedMarks).max
    subjects.filter(_.obtainedMarks == lowestScore)
  }
}


object Scorecard {
  def apply(student: Student, subjects: List[Subject]): Scorecard = {

    val totalMarksObtained = subjects.map(_.obtainedMarks).sum
    val percentage = totalMarksObtained/subjects.map(_.maxMarks).sum * 100

    val grade = if (percentage >=95) {
      "A+"
    }
    else if (percentage >= 90){
      "A"
    }
    else if (percentage >= 85){
      "B+"
    }
    else if(percentage >= 80){
      "C+"
    }
    else if(percentage >= 70){
      "C"
    }
    else if(percentage >= 60){
      "D+"
    }
    else if(percentage >= 50){
      "D"
    }
    else if(percentage >= 40){
      "E"
    }
    else{
      "F"
    }

    new Scorecard(student.id, student, subjects, totalMarksObtained, percentage, grade )

  }
}

case class CoursePerformance(id: Int, year: Int, course: Course, scoreCards: List[Scorecard]) extends ModelIdentifier