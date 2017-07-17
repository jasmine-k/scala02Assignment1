package com.knoldus.kip.service

import com.knoldus.kip.RamDatabase
import com.knoldus.kip.models.{CoursePerformance, Scorecard}
//import com.knoldus.kip.objective1.models.{Scorecard, Student}

trait Postman {

  def getTheFirstAddressOfFirstYearPerformance(id: Int) :String= {

    val coursePerformance: Option[CoursePerformance] = RamDatabase.getById(id)
    val scoreCard: Option[Scorecard] = coursePerformance.flatMap(_.scoreCards.headOption)
    val address: String = scoreCard.map(_.student.getAddress).getOrElse("N/A")
    address

  }

}
