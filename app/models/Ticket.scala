package models
import scalikejdbc._
import play.api._
import com.github.nscala_time.time.Imports._
import ModelHelper._
import models._
import play.api.libs.json._
import play.api.libs.functional.syntax._
import play.api.libs.json.Json

case class FormData(start: String, end: String, boolValues: Seq[Boolean], strValues: Seq[String], comments: Seq[String]) {
  def getBool(idx: Int) = {
    if (idx >= boolValues.length)
      false
    else
      boolValues(idx)
  }

  var idx = 0
  def getBoolSeq() = {
    val ret = getBool(idx)
    idx += 1
    if(ret)
      "checked"
    else
      ""
  }

  def getBoolSeq(trueStr:String, falseStr:String) = {
    val ret = getBool(idx)
    idx += 1
    if(ret)
      trueStr
    else
      falseStr
  }
  
  var strIdx = 0
  def getStr(idx: Int) = {
    if (idx >= strValues.length)
      ""
    else
      strValues(idx)
  }

  def getStrSeq() = {
    val ret = getStr(strIdx)
    strIdx += 1
    ret
  }
  
  def getComment(idx:Int) = {
    if(idx >= comments.length)
      ""
    else
      comments(idx)
  }
}

case class PartFormData(id:String, source:String, charged:Boolean, unit_price:String, amount:String, total:String)
case class RepairFormData(start: String, end: String, equipmentId:String, parts:Seq[PartFormData], 
    explain:String, result:String, comment:String, boolValues:Seq[Boolean], strValues:Seq[String]){
  def getBool(idx: Int) = {
    if (idx >= boolValues.length)
      false
    else
      boolValues(idx)
  }

  def getBoolStr(idx: Int, trueStr:String, falseStr:String) = {
    if(getBool(idx))
      trueStr
    else
      falseStr
  }

  def getChecked(idx:Int) = {
    val ret = getBool(idx)

    if(ret)
      "checked"
    else
      ""
  }
  
  def getStr(idx: Int) = {
    if (idx >= strValues.length)
      ""
    else
      strValues(idx)
  }
}

object TicketType extends Enumeration {
  val maintance_week = Value
  val maintance_biweek = Value
  val maintance_month = Value
  val maintance_quarter = Value
  val maintance_half_year = Value
  val maintance_year = Value
  val repair = Value

  val map = Map(repair -> "維修", maintance_week -> "單週定保", maintance_biweek -> "雙週定保",
    maintance_month -> "月定保", maintance_quarter -> "季定保", maintance_half_year->"半年定保", maintance_year -> "年定保")

  def withId(id: Int) = {
    if (id == repair.id)
      repair
    else if (id == maintance_week.id)
      maintance_week
    else if (id == maintance_biweek.id)
      maintance_biweek
    else if (id == maintance_month.id)
      maintance_month
    else if (id == maintance_quarter.id)
      maintance_quarter
    else if (id == maintance_half_year.id)
      maintance_half_year
    else if (id == maintance_year.id)
      maintance_year
    else
      throw new NoSuchElementException
  }

  implicit val tReads: Reads[TicketType.Value] = EnumUtils.enumReads(TicketType)
  implicit val tWrites: Writes[TicketType.Value] = EnumUtils.enumWrites
}

case class Ticket(id: Int, submit_date: DateTime, active: Boolean, ticketType: TicketType.Value, submiter_id: Int, owner_id: Int, monitor: Monitor.Value,
                  monitorType: Option[MonitorType.Value], reason: String, executeDate: DateTime, formJson:String){
  
  implicit val formDataRead = Json.reads[FormData]
  implicit val formDataWrite = Json.writes[FormData]
  implicit val partDataRead = Json.reads[PartFormData]
  implicit val partDataWrite = Json.writes[PartFormData]
  implicit val repairDataRead = Json.reads[RepairFormData]
  implicit val repairDataWrite = Json.writes[RepairFormData]
  
  def getRepairForm = {
    val result = Json.parse(formJson).validate[RepairFormData]
    result.fold(
      error =>
        Ticket.defaultRepairFormData,
      success => success)      
  }
  
  def getForm = {
    val result = Json.parse(formJson).validate[FormData]
    result.fold(
      error =>
        Ticket.defaultFormData,
      success => success)    
  }
}
/**
 * @author user
 */
object Ticket {
  import scala.collection.mutable.ArrayBuffer
  implicit val formDataRead = Json.reads[FormData]
  implicit val formDataWrite = Json.writes[FormData]
  implicit val partDataRead = Json.reads[PartFormData]
  implicit val partDataWrite = Json.writes[PartFormData]
  implicit val repairDataRead = Json.reads[RepairFormData]
  implicit val repairDataWrite = Json.writes[RepairFormData]

  def newTicket(ticket: Ticket)(implicit session: DBSession = AutoSession) = {
    DB localTx { implicit session =>
      val submit_tt: java.sql.Timestamp = ticket.submit_date
      val execute_date: java.sql.Date = ticket.executeDate
      val formJson = if(ticket.ticketType == TicketType.repair)
        Json.toJson(defaultRepairFormData).toString
      else
        Json.toJson(defaultFormData).toString
      
      sql"""
        Insert into Ticket(
        [submit_date]
        ,[active]
        ,[ticketType]
        ,[submiter_id]
        ,[owner_id]
        ,[monitor]
        ,[monitorType]
        ,[reason]
        ,[execute_date]
        ,[form])
        values(${submit_tt}, ${ticket.active}, ${ticket.ticketType.id}, ${ticket.submiter_id}, ${ticket.owner_id}, ${ticket.monitor.toString},
          ${ticket.monitorType.map { _.toString }}, ${ticket.reason}, ${execute_date}, 
          ${formJson})
        """.update.apply
    }
  }

  def ticketMapper =
    { r: WrappedResultSet =>
      val ticketType = TicketType.withId(r.int(4))
      Ticket(r.int(1), r.timestamp(2), r.boolean(3), ticketType,
        r.int(5), r.int(6), Monitor.withName(r.string(7)),
        if (r.stringOpt(8).isEmpty)
          None
        else
          Some(MonitorType.withName(r.string(8))),
        r.string(9), r.date(10),
        if (r.stringOpt(11).isEmpty) {
          if (ticketType == TicketType.repair)
            Json.toJson(defaultRepairFormData).toString
          else
            Json.toJson(Ticket.defaultFormData).toString
        } else
          r.stringOpt(11).get)
    }
      
  def queryTickets(start: DateTime, end: DateTime)(implicit session: DBSession = AutoSession) = {
    sql"""
      Select *
      From Ticket
      Where execute_date between ${start} and ${end}
      Order by submit_date      
      """.map {ticketMapper}.list().apply()
  }

  
  def queryActiveTickets(start: DateTime, end: DateTime)(implicit session: DBSession = AutoSession) = {
    sql"""
      Select *
      From Ticket
      Where execute_date between ${start} and ${end} and active = 1
      Order by execute_date      
      """.map {ticketMapper}.list().apply()
  }
  
  def queryMaintanceTickets(start: DateTime, end: DateTime)(implicit session: DBSession = AutoSession) = {
    sql"""
      Select *
      From Ticket
      Where execute_date between ${start} and ${end} and ticketType != ${TicketType.repair.id} and active = 1
      Order by execute_date      
      """.map {ticketMapper}.list().apply()
  }
  
  def queryAllMaintanceTickets(start: DateTime, end: DateTime)(implicit session: DBSession = AutoSession) = {
    sql"""
      Select *
      From Ticket
      Where execute_date between ${start} and ${end} and ticketType != ${TicketType.repair.id}
      Order by execute_date      
      """.map {ticketMapper}.list().apply()
  }
    
  def myTickets(ID: Int)(implicit session: DBSession = AutoSession) = {
    sql"""
      Select *
      From Ticket
      Where owner_id = ${ID} and active = 1
      Order by submit_date      
      """.map { ticketMapper}.list().apply()
  }

  def ticketSubmittedByMe(ID: Int)(implicit session: DBSession = AutoSession) = {
    sql"""
      Select *
      From Ticket
      Where submiter_id = ${ID} and active = 1
      Order by submit_date      
      """.map { ticketMapper}.list().apply()
  }
  
  def getTicket(ID: Int)(implicit session: DBSession = AutoSession) = {
    sql"""
      Select *
      From Ticket
      Where id = ${ID}
      """.map {ticketMapper}.single().apply()
  }

  def updateTicket(ticket: Ticket)(implicit session: DBSession = AutoSession) = {
    DB localTx { implicit session =>
        sql"""
        Update Ticket
        Set [ticketType]=${ticket.ticketType.id}, [owner_id]=${ticket.owner_id},
          [monitor]=${ticket.monitor.toString}, [monitorType]=${ticket.monitorType.map { _.toString }},
          [reason]=${ticket.reason}, [execute_date]=${ticket.executeDate.toDate}, [form] = ${ticket.formJson}
        Where ID = ${ticket.id}
        """.update.apply
    }
  }

  def transferTickets(old_id:Int, new_id:Int) = {
    DB localTx { implicit session =>
        sql"""
        Update Ticket
        Set [submiter_id]=${new_id}, [owner_id] = ${new_id}
        Where submiter_id = ${old_id} or owner_id = ${old_id}
        """.update.apply
    }
  }
  
  def closeTicket(id: List[Int])(implicit session: DBSession = AutoSession) = {
    DB localTx { implicit session =>
      sql"""
        Update Ticket
        Set active = 0
        Where ID in (${id})
        """.update.apply
    }
  }
  
  val defaultFormData = FormData("", "", Seq.empty[Boolean], Seq.empty[String], Seq.empty[String])
  val defaultRepairFormData = RepairFormData("", "", "", Seq.empty[PartFormData], "", "", "", Seq.empty[Boolean], Seq.empty[String])
  
  def updateTicketFormData(ID: Int, form: FormData)(implicit session: DBSession = AutoSession) = {
    DB localTx { implicit session =>
      sql"""
        Update Ticket
        Set [form]=${Json.toJson(form).toString}
        Where ID = ${ID}
        """.update.apply
    }
  }
  
  def updateRepairFormData(ID: Int, form: RepairFormData)(implicit session: DBSession = AutoSession) = {
    DB localTx { implicit session =>
      sql"""
        Update Ticket
        Set [form]=${Json.toJson(form).toString}
        Where ID = ${ID}
        """.update.apply
    }
  }
  
}