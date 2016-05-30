package models
import scalikejdbc._
import play.api._
import com.github.nscala_time.time.Imports._
import ModelHelper._
import models._
import play.api.libs.json._
import play.api.libs.functional.syntax._
import play.api.libs.json.Json
import scala.concurrent.ExecutionContext.Implicits.global
import Alarm._

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
    if (ret)
      "checked"
    else
      ""
  }

  def getBoolSeq(trueStr: String, falseStr: String) = {
    val ret = getBool(idx)
    idx += 1
    if (ret)
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

  def getComment(idx: Int) = {
    if (idx >= comments.length)
      ""
    else
      comments(idx)
  }
}

case class PartFormData(id: String, source: String, charged: Boolean, unit_price: String, amount: String, total: String)
case class RepairFormData(start: String, end: String, equipmentId: String, parts: Seq[PartFormData], alarm: Option[Alarm],
                          explain: String, result: String, comment: String, boolValues: Seq[Boolean], strValues: Seq[String]) {
  def getBool(idx: Int) = {
    if (idx >= boolValues.length)
      false
    else
      boolValues(idx)
  }

  def getBoolStr(idx: Int, trueStr: String, falseStr: String) = {
    if (getBool(idx))
      trueStr
    else
      falseStr
  }

  def getChecked(idx: Int) = {
    val ret = getBool(idx)

    if (ret)
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
    maintance_month -> "月定保", maintance_quarter -> "季定保", maintance_half_year -> "半年定保", maintance_year -> "年定保")

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
                  monitorType: Option[MonitorType.Value], reason: String, executeDate: DateTime, formJson: String,
                  repairType: Option[String], repairSubType: Option[String], readyToClose: Option[Boolean]) {

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

      val ret = sql"""
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
        ,[form]
        ,[repairType]
        ,[repairSubType]
        ,[readyToClose])
        values(${submit_tt}, ${ticket.active}, ${ticket.ticketType.id}, ${ticket.submiter_id}, ${ticket.owner_id}, ${ticket.monitor.toString},
          ${ticket.monitorType.map { _.toString }}, ${ticket.reason}, ${execute_date}, 
          ${ticket.formJson},
          ${ticket.repairType},
          ${ticket.repairSubType},
          ${ticket.readyToClose})
        """.update.apply

      if (ticket.ticketType == TicketType.repair) {
        import scala.concurrent._
        Future {
          blocking {
            import controllers.ExcelUtility
            val excel = ExcelUtility.epbNotification(List(ticket))
            val title = s"環保局通報單_${Monitor.map(ticket.monitor).name}${ticket.submit_date.toString("MMdd_HHmm")}"

            import play.api.Play.current
            import java.nio.file.StandardCopyOption._
            import java.nio.file.Paths
            import java.nio.file.Files

            val targetPath = Paths.get(current.path.getAbsolutePath + s"/notification/${title}.xlsx")
            Files.move(excel.toPath, targetPath, REPLACE_EXISTING)

            import play.api.libs.mailer._
            import play.api.Play.current
            val userOpt = User.getUserById(ticket.owner_id)
            if (userOpt.isDefined) {
              val user = userOpt.get
              val ticketTypeStr = TicketType.map(ticket.ticketType)
              val mtType = ticket.monitorType.map(MonitorType.map(_).desp)
              val msg = s"維修案件  : ${Monitor.map(ticket.monitor).name}-${ModelHelper.formatOptStr(mtType)}"
              val htmlMsg = s"<html><body><p><b>$msg</b></p></body></html>"

              val email = Email(
                s"維修案件: ${Monitor.map(ticket.monitor).name}-${ModelHelper.formatOptStr(mtType)}",
                "案件通報 <karateboy.huang@gmail.com>",
                List(user.email),
                // adds attachment
                attachments = Seq(),
                // sends text, HTML or both...
                bodyText = Some(msg),
                bodyHtml = Some(htmlMsg))

              MailerPlugin.send(email)
              SmsSender.send(List(user), msg)
            }
          }
        }
      }

      ret
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
          r.stringOpt(11).get,
        r.stringOpt(12),
        r.stringOpt(13),
        r.booleanOpt(14))
    }

  def queryTickets(start: DateTime, end: DateTime)(implicit session: DBSession = AutoSession) = {
    sql"""
      Select *
      From Ticket
      Where execute_date between ${start} and ${end}
      Order by submit_date      
      """.map { ticketMapper }.list().apply()
  }

  def queryClosedRepairTickets(start: DateTime, end: DateTime)(implicit session: DBSession = AutoSession) = {
    sql"""
      Select *
      From Ticket
      Where execute_date between ${start} and ${end} and ticketType = ${TicketType.repair.id} and active = 0
      Order by submit_date      
      """.map { ticketMapper }.list().apply()
  }

  def queryActiveTickets(start: DateTime, end: DateTime)(implicit session: DBSession = AutoSession) = {
    sql"""
      Select *
      From Ticket
      Where execute_date between ${start} and ${end} and active = 1
      Order by execute_date      
      """.map { ticketMapper }.list().apply()
  }

  def queryMaintanceTickets(start: DateTime, end: DateTime)(implicit session: DBSession = AutoSession) = {
    sql"""
      Select *
      From Ticket
      Where execute_date between ${start} and ${end} and ticketType != ${TicketType.repair.id} and active = 1
      Order by execute_date      
      """.map { ticketMapper }.list().apply()
  }

  def queryAllMaintanceTickets(start: DateTime, end: DateTime)(implicit session: DBSession = AutoSession) = {
    sql"""
      Select *
      From Ticket
      Where execute_date between ${start} and ${end} and ticketType != ${TicketType.repair.id}
      Order by execute_date      
      """.map { ticketMapper }.list().apply()
  }

  def queryActiveRepairTickets()(implicit session: DBSession = AutoSession) = {
    sql"""
      Select *
      From Ticket
      Where ticketType = ${TicketType.repair.id} and active = 1
      Order by execute_date      
      """.map { ticketMapper }.list().apply()
  }

  def getActiveRepairTicketsByGroup(groupId:Int)(implicit session: DBSession = AutoSession) = {
    val users = User.getUserByGroup(groupId)
    val userIdList = users.map { _.id.get }
    val userIdStr = SQLSyntax.createUnsafely(userIdList.mkString("('", "','", "')"))
    sql"""
      Select *
      From Ticket
      Where ticketType = ${TicketType.repair.id} and active = 1 and submiter_id in $userIdStr
      Order by execute_date      
      """.map { ticketMapper }.list().apply()
  }

  def myTickets(ID: Int)(implicit session: DBSession = AutoSession) = {
    sql"""
      Select *
      From Ticket
      Where owner_id = ${ID} and active = 1 and readyToClose = 0
      Order by submit_date      
      """.map { ticketMapper }.list().apply()
  }

  def ticketSubmittedByMe(ID: Int, readyToClose: Boolean = true)(implicit session: DBSession = AutoSession) = {
    val bit = if (readyToClose) 1 else 0
    sql"""
      Select *
      From Ticket
      Where submiter_id = ${ID} and active = 1 and readyToClose = $bit
      Order by submit_date      
      """.map { ticketMapper }.list().apply()
  }

  def getTicket(ID: Int)(implicit session: DBSession = AutoSession) = {
    sql"""
      Select *
      From Ticket
      Where id = ${ID}
      """.map { ticketMapper }.single().apply()
  }

  case class TicketPhoto(photos: List[Option[java.sql.Blob]])
  def getTicketPhoto(ID: Int)(implicit session: DBSession = AutoSession) = {
    sql"""
      Select Photo1, Photo2
      From Ticket
      Where id = ${ID}
      """.map { rs =>
      TicketPhoto(List(rs.blobOpt(1), rs.blobOpt(2)))
    }.single().apply()
  }

  def getPreviousTicket(ticket: Ticket)(implicit session: DBSession = AutoSession) = {
    sql"""
      Select Top 1 *
      From Ticket
      Where ticketType=${ticket.ticketType.id} and monitor=${ticket.monitor.toString()} and execute_date < ${ticket.executeDate.toDate()}
      Order by execute_date desc
      """.map { ticketMapper }.single().apply()
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

  import java.io.File
  def attachPhoto1(id: Int, photo: File)(implicit session: DBSession = AutoSession) = {
    import java.io.InputStream
    import java.io.FileInputStream
    import java.sql._

    val inputStream = new FileInputStream(photo)
    val bytesBinder = ParameterBinder[InputStream](
      inputStream,
      binder = (stmt: PreparedStatement, idx: Int) => stmt.setBinaryStream(idx, inputStream, photo.length))

    sql"""
      Update Ticket
      Set photo1 = ${bytesBinder}
      Where ID = $id
      """.update.apply
  }

  def attachPhoto2(id: Int, photo: File)(implicit session: DBSession = AutoSession) = {
    import java.io.InputStream
    import java.io.FileInputStream
    import java.sql._

    val inputStream = new FileInputStream(photo)
    val bytesBinder = ParameterBinder[InputStream](
      inputStream,
      binder = (stmt: PreparedStatement, idx: Int) => stmt.setBinaryStream(idx, inputStream, photo.length))

    sql"""
      Update Ticket
      Set photo2 = ${bytesBinder}
      Where ID = $id
      """.update.apply
  }

  def transferTickets(old_id: Int, new_id: Int) = {
    DB localTx { implicit session =>
      sql"""
        Update Ticket
        Set [submiter_id]=${new_id}, [owner_id] = ${new_id}
        Where submiter_id = ${old_id} or owner_id = ${old_id}
        """.update.apply
    }
  }

  def closeTicket(id: List[Int], userId: Int)(implicit session: DBSession = AutoSession) = {
    DB localTx { implicit session =>
      sql"""
        Update Ticket
        Set active = 0
        Where ID in (${id}) and submiter_id = $userId
        """.update.apply
    }
  }

  def takeOverTicket(id: List[Int], userId: Int)(implicit session: DBSession = AutoSession) = {
    DB localTx { implicit session =>
      sql"""
        Update Ticket
        Set owner_id = ${userId}
        Where ID in (${id})
        """.update.apply
    }
  }

  def readyToCloseOwnerTicket(id: List[Int], userId: Int, value: Boolean = true)(implicit session: DBSession = AutoSession) = {
    DB localTx { implicit session =>
      val bit = if (value) 1 else 0
      sql"""
        Update Ticket
        Set readyToClose = $bit
        Where ID in (${id}) and owner_id = $userId
        """.update.apply
    }
  }

  def readyToCloseSubmitterTicket(id: List[Int], userId: Int, value: Boolean)(implicit session: DBSession = AutoSession) = {
    DB localTx { implicit session =>
      val bit = if (value) 1 else 0
      sql"""
        Update Ticket
        Set readyToClose = $bit
        Where ID in (${id}) and submiter_id = $userId
        """.update.apply
    }
  }

  val defaultFormData = FormData("", "", Seq.empty[Boolean], Seq.empty[String], Seq.empty[String])
  val defaultRepairFormData = RepairFormData("", "", "", Seq.empty[PartFormData], None, "", "", "", Seq.empty[Boolean], Seq.empty[String])
  def defaultAlarmTicketForm(alarm:Alarm) = RepairFormData("", "", "", Seq.empty[PartFormData], Some(alarm), "", "", "", Seq.empty[Boolean], Seq.empty[String])

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