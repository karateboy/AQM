package controllers

import play.api._
import play.api.mvc._
import play.api.Logger
import play.api.libs.mailer._
import models._
import models.Realtime._
import com.github.nscala_time.time.Imports._
import models.ModelHelper._
import play.api.libs.json._
import play.api.libs.functional.syntax._
import play.api.libs.json.Json
import play.api.Play.current
import PdfUtility._
import play.api.libs.json._
import play.api.libs.functional.syntax._
import Ticket._
import java.nio.file.Files

/**
 * @author user
 */
object Maintance extends Controller {
  def newTicket = Security.Authenticated {
    implicit request =>
      val userInfo = Security.getUserinfo(request).get
      val group = Group.getGroup(userInfo.groupID).get
      val adminUsers = User.getAdminUsers()
      val ticketTypes = TicketType.values.toList.sorted
      Ok(views.html.newTicket(userInfo, group.privilege, adminUsers, ticketTypes))
  }

  case class TicketParam(ticketType: TicketType.Value, monitors: Seq[Monitor.Value],
                         monitorTypes: Seq[MonitorType.Value], reason: String, owner: Int, executeDate: Seq[String],
                         repairType: Option[String], repairSubType: Option[String])

  implicit val newTicketParamRead = Json.reads[TicketParam]

  def newTicketAction(idStr: String) = Security.Authenticated(BodyParsers.parse.json) {
    implicit request =>
      val submiterId = Integer.parseInt(idStr)
      val newTicketJson = request.body.validate[TicketParam]

      newTicketJson.fold(
        error => {
          Logger.error(JsError.toFlatJson(error).toString())
          BadRequest(Json.obj("ok" -> false, "msg" -> JsError.toFlatJson(error)))
        },
        ticketParam => {
          val tickets =
            if (ticketParam.ticketType != TicketType.repair) {
              for {
                m <- ticketParam.monitors
                date <- ticketParam.executeDate
              } yield {
                Ticket(0, DateTime.now, true, ticketParam.ticketType, submiterId,
                  ticketParam.owner, m, None, ticketParam.reason, DateTime.parse(date), Json.toJson(Ticket.defaultFormData).toString,
                  None, None, Some(false))
              }
            } else {
              for {
                m <- ticketParam.monitors
                mt <- ticketParam.monitorTypes
                date <- ticketParam.executeDate
              } yield {
                Ticket(0, DateTime.now, true, ticketParam.ticketType, submiterId,
                  ticketParam.owner, m, Some(mt), ticketParam.reason, DateTime.parse(date), Json.toJson(Ticket.defaultRepairFormData).toString,
                  ticketParam.repairType, ticketParam.repairSubType, Some(false))
              }
            }
          for (t <- tickets)
            Ticket.newTicket(t)

          Ok(Json.obj("ok" -> true, "nNewCase" -> tickets.length))
        })
  }

  def queryTicket = Security.Authenticated {
    implicit request =>
      val userInfo = Security.getUserinfo(request).get
      val group = Group.getGroup(userInfo.groupID).get
      val adminUsers = User.getAdminUsers()
      val ticketTypeList = TicketType.values.toList.sorted

      Ok(views.html.queryTicket(userInfo, group.privilege, adminUsers, ticketTypeList))
  }

  def ticketReport(ticketTypeStr: String, monitorStr: String, startStr: String, endStr: String) = Security.Authenticated {
    val ticketTypes = ticketTypeStr.split(":").map { TicketType.withName }
    val monitors = monitorStr.split(":").map { Monitor.withName }
    val start = DateTime.parse(startStr)
    val end = DateTime.parse(endStr) + 1.day

    val tickets = Ticket.queryTickets(start, end)
    val filterTicket = tickets.filter { t => ticketTypes.contains(t.ticketType) && monitors.contains(t.monitor) }
    val allUsers = User.getAllUsers()
    val usrMap = Map(allUsers.map { u => (u.id.get -> u) }: _*)

    Ok(views.html.ticketReport(filterTicket, usrMap))
  }

  def myTicket = Security.Authenticated {
    implicit request =>
      val userInfoOpt = Security.getUserinfo(request)
      if (userInfoOpt.isEmpty) {
        Forbidden("Invalid access!")
      } else {
        val userInfo = userInfoOpt.get
        val group = Group.getGroup(userInfo.groupID).get
        val tickets = Ticket.myTickets(userInfo.id)
        val allUsers = User.getAllUsers()
        val usrMap = Map(allUsers.map { u => (u.id.get -> u) }: _*)

        Ok(views.html.myTicket(tickets, usrMap))
      }
  }

  def ticket(ID: Int) = Security.Authenticated {
    implicit request =>

      val ticketOpt = Ticket.getTicket(ID)

      if (ticketOpt.isEmpty)
        BadRequest("No such ticket!")
      else {
        val userInfo = Security.getUserinfo(request).get
        val group = Group.getGroup(userInfo.groupID).get
        val adminUsers = User.getAdminUsers()

        Ok(views.html.ticket(ticketOpt.get, group.privilege, adminUsers))
      }
  }

  def updateTicket(ID: Int) = Security.Authenticated(BodyParsers.parse.json) {
    implicit request =>
      val ticketResult = request.body.validate[TicketParam]

      ticketResult.fold(
        error => {
          Logger.error(JsError.toFlatJson(error).toString())
          BadRequest(Json.obj("ok" -> false, "msg" -> JsError.toFlatJson(error)))
        },
        param => {
          val ticketOpt = Ticket.getTicket(ID)
          if (ticketOpt.isEmpty)
            BadRequest(Json.obj("ok" -> false, "msg" -> "無此案件"))
          else {
            val t = ticketOpt.get
            val mt = if (param.monitorTypes.length == 0)
              None
            else
              Some(param.monitorTypes(0))
            val executeDate = DateTime.parse(param.executeDate(0))
            val newT = Ticket(ID,
              t.submit_date,
              t.active,
              param.ticketType,
              t.submiter_id,
              param.owner,
              param.monitors(0),
              mt,
              param.reason,
              executeDate,
              if (t.ticketType == param.ticketType) {
                t.formJson
              } else {
                if (param.ticketType == TicketType.repair)
                  Json.toJson(defaultRepairFormData).toString
                else
                  Json.toJson(Ticket.defaultFormData).toString
              }, t.repairType, t.repairSubType, t.readyToClose)

            Ticket.updateTicket(newT)
            Ok(Json.obj("ok" -> true))
          }
        })
  }

  def attachTicketPhoto(id: Int) = Security.Authenticated(parse.multipartFormData) {
    implicit request =>
      request.body.file("photo1").map { picture =>
        import java.io.File
        val filename = picture.filename
        val contentType = picture.contentType
        Ticket.attachPhoto1(id, picture.ref.file)
      }

      request.body.file("photo2").map { picture =>
        import java.io.File
        val filename = picture.filename
        val contentType = picture.contentType
        Ticket.attachPhoto2(id, picture.ref.file)
      }

      Ok(Json.obj("ok" -> true))
  }

  def getNoPhotoByteArray = {
    import java.io.FileInputStream
    val is = new FileInputStream(current.path.getAbsolutePath + "/public/images/no_photo.png")
    import org.apache.commons.io._
    val ba = IOUtils.toByteArray(is)
    is.close()
    ba
  }

  def getTicketPhoto(id: Int, idx: Int) = Security.Authenticated {
    implicit request =>
      import org.apache.commons.io._
      val ticketPhotoOpt = Ticket.getTicketPhoto(id)
      if (ticketPhotoOpt.isDefined) {
        val ticketPhoto = ticketPhotoOpt.get
        val byte1 =
          ticketPhoto.photos(idx).map { blob => IOUtils.toByteArray(blob.getBinaryStream) }

        if (byte1.isDefined) {
          Ok(byte1.get).as("image/jpeg")
        } else {
          Ok(getNoPhotoByteArray).as("image/png")
        }
      } else
        Ok(getNoPhotoByteArray).as("image/png")
  }

  def updateForm(ID: Int) = Security.Authenticated(BodyParsers.parse.json) {
    import Ticket._
    implicit request =>
      val formResult = request.body.validate[FormData]

      formResult.fold(
        error => {
          Logger.error(JsError.toFlatJson(error).toString())
          BadRequest(Json.obj("ok" -> false, "msg" -> JsError.toFlatJson(error)))
        },
        form => {
          Ticket.updateTicketFormData(ID, form)
          Ok(Json.obj("ok" -> true))
        })
  }

  def updateRepairForm(ID: Int) = Security.Authenticated(BodyParsers.parse.json) {
    import Ticket._
    implicit request =>
      val formResult = request.body.validate[RepairFormData]

      formResult.fold(
        error => {
          Logger.error(JsError.toFlatJson(error).toString())
          BadRequest(Json.obj("ok" -> false, "msg" -> JsError.toFlatJson(error)))
        },
        form => {
          Ticket.updateRepairFormData(ID, form)
          Ok(Json.obj("ok" -> true))
        })
  }
  def getForm(ID: Int) = Security.Authenticated {
    val ticketOpt = Ticket.getTicket(ID)
    if (ticketOpt.isEmpty)
      BadRequest(Json.obj("ok" -> false, "msg" -> "無此案件"))
    else {
      val t = ticketOpt.get
      t.ticketType match {
        case TicketType.maintance_week =>
          Ok(views.html.weekMaintanceForm(ID, t.getForm))
        case TicketType.maintance_biweek =>
          Ok(views.html.biweekForm(ID, t.getForm))
        case TicketType.maintance_month =>
          Ok(views.html.monthForm(ID, t.getForm))
        case TicketType.maintance_quarter =>
          Ok(views.html.quarterForm(ID, t.getForm))
        case TicketType.maintance_half_year =>
          Ok(views.html.halfYearForm(ID, t.getForm))
        case TicketType.maintance_year =>
          Ok(views.html.yearForm(ID, t.getForm))
        case TicketType.repair =>
          val equipList = Equipment.map(t.monitor)
          val partIdNameMap = Part.getIdNameMap()
          val partEquipMap = Part.getEquipPartMap()
          if (t.getRepairForm.equipmentId.length() != 0) {
            val equipment = Equipment.getEquipment(t.getRepairForm.equipmentId).get
            val partList = partEquipMap.getOrElse(equipment.name, List.empty[Part])
            Ok(views.html.repairForm(ID, t.getRepairForm, equipList, partIdNameMap, partList))
          } else {
            Ok(views.html.repairForm(ID, t.getRepairForm, equipList, partIdNameMap, List.empty[Part]))
          }
      }
    }
  }

  def closeTicket = Security.Authenticated {
    implicit request =>
      val userInfoOpt = Security.getUserinfo(request)
      if (userInfoOpt.isEmpty) {
        Forbidden("Invalid access!")
      } else {
        val userInfo = userInfoOpt.get
        val group = Group.getGroup(userInfo.groupID).get
        val tickets = Ticket.ticketSubmittedByMe(userInfo.id)
        val adminUsers = User.getAdminUsers()
        val usrMap = Map(adminUsers.map { u => (u.id.get -> u) }: _*)

        Ok(views.html.closeTickets(tickets, usrMap))
      }
  }

  def closeTicketAction(idStr: String) = Security.Authenticated {
    implicit request =>
      val myself = request.user.id
      val ids = idStr.split(":").toList
      val idList = ids.map { Integer.parseInt }
      Ticket.readyToCloseOwnerTicket(idList, myself, true)
      Ticket.closeTicket(idList, myself)
      Ok(Json.obj("ok" -> true))
  }

  def resumeTicketAction(idStr: String) = Security.Authenticated {
    implicit request =>
      val myself = request.user.id
      val ids = idStr.split(":").toList
      val idList = ids.map { Integer.parseInt }
      Ticket.readyToCloseSubmitterTicket(idList, myself, false)
      for (id <- idList) {
        val ticketOpt = Ticket.getTicket(id)
        ticketOpt.map {
          ticket =>
            if (ticket.ticketType == TicketType.repair) {
              import play.api.libs.mailer._
              import play.api.Play.current
              val userOpt = User.getUserById(ticket.owner_id)
              if (userOpt.isDefined) {
                val user = userOpt.get
                val ticketTypeStr = TicketType.map(ticket.ticketType)
                val mtType = ticket.monitorType.map(MonitorType.map(_).desp)
                val msg = s"駁回維修案件 $id : ${Monitor.map(ticket.monitor).name}-${ModelHelper.formatOptStr(mtType)}"
                val htmlMsg = s"<html><body><p><b>$msg</b></p></body></html>"

                val email = Email(
                  s"駁回維修案件: ${Monitor.map(ticket.monitor).name}-${ModelHelper.formatOptStr(mtType)}",
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
      Ok(Json.obj("ok" -> true))
  }

  def takeOverTicketAction(idStr: String) = Security.Authenticated {
    implicit request =>
      val myself = request.user.id
      val ids = idStr.split(":").toList
      val idList = ids.map { Integer.parseInt }
      Ticket.takeOverTicket(idList, myself)
      Ok(Json.obj("ok" -> true))
  }

  def downloadForm(Id: Int) = Security.Authenticated {
    val ticketOpt = Ticket.getTicket(Id)
    import java.io.File
    import java.nio.file.Files

    if (ticketOpt.isEmpty) {
      BadRequest("no such ticket!")
    } else {
      val ticket = ticketOpt.get
      val adminUsers = User.getAdminUsers()
      val usrMap = Map(adminUsers.map { u => (u.id.get -> u) }: _*)

      val title = TicketType.map(ticket.ticketType) + ticket.id
      val excelFile =
        ticket.ticketType match {
          case TicketType.maintance_week => {
            val previousTicketOpt = Ticket.getPreviousTicket(ticket)
            ExcelUtility.exportWeekForm(ticket, usrMap, previousTicketOpt)
          }
          case TicketType.maintance_biweek =>
            ExcelUtility.exportBiWeekForm(ticket, usrMap)
          case TicketType.maintance_month =>
            ExcelUtility.exportMonthForm(ticket, usrMap)
          case TicketType.maintance_quarter =>
            ExcelUtility.exportQuarterForm(ticket, usrMap)
          case TicketType.maintance_half_year =>
            ExcelUtility.exportHalfYearForm(ticket, usrMap)
          case TicketType.maintance_year =>
            ExcelUtility.exportYearForm(ticket, usrMap)
          case TicketType.repair =>
            ExcelUtility.exportRepairForm(ticket, usrMap)
        }

      Ok.sendFile(excelFile, fileName = _ =>
        play.utils.UriEncoding.encodePathSegment(title + ".xlsx", "UTF-8"),
        onClose = () => { Files.deleteIfExists(excelFile.toPath()) })
    }
  }

  def equipmentHistory = Security.Authenticated {
    implicit request =>
      val userInfo = Security.getUserinfo(request).get
      val group = Group.getGroup(userInfo.groupID).get
      val adminUsers = User.getAdminUsers()

      Ok(views.html.equipmentHistory(userInfo, group.privilege, adminUsers))
  }

  def equipmentHistoryReport(monitorStr: String, monitorTypeStr: String, startStr: String, endStr: String, outputTypeStr: String) = Security.Authenticated {
    val monitors = monitorStr.split(":").map { Monitor.withName }
    val monitorType = MonitorType.withName(monitorTypeStr)
    val start = DateTime.parse(startStr)
    val end = DateTime.parse(endStr) + 1.day
    val outputType = OutputType.withName(outputTypeStr)

    val tickets = Ticket.queryTickets(start, end)
    val filterTicket = tickets.filter { t =>
      monitors.contains(t.monitor) &&
        t.ticketType == TicketType.repair &&
        t.monitorType.isDefined &&
        t.monitorType.get == monitorType
    }
    val adminUsers = User.getAdminUsers()
    val usrMap = Map(adminUsers.map { u => (u.id.get -> u) }: _*)

    if (outputType == OutputType.html)
      Ok(views.html.equipmentHistoryReport(filterTicket, usrMap))
    else {
      val excelFile = ExcelUtility.equipmentHistoryReport(filterTicket, start, end)
      Ok.sendFile(excelFile, fileName = _ =>
        play.utils.UriEncoding.encodePathSegment("儀器保養履歷" + start.toString("YYMMdd") + "_" +
          end.toString("YYMMdd") + ".xlsx", "UTF-8"),
        onClose = () => { Files.deleteIfExists(excelFile.toPath()) })
    }
  }

  def monitorJournal = Security.Authenticated {
    implicit request =>
      val userInfo = Security.getUserinfo(request).get
      val group = Group.getGroup(userInfo.groupID).get

      Ok(views.html.monitorJournal(group.privilege))
  }

  def monitorJournalReport(monitorStr: String, dateStr: String, outputTypeStr: String) = Security.Authenticated {
    implicit request =>
      import java.sql.Time
      val monitor = Monitor.withName(monitorStr)
      val date = DateTime.parse(dateStr)

      val reportOpt = MonitorJournal.getReportFromDb(monitor, date)
      val report =
        if (reportOpt.isEmpty) {
          MonitorJournal.newReport(monitor, date)
          val enter_time: java.sql.Time = DateTime.parse("9:00", DateTimeFormat.forPattern("HH:mm"))
          val out_time: java.sql.Time = DateTime.parse("17:00", DateTimeFormat.forPattern("HH:mm"))

          MonitorJournal(date, monitor, "", "", "", None, enter_time, out_time)
        } else
          reportOpt.get

      val invalidHourList = MonitorJournal.getInvalidHourList(monitor, date)

      val outputType = OutputType.withName(outputTypeStr)

      val title = "測站工作日誌"
      val output = views.html.monitorJournalReport(report, invalidHourList, User.getAdminUsers())

      outputType match {
        case OutputType.html =>
          Ok(output)
        case OutputType.pdf =>
          Ok.sendFile(creatPdfWithReportHeader(title, output),
            fileName = _ =>
              play.utils.UriEncoding.encodePathSegment(title + date.toString("YYYYMMdd") + ".pdf", "UTF-8"))
        case OutputType.excel =>
          val excelFile = ExcelUtility.monitorJournalReport(report, invalidHourList, User.getAdminUsers())
          Ok.sendFile(excelFile, fileName = _ =>
            play.utils.UriEncoding.encodePathSegment(title + date.toString("YYMMdd") + ".xlsx", "UTF-8"),
            onClose = () => { Files.deleteIfExists(excelFile.toPath()) })
      }
  }

  def saveMonitorJournalReport(monitorStr: String, dateStr: String) = Security.Authenticated(BodyParsers.parse.json) {
    import MonitorJournal._
    implicit request =>
      val monitor = Monitor.withName(monitorStr)
      val date = DateTime.parse(dateStr)

      val reportJsonResult = request.body.validate[MonitorJournalJson]

      reportJsonResult.fold(
        error => {
          Logger.error(JsError.toFlatJson(error).toString())
          BadRequest(Json.obj("ok" -> false, "msg" -> JsError.toFlatJson(error)))
        },
        report => {
          MonitorJournal.updateReport(report)
          Ok(Json.obj("ok" -> true))
        });
  }

  def equipmentManagement = Security.Authenticated {
    val m = Monitor.values.toList.head
    Ok(views.html.monitor(m, true))
  }

  def partManagement = Security.Authenticated {
    val parts = Part.getList
    Ok(views.html.partManagement(parts))
  }

  import play.api.data._
  import play.api.data.Forms._
  import Application.EditData

  def updatePart() = Security.Authenticated {
    implicit request =>
      try {
        val mtForm = Form(
          mapping(
            "id" -> text,
            "data" -> text)(EditData.apply)(EditData.unapply))

        val mtData = mtForm.bindFromRequest.get
        val ids = mtData.id.split(":")

        Part.update(ids(0), ids(1), mtData.data)

        Ok(mtData.data)
      } catch {
        case e: Exception =>
          Logger.error(e.toString)
          BadRequest(e.toString)
        case e: Throwable =>
          Logger.error(e.toString)
          BadRequest(e.toString)
      }
  }

  def newPart = Security.Authenticated(BodyParsers.parse.json) {
    implicit request =>
      val newPartResult = request.body.validate[Part]

      newPartResult.fold(
        error => {
          Logger.error(JsError.toFlatJson(error).toString())
          BadRequest(Json.obj("ok" -> false, "msg" -> JsError.toFlatJson(error)))
        },
        param => {
          try {
            Part.create(param)
          } catch {
            case e: Exception =>
              Logger.error(e.toString())
              BadRequest(Json.obj("ok" -> false, "msg" -> e.toString()))
          }

          Ok(Json.obj("ok" -> true))
        })
  }

  def deletePart(id: String) = Security.Authenticated {
    Part.delete(id)
    Ok(Json.obj("ok" -> true))
  }

  def testAlarmMail = Security.Authenticated {
    implicit request =>
      val userInfoOpt = Security.getUserinfo(request)
      val userInfo = userInfoOpt.get
      val user = User.getUserById(userInfo.id).get

      val email = Email(
        "測試警告信",
        s"麥寮AQMS <aqm6812646@gmail.com>",
        Seq(s"${user.name} <${user.email}>"),
        // adds attachment
        attachments = Seq(),
        // sends text, HTML or both...
        bodyText = Some("測試信"),
        bodyHtml = Some("<html><body><p>測試信</p></body></html>"))

      try {
        MailerPlugin.send(email)
        EventLog.create(EventLog(DateTime.now, EventLog.evtTypeInformAlarm, "送測試信!"))
        Ok(s"已經送信到${user.email}")
      } catch {
        case ex: Exception =>
          Console.print(ex.getCause)
          EventLog.create(EventLog(DateTime.now, EventLog.evtTypeInformAlarm, "送測試信失敗!"))
          Ok(s"無法送信到${user.email}: ${ex.getCause}")
      }

  }

  def dutySchedule = Security.Authenticated {
    Ok(views.html.dutySchedule(""))
  }

  def eventLog = Security.Authenticated {
    Ok(views.html.eventLog())
  }

  def eventLogReport(startStr: String, endStr: String) = Security.Authenticated {
    val start = DateTime.parse(startStr)
    val end = DateTime.parse(endStr) + 1.day
    val logs = EventLog.getList(start, end)
    Ok(views.html.eventLogReport(logs))
  }

  def downloadNotification = Security.Authenticated {
    Ok(views.html.downloadNotification())
  }

  def downloadNotificationForm(startStr: String, endStr: String) = Security.Authenticated {

    val start = DateTime.parse(startStr)
    val end = DateTime.parse(endStr) + 1.day

    val tickets = Ticket.queryActiveTickets(start, end)

    if (tickets.length != 0) {
      val excelFile = ExcelUtility.epbNotification(tickets)
      val title = s"環保局通報單${start.toString("MMdd")}_${end.toString("MMdd")}"

      Ok.sendFile(excelFile, fileName = _ =>
        play.utils.UriEncoding.encodePathSegment(title + ".xlsx", "UTF-8"),
        onClose = () => { Files.deleteIfExists(excelFile.toPath()) })

    } else {
      val output = views.html.notificationPage(tickets)
      val title = s"查詢區間無定保案件"
      Ok.sendFile(creatPdfWithReportHeaderP(title, output),
        fileName = _ =>
          play.utils.UriEncoding.encodePathSegment(title + ".pdf", "UTF-8"))
    }

  }

  case class MaintanceWeekEntry(start: DateTime, end: DateTime, offset: Int, map: Map[Monitor.Value, Map[TicketType.Value, List[Ticket]]])
  def maintanceSchedule = Security.Authenticated {
    implicit request =>
      val userInfoOpt = Security.getUserinfo(request)
      val userInfo = userInfoOpt.get

      val today = DateTime.now.toLocalDate().toDateTimeAtStartOfDay()
      val first_day = today - today.getDayOfWeek.day

      def weekTicketMap(offset: Int) = {
        val begin = first_day + offset.week
        val tickets = queryAllMaintanceTickets(begin, begin + 1.week)

        val mPairs =
          for {
            monitor <- Monitor.mvList
            pairs = tickets.map(t => t.monitor -> t)
            monitorTickets = tickets.filter { t => t.monitor == monitor }
            ttPair = TicketType.values.toList.map { tt => tt -> monitorTickets.filter { t => t.ticketType == tt } }
            ttMap = Map(ttPair: _*)
          } yield monitor -> ttMap

        Map(mPairs: _*)
      }

      val weekEntries =
        for (offset <- -4 to 4)
          yield MaintanceWeekEntry(first_day + offset.week, first_day + offset.week + 6.day, offset, weekTicketMap(offset))

      Ok(views.html.maintanceSchedule(weekEntries.toList))
  }

  def getMaintanceStr(map: Map[TicketType.Value, List[Ticket]]) = {
    val maintanceType = {
      import TicketType._
      List(maintance_week, maintance_biweek, maintance_month, maintance_quarter, maintance_half_year, maintance_year)
    }
    if (map.values.map { _.length }.sum == 0) {
      ""
    } else {
      val today = DateTime.now.toLocalDate().toDateTimeAtStartOfDay()

      import scala.collection.mutable.ListBuffer
      val buff = ListBuffer.empty[String]
      for (tt <- maintanceType) {
        if (map(tt).length != 0) {
          val ticketDates = map(tt).map { t =>
            val dateStr = t.executeDate.toString("MM/dd")
            if (t.executeDate < today && t.active)
              s"""
               <a href="#" onClick="loadPage('/Ticket/${t.id}','維修保養','案件細節')"><font color="red">${dateStr}</font></a>                              
               """
            else
              s"""
               <a href="#" onClick="loadPage('/Ticket/${t.id}','維修保養','案件細節')">${dateStr}</a>                              
               """
          }
          val ttMap =
            {
              import TicketType._
              Map(repair -> "維修", maintance_week -> "單週", maintance_biweek -> "雙週",
                maintance_month -> "月", maintance_quarter -> "季", maintance_half_year -> "半年", maintance_year -> "年")
            }

          buff.append(s"<strong>${ttMap(tt)}</strong>:${ticketDates.mkString(",")}")
        }
      }

      buff.mkString("<br/>")

    }
  }

  def alarmNoTicketList() = Security.Authenticated {
    implicit request =>
      val start = SystemConfig.getAlarmCheckPoint()
      val list = Alarm.getAlarmNoTicketList(start)

      Ok(views.html.alarmNoTicket(list))
  }

  case class AlarmId(monitor: Monitor.Value, mItem: String, time: Long)
  case class AlarmToTicketParam(status: String, alarmToTicketList: Seq[AlarmId])
  def alarmToTicket() = Security.Authenticated(BodyParsers.parse.json) {
    implicit request =>
      val user = request.user
      implicit val idReads = Json.reads[AlarmId]
      implicit val paramReads = Json.reads[AlarmToTicketParam]
      val alarmToTicketParam = request.body.validate[AlarmToTicketParam]
      alarmToTicketParam.fold(
        error => {
          Logger.error(JsError.toFlatJson(error).toString())
          BadRequest(Json.obj("ok" -> false, "msg" -> JsError.toFlatJson(error)))
        },
        param => {
          val defaultTicketOwner = SystemConfig.getAlarmTicketDefaultUserId()
          for (alarmId <- param.alarmToTicketList) {
            Alarm.updateAlarmTicketState(alarmId.monitor, alarmId.mItem, new DateTime(alarmId.time), param.status)
            val arOpt = Alarm.getAlarmOpt(alarmId.monitor, alarmId.mItem, new DateTime(alarmId.time))
            if (arOpt.isDefined) {
              val ar = arOpt.get
              val ar_state =
                if (ar.mVal == 0)
                  "恢復正常"
                else
                  "觸發"

              val reason = s"${ar.time.toString("YYYY/MM/dd HH:mm")} ${Monitor.map(ar.monitor).name}:${Alarm.map(alarmId.mItem)}-${MonitorStatus.map(ar.code).desp}:${ar_state}"
              val mtOpt = try {
                Some(MonitorType.withName(ar.mItem))
              } catch {
                case ex: Throwable =>
                  None
              }
              val (repairType, repairSubType) = if (mtOpt.isDefined) {
                (Some("數據"), Some(MonitorType.map(mtOpt.get).desp))
              } else
                (None, None)
  
              val ticket = Ticket(0, DateTime.now, true, TicketType.repair, user.id,
                SystemConfig.getAlarmTicketDefaultUserId(), alarmId.monitor, mtOpt, reason,
                DateTime.now, Json.toJson(Ticket.defaultAlarmTicketForm(ar)).toString, repairType, repairSubType, Some(false))
              Ticket.newTicket(ticket)
            }
          }

          Ok(Json.obj("ok" -> true, "count" -> param.alarmToTicketList.length))
        })

  }
  def manualAlarmTicket() = Security.Authenticated {
    implicit request =>
      val userInfo = Security.getUserinfo(request).get
      val group = Group.getGroup(userInfo.groupID).get
      val adminUsers = User.getAdminUsers()

      Ok(views.html.newTicket(userInfo, group.privilege, adminUsers, List(TicketType.repair), false))
  }

  def repairingAlarmTicket() = Security.Authenticated {
    implicit request =>
      val userInfo = request.user
      val group = Group.getGroup(userInfo.groupID).get
      val tickets = queryActiveRepairTickets

      val ticketWithAlarm =
        for {
          t <- tickets
          arOpt = t.getRepairForm.alarm
        } yield (t, arOpt)
      val allUsers = User.getAllUsers()
      val usrMap = Map(allUsers.map { u => (u.id.get -> u) }: _*)
      val canChangeOwner = userInfo.groupID == 5
      
      Ok(views.html.repairingTickets(ticketWithAlarm, usrMap, canChangeOwner))
  }

  def queryClosedRepairTicket = Security.Authenticated {
    implicit request =>
      val userInfo = Security.getUserinfo(request).get
      val group = Group.getGroup(userInfo.groupID).get
      val adminUsers = User.getAdminUsers()

      Ok(views.html.queryClosedRepairTicket(userInfo, group.privilege, adminUsers, List(TicketType.repair)))
  }

  def closedRepairTicketReport(monitorStr: String, startStr: String, endStr: String) = Security.Authenticated {
    val monitors = monitorStr.split(":").map { Monitor.withName }
    val start = DateTime.parse(startStr)
    val end = DateTime.parse(endStr) + 1.day

    val tickets = Ticket.queryClosedRepairTickets(start, end)
    val filterTicket = tickets.filter { t => monitors.contains(t.monitor) }
    val ticketWithRepairForm =
        for {
          t <- filterTicket
        } yield (t, t.getRepairForm)

    val allUsers = User.getAllUsers()
    val usrMap = Map(allUsers.map { u => (u.id.get -> u) }: _*)

    Ok(views.html.closedAlarmTicketReport(ticketWithRepairForm, usrMap))
  }

}