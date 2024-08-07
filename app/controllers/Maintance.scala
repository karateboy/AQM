package controllers

import com.github.nscala_time.time.Imports._
import controllers.Application.EditData
import controllers.PdfUtility._
import models.Alarm.getOverStdLevel
import models.Ticket._
import models._
import play.api.Logger
import play.api.Play.current
import play.api.data.Forms._
import play.api.data._
import play.api.libs.json._
import play.api.libs.mailer._
import play.api.mvc._

import java.nio.file.Files
import scala.concurrent.ExecutionContext.Implicits.global

object PartReplaceFilter extends Enumeration {
  val replaced = Value
  val noReplaced = Value
  val all = Value
  val map = Map(replaced -> "更換零件", noReplaced -> "未更換零件", all -> "全部")
}

/**
 * @author user
 */

object Maintance extends Controller {
  implicit val r1 = Json.reads[Part2]
  implicit val read = Json.reads[NewPartParam]

  def newTicket = Security.Authenticated {
    implicit request =>
      val userInfo = Security.getUserinfo(request).get
      val group = Group.getGroup(userInfo.groupID).get
      val adminUsers = User.getAdminUsers()
      val ticketTypes = TicketType.values.toList.sorted
      Ok(views.html.newTicket(userInfo, group.privilege, adminUsers, ticketTypes))
  }

  def newTicketAction(idStr: String) = Security.Authenticated(BodyParsers.parse.json) {
    implicit request =>
      val submiterId = Integer.parseInt(idStr)
      val newTicketJson = request.body.validate[TicketParam]

      newTicketJson.fold(
        error => {
          Logger.error(JsError.toJson(error).toString())
          BadRequest(Json.obj("ok" -> false, "msg" -> JsError.toJson(error)))
        },
        ticketParam => {
          val tickets =
            if (ticketParam.ticketType != TicketType.repair) {
              for {
                m <- ticketParam.monitors
                date <- ticketParam.executeDate
                executeDate = DateTime.parse(date)
              } yield {
                Ticket(0, DateTime.now, true, ticketParam.ticketType, submiterId,
                  ticketParam.owner, m, None, ticketParam.reason, executeDate, Json.toJson(Ticket.defaultFormData).toString,
                  None, None, Some(false))
              }
            } else {
              for {
                m <- ticketParam.monitors
                mt <- ticketParam.monitorTypes
                date <- ticketParam.executeDate
                executeDate = DateTime.parse(date)
              } yield {
                Ticket(0, DateTime.now, true, ticketParam.ticketType, submiterId,
                  ticketParam.owner, m, Some(mt), ticketParam.reason, executeDate, Json.toJson(Ticket.defaultRepairFormData).toString,
                  ticketParam.repairType, ticketParam.repairSubType, Some(false))
              }
            }
          for (t <- tickets)
            Ticket.newTicket(t)

          Ok(Json.obj("ok" -> true, "nNewCase" -> tickets.length))
        })
  }

  implicit val newTicketParamRead = Json.reads[TicketParam]

  def queryTicket = Security.Authenticated {
    implicit request =>
      val userInfo = Security.getUserinfo(request).get
      val group = Group.getGroup(userInfo.groupID).get
      val adminUsers = User.getAdminUsers()
      val ticketTypeList = TicketType.values.toList.sorted

      Ok(views.html.queryTicket(userInfo, group.privilege, adminUsers, ticketTypeList))
  }

  def ticketReport(ticketTypeStr: String, monitorStr: String, startStr: String, endStr: String) = Security.Authenticated {
    val ticketTypes = ticketTypeStr.split(":").map {
      TicketType.withName
    }
    val monitors = monitorStr.split(":").map {
      Monitor.withName
    }
    val start = DateTime.parse(startStr)
    val end = DateTime.parse(endStr) + 1.day

    val tickets = Ticket.queryTickets(start, end)
    val filterTicket = tickets.filter { t => ticketTypes.contains(t.ticketType) && monitors.contains(t.monitor) }
    val allUsers = User.getAllUsers()
    val usrMap = Map(allUsers.map { u => (u.id.get -> u) }: _*)

    Ok(views.html.ticketReport(filterTicket, usrMap))
  }

  def ticketReportOverStd(ticketTypeStr: String, monitorStr: String, startStr: String, endStr: String, overStdStr: String) = Security.Authenticated {
    val ticketTypes = ticketTypeStr.split(":").map {
      TicketType.withName
    }
    val monitors = monitorStr.split(":").map {
      Monitor.withName
    }
    val start = DateTime.parse(startStr)
    val end = DateTime.parse(endStr) + 1.day
    val overStdCode = overStdStr.split(":").map {
      AlarmLevel.withName
    }.map(AlarmLevel.map(_).code)

    val tickets = Ticket.queryOverStdTickets(start, end, overStdCode)
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
        val extendReasons = SystemConfig.getExtendedReasons
        Ok(views.html.ticket(ticketOpt.get, group.privilege, adminUsers, userInfo.id, extendReasons))
      }
  }

  def extendTicket(ID: Int) = Security.Authenticated(BodyParsers.parse.json) {
    implicit request =>
      implicit val reads = Json.reads[ExtendTickeParam]
      val ret = request.body.validate[ExtendTickeParam]
      ret.fold(
        error => {
          Logger.error(JsError.toJson(error).toString())
          BadRequest(Json.obj("ok" -> false, "msg" -> JsError.toJson(error)))
        },
        param => {
          val extendDate = DateTime.parse(param.extendDate(0))

          Ticket.extendTicket(ID, extendDate, param.extendReason)
          val extendedReasons = SystemConfig.getExtendedReasons
          if (!extendedReasons.contains(param.extendReason)) {
            val update = extendedReasons.+:(param.extendReason)
            SystemConfig.setExtededReasons(update)
          }
          Ok(Json.obj("ok" -> true))
        })
  }

  def updateTicket(ID: Int) = Security.Authenticated(BodyParsers.parse.json) {
    implicit request =>
      val ticketResult = request.body.validate[TicketParam]

      ticketResult.fold(
        error => {
          Logger.error(JsError.toJson(error).toString())
          BadRequest(Json.obj("ok" -> false, "msg" -> JsError.toJson(error)))
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

        val filename = picture.filename
        val contentType = picture.contentType
        Ticket.attachPhoto1(id, picture.ref.file)
      }

      request.body.file("photo2").map { picture =>

        val filename = picture.filename
        val contentType = picture.contentType
        Ticket.attachPhoto2(id, picture.ref.file)
      }

      Ok(Json.obj("ok" -> true))
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

  def getNoPhotoByteArray = {
    import java.io.FileInputStream
    val is = new FileInputStream(current.path.getAbsolutePath + "/public/images/no_photo.png")
    import org.apache.commons.io._
    val ba = IOUtils.toByteArray(is)
    is.close()
    ba
  }

  def updateForm(ID: Int) = Security.Authenticated(BodyParsers.parse.json) {
    import Ticket._
    implicit request =>
      val formResult = request.body.validate[FormData]

      formResult.fold(
        error => {
          Logger.error(JsError.toJson(error).toString())
          BadRequest(Json.obj("ok" -> false, "msg" -> JsError.toJson(error)))
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
          Logger.error(JsError.toJson(error).toString())
          BadRequest(Json.obj("ok" -> false, "msg" -> JsError.toJson(error)))
        },
        form => {
          val ticket = Ticket.getTicket(ID).get
          val oldRepairForm = ticket.getRepairForm
          val newForm = form.replaceAlarm(oldRepairForm.alarm)
          Ticket.updateRepairFormData(ID, newForm)
          Ok(Json.obj("ok" -> true))
        })
  }

  def getForm(ID: Int) = Security.Authenticated {
    implicit request =>
      val userInfoOpt = Security.getUserinfo(request)
      if (userInfoOpt.isEmpty) {
        Forbidden("Invalid access!")
      } else {
        val userInfo = userInfoOpt.get

        val ticketOpt = Ticket.getTicket(ID)
        if (ticketOpt.isEmpty)
          BadRequest(Json.obj("ok" -> false, "msg" -> "無此案件"))
        else {
          val t = ticketOpt.get
          t.ticketType match {
            case TicketType.maintance_week =>
              val newFormDate = new DateTime(2023, 2, 6, 0, 0)
              if(t.executeDate < newFormDate)
                Ok(views.html.weekMaintanceForm(ID, t.getForm))
              else
                Ok(views.html.weekMaintanceForm2(ID, t.getForm))
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
              val submittedByMe = t.submiter_id == userInfo.id
              val equipList = Equipment.map.getOrElse(t.monitor, Map.empty[String, Equipment]).values.toList
              val partIdNameMap = Part.getIdNameMap()
              val partEquipModelMap = Part.getEquipModelPartMap()
              if (t.getRepairForm.equipmentId.length() != 0) {
                val equipment = Equipment.getEquipment(t.getRepairForm.equipmentId).get
                val partList = partEquipModelMap.getOrElse(equipment.model, List.empty[Part2])
                Ok(views.html.repairForm(ID, t.getRepairForm, equipList, partIdNameMap, partList, submittedByMe))
              } else {
                Ok(views.html.repairForm(ID, t.getRepairForm, equipList, partIdNameMap, List.empty[Part2], submittedByMe))
              }
          }
        }
      }
  }

  def closeTicket(outputTypeStr: String) = Security.Authenticated {
    implicit request =>
      val outputType = OutputType.withName(outputTypeStr)
      val userInfoOpt = Security.getUserinfo(request)
      if (userInfoOpt.isEmpty) {
        Forbidden("Invalid access!")
      } else {
        val userInfo = userInfoOpt.get
        val group = Group.getGroup(userInfo.groupID).get
        val tickets = Ticket.ticketSubmittedByMe(userInfo.id)
        //val ticketsOwnedByMe =
        val adminUsers = User.getAllUsers()
        val usrMap = Map(adminUsers.map { u => (u.id.get -> u) }: _*)

        outputType match {
          case OutputType.html =>
            Ok(views.html.closeTickets(tickets, usrMap))
          case OutputType.excel =>
            val excelFile = ExcelUtility.tickets(tickets, "等待簽結案件", usrMap)
            Ok.sendFile(excelFile, fileName = _ =>
              play.utils.UriEncoding.encodePathSegment("等待簽結案件" + ".xlsx", "UTF-8"),
              onClose = () => {
                Files.deleteIfExists(excelFile.toPath())
              })
        }
      }
  }

  def closeTicketJson = Security.Authenticated {
    implicit request =>
      val userInfoOpt = Security.getUserinfo(request)
      if (userInfoOpt.isEmpty) {
        Forbidden("Invalid access!")
      } else {
        val userInfo = userInfoOpt.get
        val group = Group.getGroup(userInfo.groupID).get
        val tickets = Ticket.ticketSubmittedByMe(userInfo.id)
        //val ticketsOwnedByMe =
        val users = User.getAllUsers()
        val usrMap = Map(users.map { u => (u.id.get -> u) }: _*)

        implicit val write = Json.writes[TicketInfo]
        val infoList = tickets map {
          t =>
            val monitorType = if (t.monitorType.isDefined)
              MonitorType.map(t.monitorType.get).desp
            else
              "-"

            val active = if (t.active) {
              if (t.rejectReason.isDefined) {
                s"駁回(${t.rejectReason.get})"
              } else {
                "否"
              }
            } else {
              "是"
            }
            TicketInfo(id = t.id.toString,
              monitor = Monitor.map(t.monitor).name,
              monitorType = monitorType,
              submitDate = t.submit_date.toString("MM-d HH:mm"),
              submitter = usrMap(t.submiter_id).name,
              reason = t.reason,
              owner = usrMap(t.owner_id).name,
              executeDate = t.executeDate.toString("MM-d"),
              ticketType = TicketType.map(t.ticketType),
              readyToClose = ModelHelper.formatOptBool(t.readyToClose),
              active = active,
              repairType = ModelHelper.formatOptStr(t.repairType),
              repairSubType = ModelHelper.formatOptStr(t.repairSubType))
        }
        Ok(Json.toJson(infoList))
      }
  }

  def closeTicketAction(idStr: String) = Security.Authenticated {
    implicit request =>
      val myself = request.user.id
      val ids = idStr.split(":").toList
      val idList = ids.map {
        Integer.parseInt
      }
      Ticket.readyToCloseOwnerTicket(idList, myself, true)
      Ticket.closeTicket(idList, myself)
      Ok(Json.obj("ok" -> true))
  }

  def resumeTicketAction(idStr: String, reason: String) = Security.Authenticated {
    implicit request =>
      val myself = request.user.id
      val ids = idStr.split(":").toList
      val idList = ids.map {
        Integer.parseInt
      }
      Ticket.resumeTicket(idList, myself, reason)
      for (id <- idList) {
        val ticketOpt = Ticket.getTicket(id)
        ticketOpt.map {
          ticket =>
            if (ticket.ticketType == TicketType.repair) {
              import play.api.Play.current
              import play.api.libs.mailer._
              val userOpt = User.getUserById(ticket.owner_id)
              if (userOpt.isDefined) {
                val user = userOpt.get
                val ticketTypeStr = TicketType.map(ticket.ticketType)
                val mtType = ticket.monitorType.map(MonitorType.map(_).desp)
                val msg = s"駁回維修案件 $id : ${Monitor.map(ticket.monitor).name}-${ModelHelper.formatOptStr(mtType)}"
                val htmlMsg = s"<html><body><p><b>$msg 理由: $reason</b></p></body></html>"

                val email = Email(
                  s"駁回維修案件: ${Monitor.map(ticket.monitor).name}-${ModelHelper.formatOptStr(mtType)}",
                  "案件通報 <karateboy.huang@gmail.com>",
                  List(user.email),
                  // adds attachment
                  attachments = Seq(),
                  // sends text, HTML or both...
                  bodyText = Some(msg),
                  bodyHtml = Some(htmlMsg))

                try {
                  SmsSender.send(List(user), msg)
                } catch {
                  case ex: Throwable =>
                    Logger.error("fail to send notification", ex)
                }
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
      val idList = ids.map {
        Integer.parseInt
      }
      Ticket.takeOverTicket(idList, myself)
      Ok(Json.obj("ok" -> true))
  }

  def downloadForm(Id: Int) = Security.Authenticated {
    val ticketOpt = Ticket.getTicket(Id)
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
        onClose = () => {
          Files.deleteIfExists(excelFile.toPath())
        })
    }
  }

  def equipmentHistory = Security.Authenticated {
    implicit request =>
      val userInfo = Security.getUserinfo(request).get
      val group = Group.getGroup(userInfo.groupID).get
      val adminUsers = User.getAdminUsers()

      Ok(views.html.equipmentHistory(userInfo, group.privilege, adminUsers))
  }

  def equipmentHistoryReport(monitorStr: String, equipmentNameStr: String, partFilterStr: String, startStr: String, endStr: String, outputTypeStr: String) = Security.Authenticated {
    val monitor = Monitor.withName(monitorStr)
    val equipmentName = java.net.URLDecoder.decode(equipmentNameStr, "UTF-8")
    val partFilter = PartReplaceFilter.withName(partFilterStr)
    val start = DateTime.parse(startStr)
    val end = DateTime.parse(endStr) + 1.day
    val outputType = OutputType.withName(outputTypeStr)
    val tickets = Ticket.queryMonitorTickets(monitor, start, end)
    val filterTicket = tickets.filter { t =>
      t.ticketType == TicketType.repair &&
        t.monitorType.isDefined && {
        val equipmentOpt = Equipment.map(monitor).get(equipmentName)
        if (equipmentOpt.isEmpty) {
          false
        } else {
          val equipmentId = t.getRepairForm.equipmentId
          equipmentId == equipmentOpt.get.id
        }
      } && {
        partFilter match {
          case PartReplaceFilter.all =>
            true
          case PartReplaceFilter.noReplaced =>
            t.getRepairForm.parts.length == 0
          case PartReplaceFilter.replaced =>
            t.getRepairForm.parts.length != 0
        }
      }
    }
    val adminUsers = User.getAdminUsers()
    val usrMap = Map(adminUsers.map { u => (u.id.get -> u) }: _*)

    if (outputType == OutputType.html)
      Ok(views.html.equipmentHistoryReport(filterTicket, usrMap))
    else {
      val equipmentOpt = Equipment.map(monitor).get(equipmentName)
      if (equipmentOpt.isDefined) {
        val excelFile = ExcelUtility.equipmentHistoryReport(equipmentOpt.get, filterTicket, start, end)
        Ok.sendFile(excelFile, fileName = _ =>
          play.utils.UriEncoding.encodePathSegment("儀器保養履歷" + start.toString("YYMMdd") + "_" +
            end.toString("YYMMdd") + ".xlsx", "UTF-8"),
          onClose = () => {
            Files.deleteIfExists(excelFile.toPath())
          })
      } else
        Ok("儀器不存在!")
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

      val monitor = Monitor.withName(monitorStr)
      val date = DateTime.parse(dateStr)

      val report = MonitorJournal.getReportFromDb(monitor, date)

      val abnormalEntries = AbnormalReport.getLatestExplain(date)

      val monitorAbnormalEntries = abnormalEntries.filter { entry => entry.monitor == monitor }

      //val invalidHourList = MonitorJournal.getInvalidHourList(monitor, date)

      val outputType = OutputType.withName(outputTypeStr)

      val title = "測站工作日誌"
      val output = views.html.monitorJournalReport(report, monitorAbnormalEntries, User.getAdminUsers())

      outputType match {
        case OutputType.html =>
          Ok(output)
        case OutputType.pdf =>
          Ok.sendFile(creatPdfWithReportHeader(title, output),
            fileName = _ =>
              play.utils.UriEncoding.encodePathSegment(title + date.toString("YYYYMMdd") + ".pdf", "UTF-8"))
        case OutputType.excel =>
          val excelFile = ExcelUtility.monitorJournalReport(report, monitorAbnormalEntries, User.getAdminUsers())
          Ok.sendFile(excelFile, fileName = _ =>
            play.utils.UriEncoding.encodePathSegment(title + date.toString("YYMMdd") + ".xlsx", "UTF-8"),
            onClose = () => {
              Files.deleteIfExists(excelFile.toPath())
            })
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
          Logger.error(JsError.toJson(error).toString())
          BadRequest(Json.obj("ok" -> false, "msg" -> JsError.toJson(error)))
        },
        report => {
          MonitorJournal.updateReport(report)
          //Update monitor abnormal report
          val latestExplain = AbnormalReport.getLatestExplain(date)
          if (report.entries.isDefined) {
            val abnormalEntries = report.entries.get
            val updatedExplain = latestExplain map {
              entry =>
                val matchedOpt = abnormalEntries.find { updatedEntry =>
                  updatedEntry.monitor == entry.monitor &&
                    updatedEntry.monitorType == entry.monitorType
                }

                matchedOpt.getOrElse(entry)
            }
            AbnormalReport.updateReport(date, updatedExplain)
          }

          Ok(Json.obj("ok" -> true))
        });
  }

  def equipmentManagement = Security.Authenticated {
    val m = Monitor.values.toList.head
    Ok(views.html.monitorConfig(m, true))
  }

  def partManagement(outputTypeStr: String) = Security.Authenticated {
    val outputType = OutputType.withName(outputTypeStr)
    val parts = Part.getList
    outputType match {
      case OutputType.html =>
        Ok(views.html.partManagement(parts))
      case OutputType.excel =>
        val excelFile = ExcelUtility.parts(parts, "物料類別")
        Ok.sendFile(excelFile, fileName = _ =>
          play.utils.UriEncoding.encodePathSegment("物料類別" + ".xlsx", "UTF-8"),
          onClose = () => {
            Files.deleteIfExists(excelFile.toPath())
          })
    }
  }

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

  def getPartList = Security.Authenticated {
    import Part._
    Ok(Json.toJson(Part.getList))
  }

  def newPart = Security.Authenticated(BodyParsers.parse.json) {
    implicit request =>
      val newPartResult = request.body.validate[NewPartParam]

      newPartResult.fold(
        error => {
          Logger.error(JsError.toJson(error).toString())
          BadRequest(Json.obj("ok" -> false, "msg" -> JsError.toJson(error)))
        },
        param => {
          try {
            Part.create(param.part)
            val startDate = DateTime.parse(param.startDate)
            for {
              monitor <- param.monitors
              monitorModelList = Equipment.map.getOrElse(monitor, Map.empty[String, Equipment]).values.toList.map { e => e.model }
              model <- param.part.models.split(",") if monitorModelList.contains(model)
            } {
              val id = s"${param.part.id}_${model}_${monitor}"
              val partUsage = PartUsage(id, param.usage, monitor.toString, model, startDate,
                param.freqType, param.days, param.alarm)

              PartUsage.create(partUsage)

              Alarm.updateMap(Alarm.AlarmItem(partUsage.getAlarmCode, s"${model}更換${param.part.name} (${param.part.chineseName} ${param.part.id})"))
            }

            Ok(Json.obj("ok" -> true))
          } catch {
            case e: Exception =>
              Logger.error(e.toString())
              BadRequest(Json.obj("ok" -> false, "msg" -> e.toString()))
          }
        })
  }

  def deletePart(id: String) = Security.Authenticated {
    Part.delete(id)
    PartUsage.deletePart(id)
    Alarm.removePartFromMap(id)
    Ok(Json.obj("ok" -> true))
  }

  def partUsage(outputTypeStr: String) = Security.Authenticated {
    val outputType = OutputType.withName(outputTypeStr)
    outputType match {
      case OutputType.html =>
        Ok(views.html.partUsage())
      case OutputType.excel =>
        val excelFile = ExcelUtility.partUsage(PartUsage.getList(), Part.getIdMap(), "物料使用紀錄表")
        Ok.sendFile(excelFile, fileName = _ =>
          play.utils.UriEncoding.encodePathSegment("物料使用紀錄表.xlsx", "UTF-8"),
          onClose = () => {
            Files.deleteIfExists(excelFile.toPath())
          })
    }
  }

  def partUsageJson() = Security.Authenticated {
    val partUsageList = PartUsage.getList()
    implicit val write = Json.writes[PartUsageInfo]
    val partMap = Part.getIdMap()
    val partUsageInfoList = partUsageList map {
      p =>
        PartUsageInfo(p.id, partMap(p.getPartId).name, partMap(p.getPartId).chineseName,
          Monitor.map(Monitor.withName(p.monitor)).name,
          p.usage,
          p.getFreqDesc,
          p.startDate.toString("YYYY-MM-dd"),
          p.getNextReplaceDate.toString("YYYY-MM-dd"),
          ModelHelper.formatOptBool(Some(p.alarm)),
          ModelHelper.formatOptStr(p.nochange_reason),
          ModelHelper.formatOptStr(p.remark))
    }
    Ok(Json.toJson(partUsageInfoList))
  }

  def deletePartUsage(id: String) = Security.Authenticated {
    PartUsage.delete(id)
    Ok(Json.obj("ok" -> true))
  }

  def newPartUsage = Security.Authenticated(BodyParsers.parse.json) {
    implicit request =>
      implicit val read = Json.reads[NewPartUsageParam]
      val newPartResult = request.body.validate[NewPartUsageParam]

      newPartResult.fold(
        error => {
          Logger.error(JsError.toJson(error).toString())
          BadRequest(Json.obj("ok" -> false, "msg" -> JsError.toJson(error)))
        },
        param => {
          try {
            val partOpt = Part.getPart(param.partClassId)
            val startDate = DateTime.parse(param.startDate)
            for {
              part <- partOpt
              monitor <- param.monitors
              monitorModelList = Equipment.map.getOrElse(monitor, Map.empty[String, Equipment]).values.toList.map { e => e.model }
              model <- part.models.split(",") if monitorModelList.contains(model)
            } {
              val id = s"${part.id}_${model}_${monitor}"
              val partUsage = PartUsage(id, param.usage, monitor.toString, model, startDate,
                param.freqType, param.days, param.alarm)

              PartUsage.create(partUsage)
              Alarm.updateMap(Alarm.AlarmItem(partUsage.getAlarmCode, s"${model}更換(${part.id}) (${part.name}/${part.chineseName})"))
            }
            Ok(Json.obj("ok" -> true))
          } catch {
            case e: Exception =>
              Logger.error(e.toString())
              BadRequest(Json.obj("ok" -> false, "msg" -> e.toString()))
          }
        })
  }

  def updatePartUsage = Security.Authenticated(BodyParsers.parse.json) {
    implicit request =>
      implicit val read = Json.reads[UpdatePartUsageParam]
      val newPartResult = request.body.validate[UpdatePartUsageParam]

      newPartResult.fold(
        error => {
          Logger.error(JsError.toJson(error).toString())
          BadRequest(Json.obj("ok" -> false, "msg" -> JsError.toJson(error)))
        },
        param => {
          try {
            val partUsageOpt = PartUsage.getPartUsage(param.id)
            if (partUsageOpt.isDefined) {
              val partUsage = partUsageOpt.get
              if (partUsage.usage != param.usage) {
                PartUsage.update(param.id, "usage", param.usage.toString)
              }
              if (partUsage.freqType != param.freqType) {
                PartUsage.update(param.id, "freqType", param.freqType)
              }
              if (partUsage.freqDays != param.days) {
                PartUsage.update(param.id, "freq_days", param.days.toString())
              }
            }
            Ok(Json.obj("ok" -> true))
          } catch {
            case e: Exception =>
              Logger.error(e.toString())
              BadRequest(Json.obj("ok" -> false, "msg" -> e.toString()))
          }
        })
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

  def eventLogReport(eventTypeStr: String, startStr: String, endStr: String) = Security.Authenticated {
    val eventTypes = eventTypeStr.split(":").map(_.toInt)
    val start = DateTime.parse(startStr)
    val end = DateTime.parse(endStr) + 1.day
    val logs = EventLog.getList(eventTypes, start, end)
    Ok(views.html.eventLogReport(logs))
  }

  def downloadNotification = Security.Authenticated {
    Ok(views.html.downloadNotification())
  }

  def downloadNotificationForm(startStr: String, endStr: String) = Security.Authenticated {

    val start = DateTime.parse(startStr)
    val end = DateTime.parse(endStr)

    val tickets = Ticket.queryActiveTickets(start, end)

    if (tickets.length != 0) {
      val excelFile = ExcelUtility.epbNotification(tickets)
      val title = s"環保局通報單${start.toString("MMdd")}_${end.toString("MMdd")}"

      Ok.sendFile(excelFile, fileName = _ =>
        play.utils.UriEncoding.encodePathSegment(title + ".xlsx", "UTF-8"),
        onClose = () => {
          Files.deleteIfExists(excelFile.toPath())
        })

    } else {
      val output = views.html.notificationPage(tickets)
      val title = s"查詢區間無定保案件"
      Ok.sendFile(creatPdfWithReportHeaderP(title, output),
        fileName = _ =>
          play.utils.UriEncoding.encodePathSegment(title + ".pdf", "UTF-8"))
    }

  }

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
    if (map.values.map {
      _.length
    }.sum == 0) {
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
          val ttMap = {
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
      val now = DateTime.now()
      val start = now - 14.day
      val list = if(start.getYear == now.getYear)
        Alarm.getAlarmNoTicketList(start)
      else
        Alarm.getAlarmNoTicketList(start) ++ Alarm.getAlarmNoTicketList(now.withDayOfYear(1).withMillisOfDay(0))

      val excludedList = list.filter { ar =>
        ar.code != MonitorStatus.REPAIR && ar.code != "053"
      }

      Ok(views.html.alarmNoTicket(excludedList))
  }

  def getEpaTickets() = Security.Authenticated {
    implicit request =>
      val start = DateTime.now() - 3.day
      val list = EpaTicket.getTickets(start)

      Ok(views.html.epaTicket(list))
  }

  def alarmTicketList() = Security.Authenticated {
    implicit request =>
      //FIXME
      val start = DateTime.now() - 2.day
      val list = Alarm.getAlarmAutoTicketList(start)

      Ok(views.html.alarmAutoTicket(list))
  }

  def alarmNoTicketExcel() = Security.Authenticated {
    implicit request =>
      //FIXME
      val start = DateTime.now() - 21.day
      val list = Alarm.getAlarmNoTicketList(start)
      val excludedList = list.filter { ar =>
        ar.code != MonitorStatus.MAINTANCE_STAT && ar.code != MonitorStatus.REPAIR && ar.code != "053"
      }

      val excelFile = ExcelUtility.alarmList(excludedList, "待立案警報 (" + DateTime.now.toString("YYYY/MM/dd HH:mm") + ")")
      Ok.sendFile(excelFile, fileName = _ =>
        play.utils.UriEncoding.encodePathSegment("待立案.xlsx", "UTF-8"),
        onClose = () => {
          Files.deleteIfExists(excelFile.toPath())
        })
  }

  def alarmTicketExcel() = Security.Authenticated {
    implicit request =>
      //FIXME
      val start = DateTime.now() - 2.day
      val list = Alarm.getAlarmAutoTicketList(start)

      val excelFile = ExcelUtility.alarmList(list, "警報立案 (" + DateTime.now.toString("YYYY/MM/dd HH:mm") + ")")
      Ok.sendFile(excelFile, fileName = _ =>
        play.utils.UriEncoding.encodePathSegment("警報立案.xlsx", "UTF-8"),
        onClose = () => {
          Files.deleteIfExists(excelFile.toPath())
        })
  }

  def alarmToTicket() = Security.Authenticated(BodyParsers.parse.json) {
    implicit request =>
      val user = request.user
      implicit val idReads = Json.reads[AlarmTicketParam]
      implicit val paramReads = Json.reads[AlarmToTicketParam]
      val alarmToTicketParam = request.body.validate[AlarmToTicketParam]
      alarmToTicketParam.fold(
        error => {
          Logger.error(JsError.toJson(error).toString())
          BadRequest(Json.obj("ok" -> false, "msg" -> JsError.toJson(error)))
        },
        param => {
          for (alarmId <- param.alarmToTicketList) {
            Alarm.updateAlarmTicketState(alarmId.monitor, alarmId.mItem, new DateTime(alarmId.time), param.status)
            for (ar <- Alarm.getAlarmOpt(alarmId.monitor, alarmId.mItem, new DateTime(alarmId.time))) {
              if (param.status == "YES")
                Alarm.newTicketFromAlarm(ar, DateTime.parse(alarmId.executeDate))
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

      Ok(views.html.manualAlarmTicket(userInfo, group.privilege, adminUsers, List(TicketType.repair)))
  }

  def repairingAlarmTicket(outputTypeStr: String) = Security.Authenticated {
    implicit request =>
      val userInfo = request.user
      val group = Group.getGroup(userInfo.groupID).get
      val tickets = getActiveRepairTicketsByGroup(4)
      val outputType = OutputType.withName(outputTypeStr)

      val ticketWithAlarm =
        for {
          t <- tickets
          arOpt = t.getRepairForm.alarm
        } yield (t, arOpt)
      val allUsers = User.getAllUsers()
      val usrMap = Map(allUsers.map { u => (u.id.get -> u) }: _*)
      val canChangeOwner = userInfo.groupID == 5

      outputType match {
        case OutputType.html =>
          Ok(views.html.repairingTickets(ticketWithAlarm, usrMap, canChangeOwner))
        case OutputType.excel =>
          val excel = ExcelUtility.repairingTickets(ticketWithAlarm, "維修中案件", usrMap)
          Ok.sendFile(excel, fileName = _ =>
            play.utils.UriEncoding.encodePathSegment("維修中案件" + ".xlsx", "UTF-8"),
            onClose = () => {
              Files.deleteIfExists(excel.toPath())
            })
      }
  }

  def repairingAlarmTicketJson = Security.Authenticated {
    val tickets = getActiveRepairTicketsByGroup(4)
    val ticketWithAlarm =
      for {
        t <- tickets
        arOpt = t.getRepairForm.alarm
      } yield (t, arOpt)
    val allUsers = User.getAllUsers()
    val usrMap = Map(allUsers.map { u => (u.id.get -> u) }: _*)

    implicit val writer = Json.writes[RepairingTicketInfo]
    val repairingTicketInfoList = ticketWithAlarm map {
      tAr =>
        val t = tAr._1
        val arOpt = tAr._2
        val (alarmTime, alarmCode) = if (arOpt.isDefined)
          (arOpt.get.time.toString("YYYY/MM/dd HH:mm"), Alarm.getReason(arOpt.get))
        else
          ("-", "-")
        val monitorType = if (t.monitorType.isDefined)
          MonitorType.map(t.monitorType.get).desp
        else
          "-"

        val isDue = if (t.extendDate.isDefined) {
          val extendDate = t.extendDate.get
          s"展延(${extendDate.toString("M/d")})${t.extendReason.getOrElse("")}"
        } else if (t.executeDate.isBefore(DateTime.yesterday))
          "已逾期"
        else
          "未逾期"

        RepairingTicketInfo(id = t.id.toString, alarmTime = alarmTime, alarmCode = alarmCode, monitor = Monitor.map(t.monitor).name, monitorType = monitorType,
          submitDate = t.submit_date.toString("MM-d HH:mm"), submitter = usrMap(t.submiter_id).name, reason = t.reason,
          owner = usrMap(t.owner_id).name, executeDate = t.executeDate.toString("MM-d"), isDue = isDue)
    }
    Ok(Json.toJson(repairingTicketInfoList))
  }

  def dueTicketList(outputTypeStr: String) = Security.Authenticated {
    implicit request =>
      val userInfo = request.user
      val tickets = getActiveRepairDueTicketsByGroup()
      val outputType = OutputType.withName(outputTypeStr)

      val ticketWithAlarm =
        for {
          t <- tickets
          arOpt = t.getRepairForm.alarm
        } yield (t, arOpt)
      val allUsers = User.getAllUsers()
      val usrMap = Map(allUsers.map { u => (u.id.get -> u) }: _*)

      outputType match {
        case OutputType.excel =>
          val excel = ExcelUtility.repairingTickets(ticketWithAlarm, "逾期案件", usrMap)
          Ok.sendFile(excel, fileName = _ =>
            play.utils.UriEncoding.encodePathSegment("逾期案件" + ".xlsx", "UTF-8"),
            onClose = () => {
              Files.deleteIfExists(excel.toPath())
            })
        case OutputType.json =>
          implicit val writer = Json.writes[RepairingTicketInfo]
          val repairingTicketInfoList = ticketWithAlarm map {
            tAr =>
              val t = tAr._1
              val arOpt = tAr._2
              val (alarmTime, alarmCode) = if (arOpt.isDefined)
                (arOpt.get.time.toString("YYYY/MM/dd HH:mm"), Alarm.getReason(arOpt.get))
              else
                ("-", "-")
              val monitorType = if (t.monitorType.isDefined)
                MonitorType.map(t.monitorType.get).desp
              else
                "-"
              val isDue = if (t.executeDate.isBefore(DateTime.yesterday)) {
                if (t.extendDate.isEmpty)
                  "已逾期"
                else {
                  val extendDate = t.extendDate.get
                  s"展延(${extendDate.toString("M/d")})${t.extendReason.getOrElse("")}"
                }
              } else
                "未逾期"

              RepairingTicketInfo(id = t.id.toString, alarmTime = alarmTime, alarmCode = alarmCode, monitor = Monitor.map(t.monitor).name, monitorType = monitorType,
                submitDate = t.submit_date.toString("MM-d HH:mm"), submitter = usrMap(t.submiter_id).name, reason = t.reason,
                owner = usrMap(t.owner_id).name, executeDate = t.executeDate.toString("MM-d"), isDue = isDue)
          }
          Ok(Json.toJson(repairingTicketInfoList))
      }
  }

  def queryClosedRepairTicket = Security.Authenticated {
    implicit request =>
      val userInfo = Security.getUserinfo(request).get
      val group = Group.getGroup(userInfo.groupID).get
      val adminUsers = User.getAdminUsers()

      Ok(views.html.queryClosedRepairTicket(userInfo, group.privilege, adminUsers, List(TicketType.repair)))
  }

  def queryNoRepairTicketAlarm = Security.Authenticated {
    implicit request =>
      val userInfo = Security.getUserinfo(request).get
      val group = Group.getGroup(userInfo.groupID).get
      val adminUsers = User.getAdminUsers()

      Ok(views.html.alarmNoRepair(group.privilege))
  }

  def overStdAlarmTicketQuery = Security.Authenticated {
    implicit request =>
      val userInfo = Security.getUserinfo(request).get
      val group = Group.getGroup(userInfo.groupID).get

      Ok(views.html.overStdAlarmTicketQuery(group.privilege))
  }

  def overStdAlarmTicketReport(levelStr: String, monitorStr: String, startStr: String, endStr: String, outputTypeStr: String) = Security.Authenticated {
    implicit request =>
      val userInfo = Security.getUserinfo(request).get
      val group = Group.getGroup(userInfo.groupID).get
      val levels = levelStr.split(":").map(AlarmLevel.withName)
      val monitors = monitorStr.split(":").map(Monitor.withName)
      val start = DateTime.parse(startStr, DateTimeFormat.forPattern("yyyy-M-d"))
      val end = DateTime.parse(endStr, DateTimeFormat.forPattern("yyyy-M-d"))
      val outputType = OutputType.withName(outputTypeStr)

      val list = Alarm.getAlarmOverStdList(monitors, start, end.plusDays(1))
      val list2 = list.filter(ar => Alarm.isOverStd(ar))

      val list3 = list2.map(ar => {
        val v = ar.mVal
        val m = ar.monitor
        val time = ar.time
        val mt = Alarm.getMonitorType(ar).get

        def checkStdLaw(): Option[Alarm.Alarm] =
          for (stdLaw <- MonitorTypeAlert.map(m)(mt).std_law if v >= stdLaw) yield {
            val mItem = s"${mt.toString}-${AlarmDataType.Hour}-${AlarmLevel.Law}"
            Alarm.Alarm(m, mItem, time.toDateTime, v, MonitorStatus.OVER_STAT)
          }

        def checkWarn(): Option[Alarm.Alarm] =
          for (warn <- MonitorTypeAlert.map(m)(mt).warn if v >= warn) yield {
            val mItem = s"${mt.toString}-${AlarmDataType.Hour}-${AlarmLevel.Warn}"
            Alarm.Alarm(m, mItem, time.toDateTime, v, MonitorStatus.WARN_STAT)
          }

        def checkInternal(): Option[Alarm.Alarm] =
          for (std_internal <- MonitorTypeAlert.map(m)(mt).internal if v >= std_internal) yield {
            val mItem = s"${mt.toString}-${AlarmDataType.Hour}-${AlarmLevel.Internal}"
            Alarm.Alarm(m, mItem, time.toDateTime, v, MonitorStatus.WARN_STAT)
          }

        List(checkStdLaw, checkWarn, checkInternal).flatten.filter(ar=>{
          val level = getOverStdLevel(ar)
          levels.contains(level)
        })
      }).flatten

      outputType match {
        case OutputType.html =>
          Ok(views.html.overStdAlarm(list3))
        case OutputType.excel =>
          val excelFile = ExcelUtility.overStdAlarmList(list3, s"超限警報查詢(${startStr}~${endStr})")
          Ok.sendFile(excelFile, fileName = _ =>
            play.utils.UriEncoding.encodePathSegment("超限警報查詢" + ".xlsx", "UTF-8"),
            onClose = () => {
              Files.deleteIfExists(excelFile.toPath())
            })

      }
  }

  def closedRepairTicketReport(monitorStr: String, startStr: String, endStr: String, outputTypeStr: String) = Security.Authenticated {
    val monitors = monitorStr.split(":").map {
      Monitor.withName
    }
    val start = DateTime.parse(startStr)
    val end = DateTime.parse(endStr) + 1.day
    val outputType = OutputType.withName(outputTypeStr)

    val tickets = Ticket.queryClosedRepairTickets(start, end)
    val filterTicket = tickets.filter { t => monitors.contains(t.monitor) }
    val ticketWithRepairForm =
      for {
        t <- filterTicket
      } yield (t, t.getRepairForm)

    val allUsers = User.getAllUsers()
    val usrMap = Map(allUsers.map { u => (u.id.get -> u) }: _*)

    outputType match {
      case OutputType.html =>
        Ok(views.html.closedAlarmTicketReport(ticketWithRepairForm, usrMap))
      case OutputType.excel =>
        val title = s"結案查詢(${startStr}~${endStr})"
        val excelFile = ExcelUtility.closedRepairTickets(ticketWithRepairForm, title, usrMap)
        Ok.sendFile(excelFile, fileName = _ =>
          play.utils.UriEncoding.encodePathSegment("結案查詢" + ".xlsx", "UTF-8"),
          onClose = () => {
            Files.deleteIfExists(excelFile.toPath())
          })
    }

  }

  def modelList = Security.Authenticated {
    val modelList = Equipment.getDistinctModel()
    Ok(Json.toJson(modelList.filter { x => !x.isEmpty() }))
  }

  def equpimentList = Security.Authenticated {
    val idList = Equipment.getIdList
    var equipmentSet = Set.empty[String]

    for {
      id <- idList
      idParts = id.split("_") if idParts.length == 2
      equipment = idParts(0)
    } {
      equipmentSet += equipment
    }

    val equipmentList = equipmentSet.toList.sorted
    Ok(Json.toJson(equipmentList))
  }

  def freqTypeList = Security.Authenticated {
    Ok(Json.toJson(FrequencyType.getList))
  }

  def partInventoryAlarm(outputTypeStr: String) = Security.Authenticated {
    val outputType = OutputType.withName(outputTypeStr)
    val partUsageList = PartUsage.getList()
    import scala.collection.mutable.Map
    val partUsageMap = Map.empty[String, Int]

    for (partUsage <- partUsageList) {
      val usage = partUsageMap.getOrElse(partUsage.getPartId, 0)
      partUsageMap.put(partUsage.getPartId, usage + partUsage.getUsageBefore(DateTime.now() + 30.day))
    }

    val partIdMap = Part.getIdMap()
    val partInventoryShortage = partUsageMap flatMap {
      kv =>
        val part = partIdMap(kv._1)
        if (part.quantity >= kv._2)
          None
        else
          Some(PartInventory(kv._1, part.quantity, kv._2))
    }

    outputType match {
      case OutputType.html =>
        Ok(views.html.partInventoryAlarm(partInventoryShortage.toList, partIdMap))
      case OutputType.excel =>
        val excelFile = ExcelUtility.partInventoryAlarm(partInventoryShortage.toList, partIdMap, "物料庫存預警")
        Ok.sendFile(excelFile, fileName = _ =>
          play.utils.UriEncoding.encodePathSegment("物料庫存預警" + ".xlsx", "UTF-8"),
          onClose = () => {
            Files.deleteIfExists(excelFile.toPath())
          })

    }
  }

  def sop = Security.Authenticated {
    Ok(views.html.sop())
  }

  def testLineNotify = Security.Authenticated.async {
    implicit request =>
      val userInfo = Security.getUserinfo(request).get
      val f = LineNotify.notify(s"${userInfo.name}測試line訊息")

      for (ret <- f) yield {
        Ok(Json.obj("ok" -> true))
      }
  }

  def insertTestStdAlarm() = Security.Authenticated {
    implicit request =>
      val v = 100f
      for (m <- Monitor.mvList) {
        for (mt <- Monitor.map(m).monitorTypes) {
          val mItem = s"${mt.toString}-${AlarmDataType.Hour}-${AlarmLevel.Law}"
          val ar = Alarm.Alarm(m, mItem, DateTime.now, v, MonitorStatus.OVER_STAT)
          try {
            Alarm.insertAlarm(ar)
          } catch {
            case ex: Exception =>
          }
        }
        for (mt <- Monitor.map(m).monitorTypes) {
          val mItem = s"${mt.toString}-${AlarmDataType.Hour}-${AlarmLevel.Warn}"
          val ar = Alarm.Alarm(m, mItem, DateTime.now, v, MonitorStatus.WARN_STAT)
          try {
            Alarm.insertAlarm(ar)
          } catch {
            case ex: Exception =>
          }
        }
        for (mt <- Monitor.map(m).monitorTypes) {
          val mItem = s"${mt.toString}-${AlarmDataType.Hour}-${AlarmLevel.Internal}"
          val ar = Alarm.Alarm(m, mItem, DateTime.now, v, MonitorStatus.WARN_STAT)
          try {
            Alarm.insertAlarm(ar)
          } catch {
            case ex: Exception =>
          }
        }
      }
      Ok(Json.obj("ok" -> true))
  }

  def deleteExtendReason = Security.Authenticated(BodyParsers.parse.json) {
    implicit request =>
      implicit val reads = Json.reads[DeleteExtendReasonParam]
      val ret = request.body.validate[DeleteExtendReasonParam]
      ret.fold(
        error => {
          Logger.error(JsError.toJson(error).toString())
          BadRequest(Json.obj("ok" -> false, "msg" -> JsError.toJson(error)))
        },
        param => {
          val extendReason = SystemConfig.getExtendedReasons
          val reasons = extendReason.filter(p => p != param.extendReason)
          SystemConfig.setExtededReasons(reasons)
          Ok(Json.obj("ok" -> true))
        })
  }

  def testEpaLine = Security.Authenticated.async {
    for(ret<-LineNotify.notifyEpaGroup("測試環境部群組訊息"))yield
      Ok("")
  }

  case class TicketParam(ticketType: TicketType.Value, monitors: Seq[Monitor.Value],
                         monitorTypes: Seq[MonitorType.Value], reason: String, owner: Int, executeDate: Seq[String],
                         repairType: Option[String], repairSubType: Option[String])

  case class ExtendTickeParam(extendDate: Seq[String], extendReason: String)

  case class TicketInfo(id: String, monitorType: String, monitor: String,
                        submitDate: String, submitter: String, reason: String, owner: String, executeDate: String, ticketType: String,
                        readyToClose: String, active: String, repairType: String, repairSubType: String)

  case class NewPartParam(part: Part2, monitors: List[Monitor.Value], startDate: String, freqType: String, days: Int, usage: Int, alarm: Boolean)

  case class PartUsageInfo(id: String, name: String, chineseName: String, monitor: String,
                           usage: Int, freqDesc: String, startDate: String, nextReplaceDate: String, alarm: String, nochange_reason: String, remark: String)

  case class NewPartUsageParam(partClassId: String, monitors: List[Monitor.Value], startDate: String, freqType: String, days: Int, usage: Int, alarm: Boolean)

  case class UpdatePartUsageParam(id: String, freqType: String, days: Int, usage: Int)

  case class MaintanceWeekEntry(start: DateTime, end: DateTime, offset: Int, map: Map[Monitor.Value, Map[TicketType.Value, List[Ticket]]])

  case class AlarmTicketParam(monitor: Monitor.Value, mItem: String, time: Long, executeDate: String)

  case class AlarmToTicketParam(status: String, alarmToTicketList: Seq[AlarmTicketParam])

  case class RepairingTicketInfo(id: String, alarmTime: String, alarmCode: String, monitorType: String, monitor: String,
                                 submitDate: String, submitter: String, reason: String, owner: String, executeDate: String, isDue: String)

  case class PartInventory(id: String, inventory: Int, usage: Int)

  case class LinePayload(message: String)

  case class DeleteExtendReasonParam(extendReason: String)
}