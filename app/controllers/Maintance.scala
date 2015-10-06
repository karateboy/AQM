package controllers

import play.api._
import play.api.mvc._
import play.api.Logger
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

/**
 * @author user
 */
object Maintance extends Controller {
  def newTicket = Security.Authenticated {
    implicit request =>
      val userInfo = Security.getUserinfo(request).get
      val group = Group.getGroup(userInfo.groupID).get
      val adminUsers = User.getAdminUsers()

      Ok(views.html.newTicket(userInfo, group.privilege, adminUsers))
  }

  case class TicketParam(ticketType: TicketType.Value, monitors: Seq[Monitor.Value],
                         monitorTypes: Seq[MonitorType.Value], reason: String, owner: Int, executeDate: Seq[String])

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
                  ticketParam.owner, m, None, ticketParam.reason, DateTime.parse(date), Json.toJson(Ticket.defaultFormData).toString)
              }
            } else {
              for {
                m <- ticketParam.monitors
                mt <- ticketParam.monitorTypes
                date <- ticketParam.executeDate
              } yield {
                Ticket(0, DateTime.now, true, ticketParam.ticketType, submiterId,
                  ticketParam.owner, m, Some(mt), ticketParam.reason, DateTime.parse(date), Json.toJson(Ticket.defaultRepairFormData).toString)
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

      Ok(views.html.queryTicket(userInfo, group.privilege, adminUsers))
  }

  def ticketReport(ticketTypeStr: String, monitorStr: String, startStr: String, endStr: String) = Security.Authenticated {
    val ticketTypes = ticketTypeStr.split(":").map { TicketType.withName }
    val monitors = monitorStr.split(":").map { Monitor.withName }
    val start = DateTime.parse(startStr)
    val end = DateTime.parse(endStr) + 1.day

    val tickets = Ticket.queryTickets(start, end)
    val filterTicket = tickets.filter { t => ticketTypes.contains(t.ticketType) && monitors.contains(t.monitor) }
    val adminUsers = User.getAdminUsers()
    val usrMap = Map(adminUsers.map { u => (u.id.get -> u) }: _*)

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
        val adminUsers = User.getAdminUsers()
        val usrMap = Map(adminUsers.map { u => (u.id.get -> u) }: _*)

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
              if (t.ticketType == param.ticketType){
                t.formJson
              }else{
                 if(param.ticketType == TicketType.repair)
                   Json.toJson(defaultRepairFormData).toString
                 else
                   Json.toJson(Ticket.defaultFormData).toString
              })

            Ticket.updateTicket(newT)
            Ok(Json.obj("ok" -> true))
          }
        })
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
    val ids = idStr.split(":").toList
    val idInt = ids.map { Integer.parseInt }
    Ticket.closeTicket(idInt)
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
          case TicketType.maintance_week =>
            ExcelUtility.exportWeekForm(ticket, usrMap)
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

  def equipmentHistoryReport(monitorStr: String, startStr: String, endStr: String) = Security.Authenticated {
      val monitors = monitorStr.split(":").map { Monitor.withName }
      val start = DateTime.parse(startStr)
      val end = DateTime.parse(endStr) + 1.day

      val tickets = Ticket.queryRepairTickets(start, end)
      val filterTicket = tickets.filter { t => monitors.contains(t.monitor) }
      val adminUsers = User.getAdminUsers()
      val usrMap = Map(adminUsers.map { u => (u.id.get -> u) }: _*)

    Ok(views.html.equipmentHistoryReport(filterTicket, usrMap))
  }

  def monitorJournal = Security.Authenticated {
    Ok("")
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

}