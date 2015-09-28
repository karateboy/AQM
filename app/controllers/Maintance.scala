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
                  ticketParam.owner, m, None, ticketParam.reason, DateTime.parse(date), Ticket.defaultFormData)
              }
            } else {
              for {
                m <- ticketParam.monitors
                mt <- ticketParam.monitorTypes
                date <- ticketParam.executeDate
              } yield {
                Ticket(0, DateTime.now, true, ticketParam.ticketType, submiterId,
                  ticketParam.owner, m, Some(mt), ticketParam.reason, DateTime.parse(date), Ticket.defaultFormData)
              }
            }
          Logger.debug("# of ticket=" + tickets.length)
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

        Ok(views.html.ticketReport(tickets, usrMap))
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
              executeDate, t.form)

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
          Logger.debug("bool #=>"+ form.boolValues.length)
          Logger.debug("str #=>"+ form.strValues.length)
          Logger.debug("str #=>"+ form.comments.length)
          
          Ticket.updateTicketFormData(ID, form)
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
          case TicketType.maintance_week=>
            Ok(views.html.weekMaintanceForm(ID, t.form))
          case TicketType.maintance_biweek=>
            Ok(views.html.biweekForm(ID, t.form))
          case TicketType.maintance_month=>
            Ok(views.html.monthForm(ID, t.form))
          case TicketType.maintance_quarter=>
            Ok(views.html.quarterForm(ID, t.form))
          case TicketType.maintance_half_year=>
            Ok(views.html.halfYearForm(ID, t.form))
          case TicketType.maintance_year=>
            Ok(views.html.yearForm(ID, t.form))
      }
    }
  }

  def closeTicket = Security.Authenticated {
    Ok("")
  }

  def equipmentHistory = Security.Authenticated {
    Ok("")
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