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
import play.api.data._
import play.api.data.Forms._
import play.api.libs.ws._
import play.api.libs.ws.ning.NingAsyncHttpClientConfigBuilder
import scala.concurrent.Future
import PdfUtility._
import play.api.libs.json._
import play.api.libs.functional.syntax._
object Application extends Controller {

  val title = "麥寮廠區空氣品質及氣象監測系統"

  def index = Security.Authenticated {
    implicit request =>
      val userInfoOpt = Security.getUserinfo(request)
      if (userInfoOpt.isEmpty) {
        Forbidden("Invalid access!")
      } else {
        val userInfo = userInfoOpt.get
        val group = Group.getGroup(userInfo.groupID).get
        Ok(views.html.index(title, userInfo, group.privilege))
      }
  }

  def monitor(monitor: String) = Security.Authenticated {
    implicit request =>
      val m = Monitor.withName(monitor)
      Ok(views.html.monitor(m))
  }

  case class MonitorInfo(mt:Seq[MonitorType.Value], imgUrl:String, equipments:List[Equipment])
  implicit val equipmentWrite = Json.writes[Equipment]
  implicit val mInfoWrite = Json.writes[MonitorInfo]
  
  def getMonitorInfo(monitorStr: String) = Security.Authenticated {
    implicit request =>
      val m = Monitor.withName(monitorStr)
      val info = MonitorInfo(Monitor.map(m).monitorTypes, Monitor.map(m).url, Equipment.map.getOrElse(m, List.empty))

      Ok(Json.toJson(info))
  }

  def setMonitorTypes(monitorStr: String) = Security.Authenticated(BodyParsers.parse.json) {
    implicit request =>
      val monitor = Monitor.withName(monitorStr)
      val monitorTypes = request.body.validate[Seq[MonitorType.Value]]

      monitorTypes.fold(
        error => {
          Logger.error(JsError.toFlatJson(error).toString())
          BadRequest(Json.obj("ok" -> false, "msg" -> JsError.toFlatJson(error)))
        },
        mt => {
          Monitor.updateMonitorTypes(monitor, mt)
          Ok(Json.obj("ok" -> true))
        })
  }

  def getInternalStd(monitorStr: String, mtStr: String) = Security.Authenticated {
    implicit request =>
      val m = Monitor.withName(monitorStr)
      val mt = MonitorType.withName(mtStr)
      val std = Monitor.map(m).getStdInternal(mt)

      Ok(Json.obj(
        if (std.isDefined)
          "std" -> std.get
        else
          "std" -> "-"))
  }

  def setInternalStd(monitorStr: String, monitorTypeStr: String, stdStr: String) = Security.Authenticated(BodyParsers.parse.json) {
    implicit request =>
      import java.lang.Float
      val m = Monitor.withName(monitorStr)
      val monitorType = MonitorType.withName(monitorTypeStr)
      val oldCase = Monitor.map(m)

      val newStd =
        if (stdStr == "-") {
          oldCase.monitorTypeStds.filter { _.id != monitorType }
        } else {
          val stdValue = Float.parseFloat(stdStr)
          oldCase.getNewStd(monitorType, stdValue)
        }

      val newCase = Monitor(oldCase.id, oldCase.name, oldCase.lat, oldCase.lng, oldCase.url, oldCase.autoAudit, oldCase.monitorTypes, newStd)
      Monitor.updateStdInternal(newCase)

      Ok(Json.obj("ok" -> true))
  }
  
  def setMonitorImgUrl(monitorStr: String) = Security.Authenticated(BodyParsers.parse.json) {
    implicit request =>
      val monitor = Monitor.withName(monitorStr)
      val imgUrlResult = request.body.validate[String]

      imgUrlResult.fold(
        error => {
          Logger.error(JsError.toFlatJson(error).toString())
          BadRequest(Json.obj("ok" -> false, "msg" -> JsError.toFlatJson(error)))
        },
        imgUrl => {
           Monitor.updateImgUrl(monitor, imgUrl)
          Ok(Json.obj("ok" -> true))
        })
  }
  def monitorTypeConfig = Security.Authenticated {
    implicit request =>
      Ok(views.html.monitorTypeConfig())
  }

  case class EditData(id: String, data: String)
  def saveMonitorTypeConfig() = Security.Authenticated {
    implicit request =>
      try {
        val mtForm = Form(
          mapping(
            "id" -> text,
            "data" -> text)(EditData.apply)(EditData.unapply))

        val mtData = mtForm.bindFromRequest.get
        val mtInfo = mtData.id.split(":")
        val mt = MonitorType.withName(mtInfo(0))

        MonitorType.updateMonitorType(mt, mtInfo(1), mtData.data)

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

  def newEquipment = Security.Authenticated(BodyParsers.parse.json) {
    implicit request =>
      val newEquipmentResult = request.body.validate[Equipment]

      newEquipmentResult.fold(
        error => {
          Logger.error(JsError.toFlatJson(error).toString())
          BadRequest(Json.obj("ok" -> false, "msg" -> JsError.toFlatJson(error)))
        },
        param => {
          Logger.debug(param.toString())
          try {
            Equipment.create(param)
          } catch {
            case e: Exception =>
              Logger.error(e.toString())
              BadRequest(Json.obj("ok" -> false, "msg" -> e.toString()))
          }

          Ok(Json.obj("ok" -> true))
        })
  }

  def updateEquipment() = Security.Authenticated{
    implicit request =>
      try {
        val mtForm = Form(
          mapping(
            "id" -> text,
            "data" -> text)(EditData.apply)(EditData.unapply))

        val mtData = mtForm.bindFromRequest.get
        val ids = mtData.id.split(":")
       
        Equipment.update(ids, mtData.data)        

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

  def deleteEquipment(idStr:String) = Security.Authenticated{
    val ids = idStr.split(":")
    Equipment.delete(Monitor.withName(ids(0)), ids(1))
    Ok(Json.obj("ok" -> true))
  }
  
  def monitorStatusConfig = Security.Authenticated {
    implicit request =>
      Ok(views.html.monitorStatusConfig())
  }

  def saveMonitorStatusConfig() = Security.Authenticated {
   implicit request =>
      try {
        val mtForm = Form(
          mapping(
            "id" -> text,
            "data" -> text)(EditData.apply)(EditData.unapply))

        val msData = mtForm.bindFromRequest.get

        MonitorStatus.update(msData.id, msData.data)

        Ok(msData.data)
      } catch {
        case e: Exception =>
          Logger.error(e.toString)
          BadRequest(e.toString)
        case e: Throwable =>
          Logger.error(e.toString)
          BadRequest(e.toString)
      }
  }

  def recordValidation = Security.Authenticated {
    implicit request =>
      Ok(views.html.recordValidation())
  }

  def recordValidationHtml(startStr: String) = Security.Authenticated {
    implicit request =>
      val start = DateTime.parse(startStr)
      val nextDay = start + 1.day
      val end =
        if (nextDay > DateTime.now)
          DateTime.now
        else
          nextDay

      val report = Record.getRecordValidationReport(start, end)

      Ok(views.html.recordValidationReport(start, end, report))
  }

  case class EpaRealtimeData(
    siteName: String,
    county: String,
    psi: String,
    so2: String,
    co: String,
    o3: String,
    pm10: String,
    pm25: String,
    no2: String,
    windSpeed: String,
    windDir: String,
    publishTime: String)

  implicit val epaRealtimeDataRead: Reads[EpaRealtimeData] =
    ((__ \ "SiteName").read[String] and
      (__ \ "County").read[String] and
      (__ \ "PSI").read[String] and
      (__ \ "SO2").read[String] and
      (__ \ "CO").read[String] and
      (__ \ "O3").read[String] and
      (__ \ "PM10").read[String] and
      (__ \ "PM2.5").read[String] and
      (__ \ "NO2").read[String] and
      (__ \ "WindSpeed").read[String] and
      (__ \ "WindDirec").read[String] and
      (__ \ "PublishTime").read[String])(EpaRealtimeData.apply _)

  import play.api.libs.concurrent.Execution.Implicits.defaultContext
  def realtimeEpaRecord = Security.Authenticated.async {
    implicit request =>
      {
        val url = "http://opendata.epa.gov.tw/ws/Data/AQX/?$orderby=SiteName&$skip=0&$top=1000&format=json"
        WS.url(url).get().map {
          response =>
            val epaData = response.json.validate[Seq[EpaRealtimeData]]
            epaData.fold(
              error => {
                Logger.error(JsError.toFlatJson(error).toString())
                BadRequest(Json.obj("ok" -> false, "msg" -> JsError.toFlatJson(error)))
              },
              data => {
                Logger.info("#=" + data.length)
                Ok(views.html.epaRealtimeData(url, data))
              })
        }
      }
  }

  def userManagement() = Security.Authenticated {
    implicit request =>
      val userInfoOpt = Security.getUserinfo(request)
      if (userInfoOpt.isEmpty)
        Forbidden("No such user!")
      else {
        val userInfo = userInfoOpt.get
        val user = User.getUserById(userInfo.id).get
        val (userList, groupList) =
          if (!user.isAdmin)
            (List[User](), List[Group]())
          else
            (User.getAllUsers, Group.getAllGroups)

        Ok(views.html.userManagement(userInfo, user, userList, groupList))
      }
  }

  import models.User._
  implicit val userParamRead = Json.reads[User]

  def newUser = Security.Authenticated(BodyParsers.parse.json) {
    implicit request =>
      val newUserParam = request.body.validate[User]

      newUserParam.fold(
        error => {
          Logger.error(JsError.toFlatJson(error).toString())
          BadRequest(Json.obj("ok" -> false, "msg" -> JsError.toFlatJson(error)))
        },
        param => {
          User.newUser(param)
          Ok(Json.obj("ok" -> true))
        })
  }

  def deleteUser(id: Int) = Security.Authenticated {
    implicit request =>
      Logger.info("deleteUser")
      User.deleteUser(id)
      Ok(Json.obj("ok" -> true))
  }

  def getUser(id: Int) = Security.Authenticated {
    Ok("")
  }

  def updateUser(id: Int) = Security.Authenticated(BodyParsers.parse.json) {
    implicit request =>
      val userParam = request.body.validate[User]

      userParam.fold(
        error => {
          Logger.error(JsError.toFlatJson(error).toString())
          BadRequest(Json.obj("ok" -> false, "msg" -> JsError.toFlatJson(error)))
        },
        param => {
          User.updateUser(param)
          Ok(Json.obj("ok" -> true))
        })
  }

  def getAllUsers = Security.Authenticated {
    val users = User.getAllUsers()
    implicit val userWrites = Json.writes[User]

    Ok(Json.toJson(users))
  }

  def groupManagement() = Security.Authenticated {
    implicit request =>
      val userInfoOpt = Security.getUserinfo(request)
      if (userInfoOpt.isEmpty)
        Forbidden("No such user!")
      else {
        val userInfo = userInfoOpt.get
        if (!userInfo.isAdmin) {
          Forbidden("Only administrator can manage group!")
        } else {
          val group = Group.getGroup(userInfo.groupID)
          val groupList = Group.getAllGroups()
          Ok(views.html.groupManagement(userInfo, group.get, groupList))
        }
      }
  }

  import Privilege._
  implicit val groupParamRead = Json.reads[Group]
  def newGroup = Security.Authenticated(BodyParsers.parse.json) {
    implicit request =>

      val newGroupParam = request.body.validate[Group]

      newGroupParam.fold(
        error => {
          Logger.error(JsError.toFlatJson(error).toString())
          BadRequest(Json.obj("ok" -> false, "msg" -> JsError.toFlatJson(error)))
        },
        param => {
          Group.newGroup(param)
          Ok(Json.obj("ok" -> true))
        })
  }

  def deleteGroup(id: Int) = Security.Authenticated {
    implicit request =>
      if (User.getCountGroupID(id) != 0)
        Ok(Json.obj("ok" -> false, "reason" -> "該群組正被使用"))
      else {
        Group.deleteGroup(id)
        Ok(Json.obj("ok" -> true))
      }
  }

  def updateGroup(id: Int) = Security.Authenticated(BodyParsers.parse.json) {
    implicit request =>
      Logger.info("updateGroup")
      val groupParam = request.body.validate[Group]

      groupParam.fold(
        error => {
          Logger.error(JsError.toFlatJson(error).toString())
          BadRequest(Json.obj("ok" -> false, "msg" -> JsError.toFlatJson(error)))
        },
        param => {
          Group.updateGroup(param)
          Ok(Json.obj("ok" -> true))
        })
  }

  def getAllGroups = Security.Authenticated {
    val groups = Group.getAllGroups()
    implicit val groupWrites = Json.writes[Group]

    Ok(Json.toJson(groups))
  }

  def manualAudit = Security.Authenticated {
    implicit request =>
      val userInfo = Security.getUserinfo(request).get
      val group = Group.getGroup(userInfo.groupID).get
      Ok(views.html.history("/HistoryQueryReport/true/", group.privilege))
  }

  case class ManualAudit(monitor: Monitor.Value, monitorType: MonitorType.Value, time: Long, status: String)
  case class ManualAuditList(list: Seq[ManualAudit])
  def manualAuditApply(recordTypeStr:String) = Security.Authenticated(BodyParsers.parse.json) {
    implicit request =>
      val tabType = TableType.withName(recordTypeStr)
      implicit val manualAuditReads = Json.reads[ManualAudit]
      implicit val manualAuditListReads = Json.reads[ManualAuditList]
      val manualAuditList = request.body.validate[ManualAuditList]
      manualAuditList.fold(
        error => {
          Logger.error(JsError.toFlatJson(error).toString())
          BadRequest(Json.obj("ok" -> false, "msg" -> JsError.toFlatJson(error)))
        },
        manualAuditList => {
          for (ma <- manualAuditList.list) {
            Record.updateRecordStatus(tabType, ma.monitor, ma.monitorType, ma.time, ma.status)
          }

          Ok(Json.obj("ok" -> true))
        })
  }

  def auditConfig() = Security.Authenticated {
    implicit request =>
      val userInfo = Security.getUserinfo(request).get
      val group = Group.getGroup(userInfo.groupID).get

      Ok(views.html.auditConfig(group.privilege))
  }

  def getMonitorAuditConfig(monitorStr: String) = Security.Authenticated {
    implicit request =>
      val userInfo = Security.getUserinfo(request).get
      val group = Group.getGroup(userInfo.groupID).get
      val m = Monitor.withName(monitorStr)

      val auditConfig = Monitor.map(m).autoAudit

      Ok(Json.toJson(auditConfig))
  }

  def setMonitorAuditConfig(monitorStr: String) = Security.Authenticated(BodyParsers.parse.json) {
    implicit request =>
      val monitor = Monitor.withName(monitorStr)
      val autoAuditResult = request.body.validate[AutoAudit]

      autoAuditResult.fold(
        error => {
          Logger.error(JsError.toFlatJson(error).toString())
          BadRequest(Json.obj("ok" -> false, "msg" -> JsError.toFlatJson(error)))
        },
        autoAudit => {
          Monitor.updateMonitorAutoAudit(monitor, autoAudit)
          Ok(Json.obj("ok" -> true))
        })
  }

  def instrument() = Security.Authenticated {
    implicit request =>
      val userInfo = Security.getUserinfo(request).get
      val group = Group.getGroup(userInfo.groupID).get

      Ok(views.html.instrument(group.privilege))
  }

  def instrumentReport(monitorStr: String, instrumentStr: String, startStr: String, endStr: String, outputTypeStr: String) = Security.Authenticated {
    val monitor = Monitor.withName(monitorStr)
    val instrument = Instrument.withName(instrumentStr)
    val start = DateTime.parse(startStr)
    val end = DateTime.parse(endStr) + 1.day
    val outputType = OutputType.withName(outputTypeStr)

    val output =
      instrument match {
        case Instrument.H370 =>
          val records = Instrument.getH370Record(monitor, start, end)
          views.html.h370Report(monitor, start, end, records)
        case Instrument.T100 =>
          val records = Instrument.getT100Record(monitor, start, end)
          views.html.T100Report(monitor, start, end, records)
        case Instrument.T200 =>
          val records = Instrument.getT200Record(monitor, start, end)
          views.html.T200Report(monitor, start, end, records)
        case Instrument.T300 =>
          val records = Instrument.getT300Record(monitor, start, end)
          views.html.T300Report(monitor, start, end, records)
        case Instrument.T400 =>
          val records = Instrument.getT400Record(monitor, start, end)
          views.html.T400Report(monitor, start, end, records)
      }
    val title = "儀器狀態"

    outputType match {
      case OutputType.html =>
        Ok(output)
      case OutputType.pdf =>
        Ok.sendFile(creatPdfWithReportHeader(title, output),
          fileName = _ =>
            play.utils.UriEncoding.encodePathSegment(Monitor.map(monitor).name + title + start.toString("YYYYMMdd") + "_" + end.toString("MMdd") + ".pdf", "UTF-8"))
    }
  }
}
