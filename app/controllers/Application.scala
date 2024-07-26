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
import scalikejdbc.DB
object Application extends Controller {

  val title = "麥寮廠區空氣品質及氣象監測系統"

  def index = Security.Authenticated {
    implicit request =>
      val userInfoOpt = Security.getUserinfo(request)
      if (userInfoOpt.isEmpty) {
        Forbidden("Invalid access!")
      } else {
        val userInfo = userInfoOpt.get
        val user = User.getUserById(userInfo.id).get
        val group = Group.getGroup(userInfo.groupID).get
        Ok(views.html.index(title, user, userInfo, group.privilege))
      }
  }

  def monitorConfig(monitor: String) = Security.Authenticated {
    implicit request =>
      val m = Monitor.withName(monitor)
      Ok(views.html.monitorConfig(m))
  }

  def monitorList = Security.Authenticated {
    implicit request =>
      Ok(Json.toJson(Monitor.monitorList))
  }

  def epaMonitorList = Security.Authenticated {
    implicit request =>
      implicit val write = Json.writes[EpaMonitor]
      Ok(Json.toJson(EpaMonitor.map.values.toList.sortBy(_.id)))
  }

  case class MonitorInfo(mt: Seq[MonitorType.Value], imgUrl: String, equipments: List[Equipment])
  implicit val equipmentWrite = Json.writes[Equipment]
  implicit val mInfoWrite = Json.writes[MonitorInfo]

  def getMonitorInfo(monitorStr: String) = Security.Authenticated {
    implicit request =>
      val m = Monitor.withName(monitorStr)
      import scala.collection.mutable.Map

      val equipNameMap = Equipment.map.getOrElseUpdate(m, Map.empty[String, Equipment])

      val equipmentList = equipNameMap.values.toList
      val info = MonitorInfo(Monitor.map(m).monitorTypes, Monitor.map(m).url, equipmentList)

      Ok(Json.toJson(info))
  }

  def getMonitorBios(monitorStr: String) = Security.Authenticated {
    implicit request =>
      val m = Monitor.withName(monitorStr)
      val bios = MonitorBios.map(m)

      Ok(views.html.monitorBios(m, bios))
  }

  def setMonitorTypes(monitorStr: String) = Security.Authenticated(BodyParsers.parse.json) {
    implicit request =>
      val monitor = Monitor.withName(monitorStr)
      val monitorTypes = request.body.validate[Seq[MonitorType.Value]]

      monitorTypes.fold(
        error => {
          Logger.error(JsError.toJson(error).toString())
          BadRequest(Json.obj("ok" -> false, "msg" -> JsError.toJson(error)))
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
      val std = MonitorTypeAlert.map(m)(mt).internal

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

      val newCase = Monitor(oldCase.id, oldCase.name, oldCase.lat, oldCase.lng, oldCase.url, oldCase.autoAudit, oldCase.monitorTypes,
        newStd, oldCase.calibrationZds, oldCase.calibrationSds)
      Monitor.updateStdInternal(newCase)

      Ok(Json.obj("ok" -> true))
  }

  def getCalibrationInternalStd(monitorStr: String, mtStr: String) = Security.Authenticated {
    implicit request =>
      val m = Monitor.withName(monitorStr)
      val mt = MonitorType.withName(mtStr)
      val zdStr = Monitor.map(m).zdInternal(mt).map(_.toString).getOrElse("-")
      val sdStr = Monitor.map(m).sdInternal(mt).map(_.toString).getOrElse("-")

      Ok(Json.obj("zd" -> zdStr, "sd" -> sdStr))
  }
  def setCalibrationInternalStd(monitorStr: String, monitorTypeStr: String, zdStr: String, sdStr: String) = Security.Authenticated(BodyParsers.parse.json) {
    implicit request =>
      import java.lang.Float
      val m = Monitor.withName(monitorStr)
      val monitorType = MonitorType.withName(monitorTypeStr)
      val oldCase = Monitor.map(m)

      val newZd = {
        val pairs = oldCase.calibrationZds map { zds => zds.id -> zds.std_internal }
        val map = pairs.toMap
        val newMap =
          if (zdStr == "-") {
            map - monitorType
          } else {
            val v = Float.parseFloat(zdStr)
            map + (monitorType -> v)
          }
        newMap.map { kv => MonitorTypeStandard(kv._1, kv._2) }
      }

      val newSd = {
        val pairs = oldCase.calibrationSds map { sds => sds.id -> sds.std_internal }
        val map = pairs.toMap
        val newMap =
          if (zdStr == "-") {
            map - monitorType
          } else {
            val v = Float.parseFloat(sdStr)
            map + (monitorType -> v)
          }
        newMap.map { kv => MonitorTypeStandard(kv._1, kv._2) }
      }

      val newCase = Monitor(oldCase.id, oldCase.name, oldCase.lat, oldCase.lng, oldCase.url, oldCase.autoAudit, oldCase.monitorTypes,
        oldCase.monitorTypeStds, newZd.toSeq, newSd.toSeq)
      Monitor.updateCalibrationInternalStd(newCase)

      Ok(Json.obj("ok" -> true))
  }
  def setMonitorImgUrl(monitorStr: String) = Security.Authenticated(BodyParsers.parse.json) {
    implicit request =>
      val monitor = Monitor.withName(monitorStr)
      val imgUrlResult = request.body.validate[String]

      imgUrlResult.fold(
        error => {
          Logger.error(JsError.toJson(error).toString())
          BadRequest(Json.obj("ok" -> false, "msg" -> JsError.toJson(error)))
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

  def getAutoAuditNormal()= Security.Authenticated {
    implicit request =>
      val v = SystemConfig.getConfig(SystemConfig.AutoAuditAsNormal, "true")
      Ok(Json.obj("value" -> v.toBoolean))
  }

  def setAutoAuditNormal(booleanStr: String) = Security.Authenticated {
    implicit request =>
      SystemConfig.setConfig(SystemConfig.AutoAuditAsNormal, booleanStr)
      Ok(Json.obj("ok" -> true))
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
        case e: Throwable =>
          Logger.error(e.toString, e)
          BadRequest(e.toString)
      }
  }

  def setInstrumentThreshold() = Security.Authenticated {
    implicit request =>
      try {
        val mtForm = Form(
          mapping(
            "id" -> text,
            "data" -> text)(EditData.apply)(EditData.unapply))

        val threshold = mtForm.bindFromRequest.get

        val v =
          if (threshold.data.length() == 0 || threshold.data == "-")
            None
          else
            Some(threshold.data.toFloat)

        InstrumentThreshold.setValue(threshold.id, v)

        Ok(threshold.data)
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
          Logger.error(JsError.toJson(error).toString())
          BadRequest(Json.obj("ok" -> false, "msg" -> JsError.toJson(error)))
        },
        param => {
          try {
            Equipment.create(param)
            Ok(Json.obj("ok" -> true))
          } catch {
            case ex: Exception =>
              Logger.error(ex.toString(), ex)
              Ok(Json.obj("ok" -> false, "msg" -> ex.toString()))
          }
        })
  }

  def updateEquipment() = Security.Authenticated {
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

  def deleteEquipment(idStr: String) = Security.Authenticated {
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
      val url = "http://opendata.epa.gov.tw/ws/Data/AQX/?$orderby=SiteName&$skip=0&$top=1000&format=json"
      WS.url(url).get().map {
        response =>
          try {
            val epaData = response.json.validate[Seq[EpaRealtimeData]]
            epaData.fold(
              error => {
                Logger.error(JsError.toJson(error).toString())
                Ok(views.html.epaRealtimeData(url, Seq.empty[EpaRealtimeData]))
              },
              data => {
                Ok(views.html.epaRealtimeData(url, data))
              })

          } catch {
            case ex: Exception =>
              Logger.error(ex.toString)
              Ok(views.html.epaRealtimeData(url, Seq.empty[EpaRealtimeData]))
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
          Logger.error(JsError.toJson(error).toString())
          BadRequest(Json.obj("ok" -> false, "msg" -> JsError.toJson(error)))
        },
        param => {
          User.newUser(param)
          Ok(Json.obj("ok" -> true))
        })
  }

  def deleteUser(id: Int) = Security.Authenticated {
    implicit request =>
      val userInfoOpt = Security.getUserinfo(request)
      val userInfo = userInfoOpt.get

      Ticket.transferTickets(id, userInfo.id)
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
          Logger.error(JsError.toJson(error).toString())
          BadRequest(Json.obj("ok" -> false, "msg" -> JsError.toJson(error)))
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
          Logger.error(JsError.toJson(error).toString())
          BadRequest(Json.obj("ok" -> false, "msg" -> JsError.toJson(error)))
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
          Logger.error(JsError.toJson(error).toString())
          BadRequest(Json.obj("ok" -> false, "msg" -> JsError.toJson(error)))
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
      Ok(views.html.history("/HistoryQueryReport/true/", group.privilege, true))
  }

  case class ManualAudit(monitor: Monitor.Value, monitorType: MonitorType.Value, time: Long, status: String, reason: Option[String])
  case class ManualAuditList(list: Seq[ManualAudit])
  def manualAuditApply(recordTypeStr: String) = Security.Authenticated(BodyParsers.parse.json) {
    implicit request =>
      val userInfo = Security.getUserinfo(request).get
      val user = User.getUserById(userInfo.id).get
      val tabType = TableType.withName(recordTypeStr)
      implicit val manualAuditReads = Json.reads[ManualAudit]
      implicit val manualAuditListReads = Json.reads[ManualAuditList]
      val manualAuditList = request.body.validate[ManualAuditList]
      manualAuditList.fold(
        error => {
          Logger.error(JsError.toJson(error).toString())
          BadRequest(Json.obj("ok" -> false, "msg" -> JsError.toJson(error)))
        },
        manualAuditList => {
          for (ma <- manualAuditList.list.reverse) {
            val tagInfo = MonitorStatus.getTagInfo(ma.status)
            EventLog.create(EventLog(DateTime.now, EventLog.evtTypeManualAudit,
              s"${user.name} 進行人工註記 :${new DateTime(ma.time).toString("YYYY/MM/dd HH:mm")}:${Monitor.map(ma.monitor).name}:${MonitorType.map(ma.monitorType).desp}-${MonitorStatus.map(ma.status).desp}"))
            if (tagInfo.statusType == StatusType.Manual) {
              val log = ManualAuditLog(tabType, ma.monitor, new DateTime(ma.time), ma.monitorType, DateTime.now, ma.status, user.name, ma.reason)
              try {
                val logOpt = ManualAuditLog.getLog(tabType, ma.monitor, new DateTime(ma.time), ma.monitorType)
                if (logOpt.isEmpty)
                  ManualAuditLog.newLog(log)
                else
                  ManualAuditLog.updateLog(log)
              } catch {
                case ex: Exception =>

              }
            } else if (tagInfo.statusType == StatusType.Internal) {
              ManualAuditLog.deleteLog(tabType, ma.monitor, new DateTime(ma.time), ma.monitorType)
            }

            Record.updateRecordStatus(tabType, ma.monitor, ma.monitorType, ma.time, ma.status)
            val startTime = new DateTime(ma.time)
            val endTime = if(tabType == TableType.Hour)
              startTime + 1.hour
            else
              startTime + 1.minute
            OverStdConverter.convertStatus(ma.monitor, tabType, startTime, endTime)
          }

          Ok(Json.obj("ok" -> true))
        })
  }

  def manualAuditQuery() = Security.Authenticated {
    implicit request =>
      val userInfo = Security.getUserinfo(request).get
      val group = Group.getGroup(userInfo.groupID).get

      Ok(views.html.manualAuditQuery(group.privilege))
  }

  def manualAuditQueryReport(monitorStr: String, startStr: String, endStr: String, outputTypeStr: String) = Security.Authenticated {
    val monitorStrArray = monitorStr.split(':')
    val monitors = monitorStrArray.map { Monitor.withName }
    val start = DateTime.parse(startStr)
    val end = DateTime.parse(endStr) + 1.day
    val outputType = OutputType.withName(outputTypeStr)

    val records = ManualAuditLog.getLogs(TableType.Hour, monitors, start, end)

    val output = views.html.manualAuditQueryReport(start, end, records)
    val title = "人工註記查詢"
    outputType match {
      case OutputType.html =>
        Ok(output)
      //case OutputType.excel =>

      case OutputType.pdf =>
        Ok.sendFile(creatPdfWithReportHeader(title, output),
          fileName = _ =>
            play.utils.UriEncoding.encodePathSegment(title + start.toString("YYMMdd") + "_" + end.toString("MMdd") + ".pdf", "UTF-8"))
    }

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

      val autoAudit = Monitor.map(m).autoAudit

      Ok(Json.toJson(autoAudit))
  }

  def setMonitorAuditConfig(monitorStr: String) = Security.Authenticated(BodyParsers.parse.json) {
    implicit request =>
      val monitor = Monitor.withName(monitorStr)
      val autoAuditResult = request.body.validate[AutoAudit]

      autoAuditResult.fold(
        error => {
          Logger.error(JsError.toJson(error).toString())
          BadRequest(Json.obj("ok" -> false, "msg" -> JsError.toJson(error)))
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
        case Instrument.TSP =>
          val records = Instrument.getTSPRecord(monitor, start, end)
          views.html.TSPReport(monitor, start, end, records)
        case Instrument.PM10 =>
          val records = Instrument.getPM10Record(monitor, start, end)
          views.html.PM10Report(monitor, start, end, records)
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

  def getPm10Threshold = Security.Authenticated {
    Ok(SystemConfig.getPM10Threshold().toString())
  }

  case class Threshold(v: Double)
  def setPm10Threshold = Security.Authenticated(BodyParsers.parse.json) {
    implicit request =>
      implicit val read = Json.reads[Threshold]
      val result = request.body.validate[Threshold]

      result.fold(error => {
        Logger.error(JsError.toJson(error).toString())
        BadRequest(Json.obj("ok" -> false, "msg" -> JsError.toJson(error)))
      },
        threshold => {
          SystemConfig.setPM10Threshold(threshold.v)
          Ok(Json.obj("ok" -> true))
        })
  }

  def getMonitorTypeAlertInfo(monitorStr:String) = Security.Authenticated {
    val monitor = Monitor.withName(monitorStr)
    val list = MonitorTypeAlert.map(monitor).filter(p=>Monitor.map(monitor).monitorTypes.contains(p._1))
      .values.toList.map{mta=>mta.getInfo}.sortBy(_.monitorTypeID)

    implicit val write = Json.writes[MonitorTypeAlertInfo]
    Ok(Json.toJson(list))
  }

  def getEpaMonitorTypeAlertInfo(monitorID:Int) = Security.Authenticated {
    val epaMonitor = EpaMonitor.idMap(monitorID)
    val list = EpaMonitorTypeAlert.map(epaMonitor).values.toList.sortBy(_.monitorType)

    implicit val write = Json.writes[EpaMonitorTypeAlert]
    Ok(Json.toJson(list))
  }

  def updateMonitorTypeAlertInfo()= Security.Authenticated(BodyParsers.parse.json) {
    implicit request =>
      implicit val read = Json.reads[MonitorTypeAlertInfo]
      val monitorTypes = request.body.validate[MonitorTypeAlertInfo]

      monitorTypes.fold(
        error => {
          Logger.error(JsError.toJson(error).toString())
          BadRequest(Json.obj("ok" -> false, "msg" -> JsError.toJson(error)))
        },
        mta => {
          MonitorTypeAlert.save(mta)
          Ok(Json.obj("ok" -> true))
        })
  }

  def updateEpaMonitorTypeAlert()= Security.Authenticated(BodyParsers.parse.json) {
    implicit request =>
      implicit val read = Json.reads[EpaMonitorTypeAlert]
      val ret = request.body.validate[EpaMonitorTypeAlert]

      ret.fold(
        error => {
          Logger.error(JsError.toJson(error).toString())
          BadRequest(Json.obj("ok" -> false, "msg" -> JsError.toJson(error)))
        },
        mta => {
          DB autoCommit {
            implicit session=>
              EpaMonitorTypeAlert.save(mta)
          }
          Ok(Json.obj("ok" -> true))
        })
  }
  def getMonitorTypes()  = Security.Authenticated {
    val list = MonitorType.map.values.toList.sortBy(_.id)
    implicit val writes = Json.writes[MonitorType]
    implicit val write = Json.writes[MonitorTypeAlert]
    Ok(Json.toJson(list))
  }

  def reloadEpaData(start:Long, end:Long)= Security.Authenticated {
    implicit request =>
    val startDate = new DateTime(start).withMillisOfDay(0)
    val endDate = new DateTime(end).withMillisOfDay(0)
    val userInfo = Security.getUserinfo(request).get
    LineNotify.notify(s"${userInfo.name}回補環境部資料自${startDate.toString("yyyy/MM/dd")} 至 ${endDate.toString("yyyy/MM/dd")}")
    OpenDataReceiver.reloadEpaData(startDate, endDate)
    Ok("")
  }
}
