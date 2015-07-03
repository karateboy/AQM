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
import play.api.libs.ws._
import play.api.libs.ws.ning.NingAsyncHttpClientConfigBuilder
import scala.concurrent.Future

object Application extends Controller {

  val title = "麥寮廠區空氣品質及氣象監測系統"
  
  def index = Security.Authenticated{
    implicit request =>
      val userInfoOpt = Security.getUserinfo(request)
      if(userInfoOpt.isEmpty){
        Forbidden("Invalid access!")
      }else{
        val userInfo = userInfoOpt.get
        
        Ok(views.html.index(title, userInfo))
      }
        
  }
  
  def monitor(monitor:String) = Security.Authenticated{
    implicit request =>
      Logger.debug("monitor=>"+monitor)
      val monitorValue = Monitor.withName(monitor)
    Ok(views.html.monitor(monitor))
  }
  
  def monitoredTypes(monitorStr:String) = Security.Authenticated{
    implicit request =>
      val monitor = Monitor.withName(monitorStr)
      val monitoredTypes = MonitoredType.getMonitoredTypes(monitor)
    Ok(views.html.monitoredTypes(monitor.toString(), monitoredTypes))
  }
  
  def setMonitoredTypes(monitorStr:String, monitoredTypeStr:String, used:Boolean) = Security.Authenticated{
    implicit request =>
      val monitor = Monitor.withName(monitorStr)
      val monitorType = MonitorType.withName(monitoredTypeStr)
      MonitoredType.setMonitoredType(monitor, monitorType, used)
      Ok("")
  }
  
  
  def monitorTypeConfig = Security.Authenticated{
    implicit request =>
    Ok(views.html.monitorTypeConfig())
  } 
  
  def recordValidation = Security.Authenticated{
    implicit request =>
    Ok(views.html.recordValidation())
  }
  
  def recordValidationHtml(startStr:String) = Security.Authenticated{
    implicit request =>
    val start = DateTime.parse(startStr)
    val nextDay = start + 1.day
    val end = 
      if(nextDay > DateTime.now)
        DateTime.now
      else
        nextDay
    
    
    val report = Record.getRecordValidationReport(start, end)
    
    Ok(views.html.recordValidationReport(start, end, report))
  }

  case class EpaRealtimeData(
    siteName:String,
    county:String, 
    psi:String,
    so2:String,
    co:String,
    o3:String,
    pm10:String,
    pm25:String,
    no2:String,
    windSpeed:String,
    windDir:String,
    publishTime:String
  )
  
  
  implicit val epaRealtimeDataRead:Reads[EpaRealtimeData] = 
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
     (__ \ "PublishTime").read[String]
    )(EpaRealtimeData.apply _)
    
  import play.api.libs.concurrent.Execution.Implicits.defaultContext
  def realtimeEpaRecord = Security.Authenticated.async{
    implicit request =>{
      val url = "http://opendata.epa.gov.tw/ws/Data/AQX/?$orderby=SiteName&$skip=0&$top=1000&format=json" 
        WS.url(url).get().map{
          response =>
            val epaData = response.json.validate[Seq[EpaRealtimeData]]
            epaData.fold(
                error=>{
                Logger.error(JsError.toFlatJson(error).toString())
                BadRequest(Json.obj("ok"->false, "msg"->JsError.toFlatJson(error)))
              },
              data=>{
                Logger.info("#="+data.length)
                Ok(views.html.epaRealtimeData(url, data))         
              }
            )
       }
    }
  }
  
  def userManagement() = Security.Authenticated{
    implicit request =>
    val userInfoOpt = Security.getUserinfo(request)
    if(userInfoOpt.isEmpty)
      Forbidden("No such user!")
    else{
      val userInfo = userInfoOpt.get
      Logger.info("id="+userInfo.id)
      val user = User.getUserById(userInfo.id).get
      val (userList, groupList) =
        if(!user.isAdmin)
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
  
  def deleteUser(id:Int) = Security.Authenticated{
    implicit request =>
    Logger.info("deleteUser")
    User.deleteUser(id)
    Ok(Json.obj("ok" -> true))
  }
  
  def getUser(id:Int) = Security.Authenticated{
    Ok("")
  }
    
  def updateUser(id:Int) = Security.Authenticated(BodyParsers.parse.json){
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
  
  
  def getAllUsers=Security.Authenticated{
    val users = User.getAllUsers()
    implicit val userWrites = Json.writes[User]
    
    Ok(Json.toJson(users))
  }
  
  def groupManagement() = Security.Authenticated{
    implicit request =>
    val userInfoOpt = Security.getUserinfo(request)
    if(userInfoOpt.isEmpty)
      Forbidden("No such user!")
    else{
      val userInfo = userInfoOpt.get
      if(!userInfo.isAdmin){
        Forbidden("Only administrator can manage group!")
      }else{
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
            Logger.debug(param.toString)
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
  
  def getAllGroups=Security.Authenticated{
    val groups = Group.getAllGroups()
    implicit val groupWrites = Json.writes[Group]
    
    Ok(Json.toJson(groups))
  }

  def manualAudit = Security.Authenticated {
    implicit request =>
      Ok(views.html.manualAudit(true))
  } 
  
  case class ManualAudit(monitor:Monitor.Value, monitorType:MonitorType.Value, time:Long, status:String)
  case class ManualAuditList(list:Seq[ManualAudit])
  def manualAuditApply()=Security.Authenticated(BodyParsers.parse.json){
    implicit request =>
      implicit val manualAuditReads = Json.reads[ManualAudit]
      implicit val manualAuditListReads = Json.reads[ManualAuditList]
      val manualAuditList = request.body.validate[ManualAuditList]
      Logger.info("manualAuditApply")
      manualAuditList.fold(
        error=>{
          Logger.error(JsError.toFlatJson(error).toString())
          BadRequest(Json.obj("ok"->false, "msg"->JsError.toFlatJson(error)))
        }, 
         manualAuditList=>{
           for(ma <- manualAuditList.list){
             Record.updateHourRecordStatus(ma.monitor, ma.monitorType, ma.time, ma.status)
           }
           
           Ok(Json.obj("ok"->true)) 
         })
  }
    
  def auditConfig()=Security.Authenticated{
    Ok(views.html.auditConfig())
  }
}
