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
      val userID = Security.getUserinfo(request)
      if(userID.isEmpty)
        Ok(views.html.index(title, ""))
      else
        Ok(views.html.index(title, userID.get))
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

  def calibrationQueryForm = Security.Authenticated {
    implicit request =>
      Logger.info("calibrationQueryForm")
      Ok(views.html.calibration())
  }
  
  def calibrationQueryResult(monitorStr:String, startStr:String, endStr:String) = Security.Authenticated {
    implicit request =>
      Logger.info("calibrationQueryResult")
      val monitor = Monitor.withName(monitorStr)
      val start = DateTime.parse(startStr)
      val end = DateTime.parse(endStr)
      val result = Calibration.calibrationQueryReport(monitor, start, end)
      Ok(views.html.calibrationQueryResult(result))
  }
  
}
