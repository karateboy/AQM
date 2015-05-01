package controllers

import play.api._
import play.api.mvc._
import play.api.Logger
import models._
import models.Realtime._
import com.github.nscala_time.time.Imports._
import models.ModelHelper._

object Application extends Controller {

  val title = "麥寮廠區空氣品質及氣象監測系統"
  
  def index = Security.Authenticated{
    implicit request =>
    Ok(views.html.index(title))
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
  
  def realtimeStat = Security.Authenticated{
    implicit request =>
      val rt_status = Realtime.getRealtimeStatusHour()
      val rt_psi = Realtime.getRealtimePSI
      Ok(views.html.realtimeStatus(rt_status, MonitorType.psiList, rt_psi))
  } 
  
  def realtimeImg = Security.Authenticated{
    implicit request =>
    Ok(views.html.realtimeImage(""))
  } 
  
import play.api.libs.json._
import play.api.libs.functional.syntax._

  case class WeatherStat(monitor:String, windDir:Float, windSpeed:Float)
  implicit val weatherStatWrites: Writes[WeatherStat] = (
  (JsPath \ "name").write[String] and
  (JsPath \ "windDir").write[Float] and
  (JsPath \ "windSpeed").write[Float]
)(unlift(WeatherStat.unapply))

  def realtimeWeather = Security.Authenticated{
    implicit request =>
    Ok(views.html.realtimeImage(""))
  } 

  case class RealtimeTrendParam(monitors:List[Monitor.Value], monitorTypes:List[MonitorType.Value])
  //implicit val readsUserInfo: Reads[RealtimeTrendParam] = 
  //  ((__ \ "monitor").read[String] and (__ \ "reportType").read[String]
  //  and (__ \ "startTime").read[String])(RealtimeTrendParam.apply _)

  def realtimeTrend = Security.Authenticated{
    implicit request =>
    Ok(views.html.realtimeTrend(""))
  } 
}
