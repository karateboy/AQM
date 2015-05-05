package controllers

import play.api._
import play.api.mvc._
import play.api.Logger
import models._
import models.Realtime._
import com.github.nscala_time.time.Imports._
import models.ModelHelper._
import play.api.libs.json._
import play.api.libs.json.Json


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
  

}
