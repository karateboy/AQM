package controllers
import play.api._
import play.api.mvc._
import play.api.Logger
import models._
import com.github.nscala_time.time.Imports._
import models.ModelHelper._
import play.api.libs.json._
import play.api.libs.functional.syntax._

object Query extends Controller{

  def history() = Security.Authenticated {
    implicit request =>
      
    Ok(views.html.history())
  }
  
  def historyReport(monitorStr:String, monitorTypeStr:String, startStr:String, endStr:String)=Security.Authenticated {
    implicit request =>
    val monitor = Monitor.withName(monitorStr)
    val monitorType = MonitorType.withName(monitorTypeStr)
    val start = DateTime.parse(startStr)
    val end = DateTime.parse(endStr)
    
    val records = Record.getHourRecords(monitor, start, end)
    val mtRecords = records.map {rs=>(Record.timeProjection(rs).toDateTime, Record.monitorTypeProject2(monitorType)(rs))}
    
    Ok(views.html.historyReport(monitor, monitorType, start, end, mtRecords))
  }
  
  def trend = Security.Authenticated {
    Ok(views.html.trendReport())
  }

}