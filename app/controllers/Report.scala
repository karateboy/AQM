package controllers
import play.api._
import play.api.mvc._
import com.github.nscala_time.time.Imports._
import play.api.libs.json._
import play.api.libs.functional.syntax._
import models._

/**
 * @author user
 */

import java.util.Date
import play.api.Play.current
import play.api.db.DB
object PeriodReport extends Enumeration{
  val DailyReport = Value("dailyReport")
  val MonthlyReport = Value("monthlyReport")
  val YearlyReport = Value("yearlyReport")
}

object ReportType extends Enumeration{
  val MonitorReport = Value("monitorReport")
}

object Report extends Controller {
  
  def getReport(reportType: String) = Action {
    if(reportType == ReportType.MonitorReport.toString())
      Ok(views.html.monitorReport(Application.title))
    else
      BadRequest("未知的報表種類:" + reportType)
  }
  
  //def db(implicit app: play.api.Application) = 
  //  Database.f
  def periodReport= Action {
    implicit request =>
      
      DB.withConnection("aqmdata") { conn =>  
      }
      Ok("")
  }
  
  case class ReportInfo(monitor: String, reportType: String, startTime:String)
  implicit val readsUserInfo: Reads[ReportInfo] = 
    ((__ \ "monitor").read[String] and (__ \ "reportType").read[String]
    and (__ \ "startTime").read[String])(ReportInfo.apply _)

  def monitorReport = Action(BodyParsers.parse.json){
    implicit request =>
      val reportInfo = request.body.validate[ReportInfo]      
      reportInfo.fold(
          error=>{
            Logger.error(JsError.toFlatJson(error).toString())
            BadRequest(Json.obj("ok"->false, "msg"->JsError.toFlatJson(error)))
          }, 
          reportInfo=>{
            val monitor = Monitor.withName(reportInfo.monitor)
            val reportType = PeriodReport.withName(reportInfo.reportType)
            val startTime = DateTime.parse(reportInfo.startTime)
            if(reportType == PeriodReport.DailyReport){
              val dailyReport = MinRecord.getDailyReport(monitor, startTime)
              Ok(Json.obj("ok"->true))
            }else
              Ok(Json.obj("ok"->true))
          })
  }
}



