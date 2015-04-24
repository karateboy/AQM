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

  case class MonitorTypeReport(monitorType:MonitorType.Value , dataList:List[Stat], stat:Stat)
  case class MonthlyReport(typeArray:Array[MonitorTypeReport])
  case class YearlyReport(typeArray:Array[MonitorTypeReport])
  
  def monitorReport = Security.Authenticated(BodyParsers.parse.json){
    implicit request =>
      Logger.info("monitorReport")
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
            Logger.debug(Monitor.map(monitor) + ":" + reportType + ":" + startTime)
            def getMonthlyReport()={
               val endTime = startTime + Period.months(1)
               def getDays(current:DateTime):List[DateTime]={
                 if(current == endTime)
                   Nil
                 else
                   current :: getDays(current + 1.days)
               }
               
               val days = getDays(startTime)
               val dailyReports =  
                 for{day <- days}yield{
                   HourRecord.getDailyReport(monitor, day)
                 }
                 
               def getTypeStat(i:Int)={
                 dailyReports.map { _.typeList(i).stat}
               }
               val typeReport = 
               for {t<-dailyReports(0).typeList
                 monitorType = t.monitorType
                 pos = dailyReports(0).typeList.indexWhere { x => x.monitorType == monitorType }
                 typeStat = getTypeStat(pos)
                 validData = typeStat.filter { _.count !=0 } 
                 count = validData.length
                 total = dailyReports.length
                 max = if (count != 0) validData.map(_.min).min else Float.MinValue
                 min = if (count != 0) validData.map(_.max).max else Float.MaxValue
                 avg = if (count != 0) validData.map(_.avg).sum/count else 0
                 overCount = validData.map(_.overCount).sum
               } yield{
                 MonitorTypeReport(monitorType, typeStat, Stat(avg, min, max, count, total, overCount) )
               }
               MonthlyReport(typeReport.toArray)
            }            
            
            
            val monitorName = Monitor.map(monitor)
            reportType match{
              case PeriodReport.DailyReport =>                
                val dailyReport = HourRecord.getDailyReport(monitor, startTime)
                Ok(views.html.dailyReport(monitorName, startTime, dailyReport))
              case PeriodReport.MonthlyReport =>
                val monthlyReport = getMonthlyReport()
                val nDays = monthlyReport.typeArray(0).dataList.length
                //val firstDay = new DateTime(startTime.year, startTime.month, 1)
                Ok(views.html.monthlyReport(monitorName, startTime, monthlyReport, nDays))
              case PeriodReport.YearlyReport =>
                Ok("")                
            }
          })
  }
  
  def monitorReportPDF(monitorStr: String, reportTypeStr: String, startDateStr:String) =
    Security.Authenticated { implicit request =>
    Logger.debug("monitorReportPDF")
    
    val monitor = Monitor.withName(monitorStr)
    val reportType = PeriodReport.withName(reportTypeStr)
    val startDate = DateTime.parse(startDateStr)
    if(reportType == PeriodReport.DailyReport){
      Logger.debug(reportType.toString())
      val dailyReport = HourRecord.getDailyReport(monitor, startDate)
      val output = views.html.reportTemplate(views.html.dailyReport(Monitor.map(monitor), startDate, dailyReport))
      Logger.info(output.toString())
      
      // Create a new Pdf converter with a custom configuration
      // run `wkhtmltopdf --extended-help` for a full list of options
      import io.github.cloudify.scala.spdf._
      import java.io._
      import java.net._
    
      val pdf = Pdf(new PdfConfig {
          orientation := Landscape
          pageSize := "Letter"
          marginTop := "1in"
          marginBottom := "1in"
          marginLeft := "1in"
          marginRight := "1in"
        })
      val outputStream = new ByteArrayOutputStream
      val page = <html><body><h1>Hello World</h1></body></html>
      
      val html = views.html.reportTemplate(views.html.dailyReport(Monitor.map(monitor), startDate, dailyReport))
      
      pdf.run(html.toString(), new File("google.pdf"))
      
      Ok("")
    }else
      Ok("")
  }
}



