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
  val DailyReport = Value
  val MonthlyReport = Value
  val YearlyReport = Value
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
            def getMonthlyReport(startTime:DateTime)={
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
            
            def getYearlyReport(startTime:DateTime)={
               val endTime = startTime + Period.years(1)
               def getMonths(current:DateTime):List[DateTime]={
                 if(current == endTime)
                   Nil
                 else
                   current :: getMonths(current + 1.months)
               }
               
               val monthes = getMonths(startTime)
               val monthlyReports =  
                 for{month <- monthes}yield{
                   getMonthlyReport(month)
                 }
                 
               def getTypeStat(i:Int)={
                 monthlyReports.map { _.typeArray(i).stat}
               }
               
               val typeReport = 
               for {t<-monthlyReports(0).typeArray
                 monitorType = t.monitorType
                 pos = monthlyReports(0).typeArray.indexWhere { x => x.monitorType == monitorType }
                 typeStat = getTypeStat(pos)
                 validData = typeStat.filter { _.count !=0 } 
                 count = validData.length
                 total = monthlyReports.length
                 max = if (count != 0) validData.map(_.min).min else Float.MinValue
                 min = if (count != 0) validData.map(_.max).max else Float.MaxValue
                 avg = if (count != 0) validData.map(_.avg).sum/count else 0
                 overCount = validData.map(_.overCount).sum
               } yield{
                 MonitorTypeReport(monitorType, typeStat, Stat(avg, min, max, count, total, overCount) )
               }
               YearlyReport(typeReport.toArray)
            }
            
            val monitorName = Monitor.map(monitor)
            reportType match{
              case PeriodReport.DailyReport =>                
                val dailyReport = HourRecord.getDailyReport(monitor, startTime)
                Ok(views.html.dailyReport(monitor, startTime, dailyReport))
              case PeriodReport.MonthlyReport =>
                val adjustStartDate = DateTime.parse(startTime.toString("YYYY-MM-1"))
                val monthlyReport = getMonthlyReport(adjustStartDate)
                val nDays = monthlyReport.typeArray(0).dataList.length
                //val firstDay = new DateTime(startTime.year, startTime.month, 1)
                Ok(views.html.monthlyReport(monitorName, startTime, monthlyReport, nDays))
              case PeriodReport.YearlyReport =>
                val adjustStartDate = DateTime.parse(startTime.toString("YYYY-1-1"))
                val yearlyReport = getYearlyReport(adjustStartDate)
                //val firstDay = new DateTime(startTime.year, startTime.month, 1)
                Ok(views.html.yearlyReport(monitorName, startTime, yearlyReport))
                
            }
          })
  }
  
  def monitorReportHtml(monitorStr: String, reportTypeStr: String, startDateStr:String) =
    Action { implicit request =>
    Logger.debug("monitorReportPDF")
    
    val monitor = Monitor.withName(monitorStr)
    val reportType = PeriodReport.withName(reportTypeStr)
    val startDate = DateTime.parse(startDateStr)
    
    if(reportType == PeriodReport.DailyReport){
      val dailyReport = HourRecord.getDailyReport(monitor, startDate) 
      Ok(views.html.reportTemplate(views.html.dailyReport(monitor, startDate, dailyReport)))
    }else
      Ok("")
  }
  
  def monitorReportPDF(monitorStr: String, reportTypeStr: String, startDateStr:String) =
    Security.Authenticated { implicit request =>
    Logger.debug("monitorReportPDF")
    
    val monitor = Monitor.withName(monitorStr)
    val reportType = PeriodReport.withName(reportTypeStr)
    val startDate = DateTime.parse(startDateStr)
    import play.api.templates._
    def sendPDF(url:String) = {
      import io.github.cloudify.scala.spdf._
      import java.io._
      import java.net._
    
      val pdf = Pdf("C:/Program Files/wkhtmltopdf/bin/wkhtmltopdf.exe", new PdfConfig {
          orientation := Landscape
          pageSize := "A4"
        })
              
      val tempReportFile = File.createTempFile("report", ".pdf")
      pdf.run(new URL(url), tempReportFile)

      tempReportFile
    }
    
    if(reportType == PeriodReport.DailyReport){
      val dailyReport = HourRecord.getDailyReport(monitor, startDate)
      val output = views.html.reportTemplate(views.html.dailyReport(monitor, startDate, dailyReport))
      
      val url = "http://" + request.host + routes.Report.monitorReportHtml(monitorStr, reportTypeStr, startDateStr).toString()
      Logger.info("url=" + url)
      Ok.sendFile(sendPDF(url),  fileName= _=>"Daily_"+monitor+startDate.toString("YYYYMMDD")+".pdf")
    }else
      Ok("")
  }
}



