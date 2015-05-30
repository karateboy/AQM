package controllers
import play.api._
import play.api.mvc._
import com.github.nscala_time.time.Imports._
import play.api.libs.json._
import play.api.libs.functional.syntax._
import models._
import java.util.Date
import play.api.Play.current
import play.api.db.DB

object PeriodReport extends Enumeration{
  val DailyReport = Value
  val MonthlyReport = Value
  val MonthlyHourReport = Value
  val YearlyReport = Value
}

object ReportType extends Enumeration{
  val MonitorReport = Value("monitorReport")
  val MonthlyHourReport = Value("MonthlyHourReport")
}

object Report extends Controller {
  
  def getReport(reportType: String) = Security.Authenticated { implicit request =>
    val MR = ReportType.MonitorReport.toString()
    val MHR = ReportType.MonthlyHourReport.toString()
    reportType match {
      case MR=>
        Ok(views.html.monitorReport(false))
      case MHR=>
        Ok(views.html.monthlyHourReportForm())
      case _=>
        BadRequest("未知的報表種類:" + reportType)
    }
  }

  def getDays(current:DateTime, endTime: DateTime): List[DateTime] = {
    if (current == endTime)
      Nil
    else
      current :: getDays(current + 1.days, endTime)
  }
   
  case class MonthHourReport(hourStatArray:Array[Stat], dailyReports:Array[DailyReport], StatStat:Stat)
  def monthlyHourReportHtml(monitorStr:String, monitorTypeStr:String, startDateStr:String) = Security.Authenticated{ implicit request =>
    Logger.debug("monthlyHourReportHtml()")
    val monitor = Monitor.withName(monitorStr)
    val monitorType = MonitorType.withName(monitorTypeStr)
    val startDate = DateTime.parse(startDateStr)
    val adjustStartDate = DateTime.parse(startDate.toString("YYYY-MM-1"))
    val endDate = startDate + Period.months(1)
    val days = getDays(startDate, endDate)
    val nDay = days.length
    val dailyReports =
      for { day <- days } yield {
        Record.getDailyReport(monitor, day, List(monitorType))
      }

    Logger.debug("dailyReports=" + dailyReports.length)
    
    def getHourRecord(i:Int) = {
      dailyReports.map { _.typeList(0).dataList(i) }
    }

    val monthHourStats =
      for {
        hour <- 0 to 23
        hourRecord = getHourRecord(hour)
        validData = hourRecord.filter {hr=>
          hr._3 match {
            case Some(stat)=> Record.isValidStat(stat)
            case _=>false
          }
        }.map(r => r._2.get)

        count = validData.length
        total = nDay
        max = if (count != 0) validData.max else Float.MinValue
        min = if (count != 0) validData.min else Float.MaxValue
        overCount = 0
      } yield {
        val avg = if(MonitorType.windDirList.contains(monitorType)){
            val sum_sin = validData.map(v=>Math.sin(Math.toRadians(v))).sum
            val sum_cos = validData.map(v=>Math.cos(Math.toRadians(v))).sum
            Math.toDegrees(Math.atan (sum_sin/sum_cos)).toFloat
          } else{
            val sum = validData.sum
            if (count != 0) sum / count else 0
          }
        
        Stat(avg, min, max, count, total, overCount)
      }
      Logger.debug("monthHourStats #=" + monthHourStats.length);

      val stats = monthHourStats.filter(t=>t.count !=0)
      val count = stats.map(_.count).sum
      val max = if(count != 0) stats.map {_.max}.max else Float.MinValue
      val min = if(count != 0) stats.map { _.min}.min else Float.MaxValue
      val total = stats.map{_.total}.sum
      val avg = if(MonitorType.windDirList.contains(monitorType)){
            val sum_sin = stats.map(v=>Math.sin(Math.toRadians(v.avg))).sum
            val sum_cos = stats.map(v=>Math.cos(Math.toRadians(v.avg))).sum
            Math.toDegrees(Math.atan (sum_sin/sum_cos)).toFloat
          } else{
            if(count != 0) stats.map {_.avg}.sum/count else 0
          }
      val result = MonthHourReport(monthHourStats.toArray, dailyReports.toArray, Stat(avg, min, max, count, total, 0))
      Logger.debug("result is ready!")
      
      Ok(views.html.monthlyHourReport(Monitor.map(monitor).name, startDate, result, nDay));
  }
  
  case class ReportInfo(monitor: String, reportType: String, startTime:String)
  implicit val readsUserInfo: Reads[ReportInfo] = 
    ((__ \ "monitor").read[String] and (__ \ "reportType").read[String]
    and (__ \ "startTime").read[String])(ReportInfo.apply _)

  case class MonitorTypeReport(monitorType:MonitorType.Value , dataList:List[Stat], stat:Stat)
  case class MonthlyReport(typeArray:Array[MonitorTypeReport])
  case class YearlyReport(typeArray:Array[MonitorTypeReport])

  def getMonthlyReport(monitor: Monitor.Value, startTime: DateTime, includeTypes:List[MonitorType.Value]=MonitorType.mtvList) = {
    val endTime = startTime + Period.months(1)
    val days = getDays(startTime, endTime)
    val dailyReports =
      for { day <- days } yield {
        Record.getDailyReport(monitor, day, includeTypes)
      }

    def getTypeStat(i: Int) = {
      dailyReports.map { _.typeList(i).stat }
    }
    val typeReport =
      for {
        t <- dailyReports(0).typeList
        monitorType = t.monitorType
        pos = dailyReports(0).typeList.indexWhere { x => x.monitorType == monitorType }
        typeStat = getTypeStat(pos)
        validData = typeStat.filter { _.count != 0 }
        count = validData.length
        total = dailyReports.length
        max = if (count != 0) validData.map(_.min).min else Float.MinValue
        min = if (count != 0) validData.map(_.max).max else Float.MaxValue
        overCount = validData.map(_.overCount).sum
      } yield {
        val avg = if(MonitorType.windDirList.contains(monitorType)){
            val sum_sin = validData.map(v=>Math.sin(Math.toRadians(v.avg))).sum
            val sum_cos = validData.map(v=>Math.cos(Math.toRadians(v.avg))).sum
            Math.toDegrees(Math.atan (sum_sin/sum_cos)).toFloat
          } else{
            if(count != 0) validData.map {_.avg}.sum/count else 0
          }
        MonitorTypeReport(monitorType, typeStat, Stat(avg, min, max, count, total, overCount))
      }
    MonthlyReport(typeReport.toArray)
  }            
  
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
            implicit val monitor = Monitor.withName(reportInfo.monitor)
            val reportType = PeriodReport.withName(reportInfo.reportType)
            val startTime = DateTime.parse(reportInfo.startTime)
            Logger.debug(Monitor.map(monitor) + ":" + reportType + ":" + startTime)
           
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
                   getMonthlyReport(monitor, month)
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
                 overCount = validData.map(_.overCount).sum
               } yield{
                val avg = if (MonitorType.windDirList.contains(monitorType)) {
                  val sum_sin = validData.map(v => Math.sin(Math.toRadians(v.avg))).sum
                  val sum_cos = validData.map(v => Math.cos(Math.toRadians(v.avg))).sum
                  Math.toDegrees(Math.atan(sum_sin / sum_cos)).toFloat
                } else {
                  if (count != 0) validData.map { _.avg }.sum / count else 0
                }
                 MonitorTypeReport(monitorType, typeStat, Stat(avg, min, max, count, total, overCount) )
               }
               YearlyReport(typeReport.toArray)
            }
            
            val monitorCase = Monitor.map(monitor)
            reportType match{
              case PeriodReport.DailyReport =>                
                val dailyReport = Record.getDailyReport(monitor, startTime)
                Ok(views.html.dailyReport(monitor, startTime, dailyReport))
              case PeriodReport.MonthlyReport =>
                val adjustStartDate = DateTime.parse(startTime.toString("YYYY-MM-1"))
                val monthlyReport = getMonthlyReport(monitor, adjustStartDate)
                val nDays = monthlyReport.typeArray(0).dataList.length
                //val firstDay = new DateTime(startTime.year, startTime.month, 1)
                Ok(views.html.monthlyReport(monitorCase.name, startTime, monthlyReport, nDays))
              case PeriodReport.MonthlyHourReport=>
                val adjustStartDate = DateTime.parse(startTime.toString("YYYY-MM-1"))
                val monthlyReport = getMonthlyReport(monitor, adjustStartDate)
                val nDays = monthlyReport.typeArray(0).dataList.length
                Ok("")
              case PeriodReport.YearlyReport =>
                val adjustStartDate = DateTime.parse(startTime.toString("YYYY-1-1"))
                val yearlyReport = getYearlyReport(adjustStartDate)
                //val firstDay = new DateTime(startTime.year, startTime.month, 1)
                Ok(views.html.yearlyReport(monitorCase.name, startTime, yearlyReport))
              
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
      val dailyReport = Record.getDailyReport(monitor, startDate) 
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
      val dailyReport = Record.getDailyReport(monitor, startDate)
      val output = views.html.reportTemplate(views.html.dailyReport(monitor, startDate, dailyReport))
      
      val url = "http://" + request.host + routes.Report.monitorReportHtml(monitorStr, reportTypeStr, startDateStr).toString()
      Logger.info("url=" + url)
      Ok.sendFile(sendPDF(url),  fileName= _=>"Daily_"+monitor+startDate.toString("YYYYMMDD")+".pdf")
    }else
      Ok("")
  }
  
  def psiReportPrompt() = Security.Authenticated { 
    implicit request =>
      Ok(views.html.psiReport())
  }
  
  def psiReportReport(monitorStr:String, reportTypeStr:String, startDateStr:String) = Security.Authenticated {
    implicit request =>
      import models.Realtime._

      val monitor = Monitor.withName(monitorStr)
      val reportType = PeriodReport.withName(reportTypeStr)
      val startDate = DateTime.parse(startDateStr)
      reportType match {
        case PeriodReport.DailyReport=>
          val psiList = getDailyPSI(monitor, startDate)
          Ok(views.html.psiDailyReport(monitor, startDate, psiList))
        case PeriodReport.MonthlyReport=>
          Ok("")
        case PeriodReport.YearlyReport=>
          Ok("")
      }
      
  }
  
}



