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
import PdfUtility._
import java.io.File
import java.nio.file.Files
import Record.windAvg

object PeriodReport extends Enumeration {
  val DailyReport = Value
  val MonthlyReport = Value
  val MonthlyHourReport = Value
  val YearlyReport = Value
}

object ReportType extends Enumeration {
  val MonitorReport = Value("monitorReport")
  val MonthlyHourReport = Value("MonthlyHourReport")
}

object OutputType extends Enumeration {
  val html = Value("html")
  val pdf = Value("pdf")
  val excel = Value("excel")
}

object Report extends Controller {

  def getReport(reportTypeStr: String) = Security.Authenticated { implicit request =>
    val userInfo = Security.getUserinfo(request).get
    val group = Group.getGroup(userInfo.groupID).get
    val reportType = ReportType.withName(reportTypeStr)
    reportType match {
      case ReportType.MonitorReport =>
        Ok(views.html.monitorReport(group.privilege))
      case ReportType.MonthlyHourReport =>
        Ok(views.html.monthlyHourReportForm(group.privilege))
      case _ =>
        BadRequest("未知的報表種類:" + reportType)
    }
  }

  def getDays(current: DateTime, endTime: DateTime): List[DateTime] = {
    if (current >= endTime)
      Nil
    else
      current :: getDays(current + 1.days, endTime)
  }

  def getWeeks(current: DateTime, endTime: DateTime): List[DateTime] = {
    assert(endTime.getDayOfWeek == 7)
    if (current == endTime)
      Nil
    else
      current :: getWeeks(current + 1.week, endTime)
  }

  def getMonths(current: DateTime, endTime: DateTime): List[DateTime] = {
    assert(endTime.getDayOfMonth == 1)

    if (current == endTime)
      Nil
    else
      current :: getMonths(current + 1.month, endTime)
  }
  
  def getQuarters(current: DateTime, endTime: DateTime): List[DateTime] = {
    assert(endTime.getDayOfMonth == 1)

    if (current >= endTime)
      Nil
    else
      current :: getQuarters(current + 3.month, endTime)
  }

  def monthlyHourReportHelper(monitor: Monitor.Value, startDate: DateTime) = {
    val endDate = startDate + Period.months(1)
    val days = getDays(startDate, endDate)
    val nDay = days.length
    val dailyReports =
      for { day <- days } yield {
        Record.getDailyReport(monitor, day, MonitorType.monitorReportList)
      }

    def getHourRecord(i: Int, j: Int) = {
      dailyReports.map { _.typeList(i).dataList(j) }
    }

    val monthHourStats =
      for {
        (mt, mt_i) <- MonitorType.monitorReportList.zipWithIndex
        hour <- 0 to 23
        hourRecord = getHourRecord(mt_i, hour)
        validData = hourRecord.filter { hr =>
          hr._3 match {
            case Some(stat) => MonitorStatus.isNormalStat(stat)
            case _          => false
          }
        }.map(r => r._2.get)

        count = validData.length
        total = nDay
        max = if (count != 0) validData.max else Float.MinValue
        min = if (count != 0) validData.min else Float.MaxValue
        overCount = 0
      } yield {
        val avg = if (MonitorType.windDirList.contains(mt)) {         
          val sum_sin = validData.map(v => Math.sin(Math.toRadians(v))).sum
          val sum_cos = validData.map(v => Math.cos(Math.toRadians(v))).sum
          windAvg(sum_sin, sum_cos)
         } else {
          val sum = validData.sum
          if (count != 0) sum / count else 0
        }

        Stat(avg, min, max, count, total, overCount)
      }

    val stats = monthHourStats.filter(t => t.count != 0)
    val count = stats.map(_.count).sum
    val max = if (count != 0) stats.map { _.max }.max else Float.MinValue
    val min = if (count != 0) stats.map { _.min }.min else Float.MaxValue
    val total = stats.map { _.total }.sum
    val avg = {
      if (count != 0) stats.map { _.avg }.sum / count else 0
    }
    MonthHourReport(monthHourStats.toArray, dailyReports.toArray, Stat(avg, min, max, count, total, 0))
  }

  case class MonthHourReport(hourStatArray: Array[Stat], dailyReports: Array[DailyReport], StatStat: Stat)
  def monthlyHourReport(monitorStr: String, monitorTypeStr: String, startDateStr: String, outputTypeStr: String) = Security.Authenticated { implicit request =>
    val monitor = Monitor.withName(monitorStr)
    val monitorType = MonitorType.withName(monitorTypeStr)
    val startDate = DateTime.parse(startDateStr)
    val adjustStartDate = DateTime.parse(startDate.toString("YYYY-MM-1"))
    val outputType = OutputType.withName(outputTypeStr)

    val endDate = startDate + Period.months(1)
    val days = getDays(startDate, endDate)
    val nDay = days.length
    if (outputType == OutputType.excel) {
      import java.io.File
      import java.nio.file.Files
      val monthlyReport = getMonthlyReport(monitor, adjustStartDate)

      val (title, excelFile) = ("月報", ExcelUtility.createMonthlyReport(monitor, adjustStartDate, monthlyReport, nDay))
      Ok.sendFile(excelFile, fileName = _ =>
        play.utils.UriEncoding.encodePathSegment(Monitor.map(monitor).name + title + adjustStartDate.toString("YYYYMMdd") + ".xlsx", "UTF-8"),
        onClose = () => { Files.deleteIfExists(excelFile.toPath()) })
    } else {
      val dailyReports =
        for { day <- days } yield {
          Record.getDailyReport(monitor, day, List(monitorType))
        }

      def getHourRecord(i: Int) = {
        dailyReports.map { _.typeList(0).dataList(i) }
      }

      val monthHourStats =
        for {
          hour <- 0 to 23
          hourRecord = getHourRecord(hour)
          validData = hourRecord.filter { hr =>
            hr._3 match {
              case Some(stat) => MonitorStatus.isNormalStat(stat)
              case _          => false
            }
          }.map(r => r._2.get)

          count = validData.length
          total = nDay
          max = if (count != 0) validData.max else Float.MinValue
          min = if (count != 0) validData.min else Float.MaxValue
          overCount = 0
        } yield {
          val avg = if (MonitorType.windDirList.contains(monitorType)) {
            val sum_sin = validData.map(v => Math.sin(Math.toRadians(v))).sum
            val sum_cos = validData.map(v => Math.cos(Math.toRadians(v))).sum
            windAvg(sum_sin, sum_cos)
          } else {
            val sum = validData.sum
            if (count != 0) sum / count else 0
          }

          Stat(avg, min, max, count, total, overCount)
        }

      val stats = monthHourStats.filter(t => t.count != 0)
      val count = stats.map(_.count).sum
      val max = if (count != 0) stats.map { _.max }.max else Float.MinValue
      val min = if (count != 0) stats.map { _.min }.min else Float.MaxValue
      val total = stats.map { _.total }.sum
      val avg = if (MonitorType.windDirList.contains(monitorType)) {
        val sum_sin = stats.map(v => Math.sin(Math.toRadians(v.avg))).sum
        val sum_cos = stats.map(v => Math.cos(Math.toRadians(v.avg))).sum
        windAvg(sum_sin, sum_cos)
      } else {
        if (count != 0) stats.map { _.avg }.sum / count else 0
      }
      val result = MonthHourReport(monthHourStats.toArray, dailyReports.toArray, Stat(avg, min, max, count, total, 0))

      val output = views.html.monthlyHourReport(Monitor.map(monitor).name, startDate, result, nDay)
      val title = "月份時報表"
      outputType match {
        case OutputType.html =>
          Ok(output)
        case OutputType.pdf =>
          Ok.sendFile(creatPdfWithReportHeader(title, output),
            fileName = _ =>
              play.utils.UriEncoding.encodePathSegment(Monitor.map(monitor).name + title + startDate.toString("YYYYMM") + ".pdf", "UTF-8"))
      }

    }
  }

  case class ReportInfo(monitor: String, reportType: String, startTime: String)
  implicit val readsUserInfo: Reads[ReportInfo] =
    ((__ \ "monitor").read[String] and (__ \ "reportType").read[String]
      and (__ \ "startTime").read[String])(ReportInfo.apply _)

  case class MonitorTypeReport(monitorType: MonitorType.Value, dataList: List[Stat], stat: Stat)
  case class IntervalReport(typeArray: Array[MonitorTypeReport])

  case class MonthlyReport(typeArray: Array[MonitorTypeReport])

  def getWeeklyReport(monitor: Monitor.Value, startTime: DateTime, includeTypes: List[MonitorType.Value] = MonitorType.monitorReportList,
                      filter: MonitorStatusFilter.Value = MonitorStatusFilter.All) = {
    val endTime = startTime + 1.weeks
    val days = getDays(startTime, endTime)
    val dailyReports =
      for { day <- days } yield {
        Record.getDailyReport(monitor, day, includeTypes, filter)
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
        val avg = if (MonitorType.windDirList.contains(monitorType)) {
          val sum_sin = validData.map(v => Math.sin(Math.toRadians(v.avg))).sum
          val sum_cos = validData.map(v => Math.cos(Math.toRadians(v.avg))).sum
          windAvg(sum_sin, sum_cos)
        } else {
          if (count != 0) validData.map { _.avg }.sum / count else 0
        }
        MonitorTypeReport(monitorType, typeStat, Stat(avg, min, max, count, total, overCount))
      }
    IntervalReport(typeReport.toArray)
  }

  def getMonthlyReport(monitor: Monitor.Value, startTime: DateTime, includeTypes: List[MonitorType.Value] = MonitorType.monitorReportList,
                       filter: MonitorStatusFilter.Value = MonitorStatusFilter.Normal) = {
    val endTime = startTime + Period.months(1)
    val days = getDays(startTime, endTime)
    val dailyReports =
      for { day <- days } yield {
        Record.getDailyReport(monitor, day, includeTypes, filter)
      }

    def getTypeStat(i: Int) = {
      dailyReports.map { _.typeList(i).stat }
    }
    val typeReport =
      for {
        (monitorType, pos) <- includeTypes.zipWithIndex
        typeStat = getTypeStat(pos)
        validData = typeStat.filter { _.count != 0 }
        count = validData.length
        total = dailyReports.length
        max = if (count != 0) validData.map(_.avg).max else Float.MinValue
        min = if (count != 0) validData.map(_.avg).min else Float.MaxValue
        overCount = validData.map(_.overCount).sum
      } yield {
        val avg = if (MonitorType.windDirList.contains(monitorType)) {
          val sum_sin = validData.map(v => Math.sin(Math.toRadians(v.avg))).sum
          val sum_cos = validData.map(v => Math.cos(Math.toRadians(v.avg))).sum
          windAvg(sum_sin, sum_cos)
        } else {
          if (count != 0) validData.map { _.avg }.sum / count else 0
        }
        MonitorTypeReport(monitorType, typeStat, Stat(avg, min, max, count, total, overCount))
      }
    MonthlyReport(typeReport.toArray)
  }

  def getPeriodReport(monitor: Monitor.Value, startTime: DateTime, period: Period, includeTypes: List[MonitorType.Value] = MonitorType.monitorReportList,
                      filter: MonitorStatusFilter.Value = MonitorStatusFilter.All) = {
    val endTime = startTime + period
    val report = Record.getPeriodReport(monitor, startTime, period, includeTypes, filter)
    
    val typeReport =
      for {
        t <- report.typeList.zipWithIndex
        monitorType = t._1.monitorType
        pos = t._2
        typeStat = report.typeList(pos).stat
      } yield {
        MonitorTypeReport(monitorType, List(typeStat), typeStat)
      }      
    IntervalReport(typeReport)
  }

  def adjustWeekDay(date: DateTime) = {
    import org.joda.time.DateTimeConstants._
    if (date.getDayOfWeek == SUNDAY)
      date
    else
      date - (date.getDayOfWeek).days
  }

  def getPeriods(start: DateTime, endTime: DateTime, d: Period): List[DateTime] = {
    import scala.collection.mutable.ListBuffer
    
    val buf = ListBuffer[DateTime]()
    var current = start
    while(current < endTime){
      buf.append(current)
      current += d
    }
    
    buf.toList
   }

  def getPeriodReportMap(monitor: Monitor.Value, start: DateTime, end: DateTime, filter: MonitorStatusFilter.Value, period: Period) = {
    val adjustStart = DateTime.parse(start.toString("YYYY-MM-dd"))
    val adjustEnd = DateTime.parse(end.toString("YYYY-MM-dd")) + 1.day
    val periods = getPeriods(adjustStart, adjustEnd, period)
    val nPeriod = periods.length
    val periodReports =
      for { p <- periods } yield {
        (p -> getPeriodReport(monitor, p, period, MonitorType.mtvAllList, filter))
      }

    import scala.collection.mutable.Map
    val map = Map.empty[MonitorType.Value, Map[DateTime, (Option[Float], Option[String])]]

    for {
      (time, report) <- periodReports
      t <- report.typeArray
    } {
      val periodMap = map.getOrElse(t.monitorType, Map.empty[DateTime, (Option[Float], Option[String])])
      periodMap.put(time, (Some(t.stat.avg), Some(MonitorStatusFilter.statusMap(filter))))
      map.put(t.monitorType, periodMap)
    }
    map
  }

  def getWeeklyReportMap(monitor: Monitor.Value, start: DateTime, end: DateTime, filter: MonitorStatusFilter.Value) = {
    import org.joda.time.DateTimeConstants._
    val adjustStart = DateTime.parse(adjustWeekDay(start).toString("YYYY-MM-dd"))

    val adjustEnd = DateTime.parse(adjustWeekDay(end).toString("YYYY-MM-dd")) + 1.weeks

    def getWeeks(current: DateTime): List[DateTime] = {
      if (current == adjustEnd)
        Nil
      else
        current :: getWeeks(current + 1.weeks)
    }

    val weeks = getWeeks(adjustStart)
    val weeklyReports =
      for { week <- weeks } yield {
        (week, getPeriodReport(monitor, week, 1.weeks, MonitorType.mtvAllList, filter))
      }

    import scala.collection.mutable.Map
    val map = Map.empty[MonitorType.Value, Map[DateTime, (Option[Float], Option[String])]]

    for {
      (week, report) <- weeklyReports
      t <- report.typeArray
    } {
      val dateMap = map.getOrElse(t.monitorType, Map.empty[DateTime, (Option[Float], Option[String])])
      dateMap.put(week, (Some(t.stat.avg), Some(MonitorStatusFilter.statusMap(filter))))
      map.put(t.monitorType, dateMap)
    }
    map
  }

  def getMonthlyReportMap(monitor: Monitor.Value, start: DateTime, end: DateTime, filter: MonitorStatusFilter.Value) = {
    val adjustStart = DateTime.parse(start.toString("YYYY-MM-01"))
    val adjustEnd = DateTime.parse(end.toString("YYYY-MM-01")) + 1.month

    def getMonths(current: DateTime): List[DateTime] = {
      if (current == adjustEnd)
        Nil
      else
        current :: getMonths(current + 1.months)
    }

    val monthes = getMonths(adjustStart)
    val monthlyReports =
      for { month <- monthes } yield {
        (month, getMonthlyReport(monitor, month, MonitorType.mtvAllList, filter))
      }

    import scala.collection.mutable.Map
    val map = Map.empty[MonitorType.Value, Map[DateTime, (Option[Float], Option[String])]]

    for {
      (month, report) <- monthlyReports
      t <- report.typeArray
    } {
      val dateMap = map.getOrElse(t.monitorType, Map.empty[DateTime, (Option[Float], Option[String])])
      dateMap.put(month, (Some(t.stat.avg), Some(MonitorStatusFilter.statusMap(filter))))
      map.put(t.monitorType, dateMap)
    }
    map
  }

  def getQuarterReportMap(monitor: Monitor.Value, start: DateTime, end: DateTime, filter: MonitorStatusFilter.Value) = {
    val adjustStart = DateTime.parse(s"${start.getYear}-${1 + (start.getMonthOfYear - 1) / 3 * 3}-01")
    val adjustEnd = DateTime.parse(s"${end.getYear}-${1 + (end.getMonthOfYear - 1) / 3 * 3}-01") + 3.month

    def getQuarters(current: DateTime): List[DateTime] = {
      if (current == adjustEnd)
        Nil
      else
        current :: getQuarters(current + 3.months)
    }

    val quarters = getQuarters(adjustStart)

    val quarterReports =
      for { quarter <- quarters } yield {
        (quarter, getPeriodReport(monitor, quarter, Period.months(3), MonitorType.mtvAllList, filter))
      }

    import scala.collection.mutable.Map
    val map = Map.empty[MonitorType.Value, Map[DateTime, (Option[Float], Option[String])]]

    for {
      (quarter, report) <- quarterReports
      t <- report.typeArray
    } {
      val dateMap = map.getOrElse(t.monitorType, Map.empty[DateTime, (Option[Float], Option[String])])
      dateMap.put(quarter, (Some(t.stat.avg), Some(MonitorStatusFilter.statusMap(filter))))
      map.put(t.monitorType, dateMap)
    }
    map
  }

  def monitorReport(monitorStr: String, reportTypeStr: String, startDateStr: String, outputTypeStr: String) = Security.Authenticated {
    implicit request =>
      val monitor = Monitor.withName(monitorStr)
      val reportType = PeriodReport.withName(reportTypeStr)
      val startTime = DateTime.parse(startDateStr)
      val outputType = OutputType.withName(outputTypeStr)

      def getYearlyReport(startTime: DateTime) = {
        val endTime = startTime + Period.years(1)
        def getMonths(current: DateTime): List[DateTime] = {
          if (current == endTime)
            Nil
          else
            current :: getMonths(current + 1.months)
        }

        val monthes = getMonths(startTime)
        val monthlyReports =
          for { month <- monthes } yield {
            getMonthlyReport(monitor, month)
          }

        def getTypeStat(i: Int) = {
          monthlyReports.map { _.typeArray(i).stat }
        }

        val typeReport =
          for {
            (monitorType, pos) <- MonitorType.monitorReportList.zipWithIndex
            typeStat = getTypeStat(pos)
            validData = typeStat.filter { _.count != 0 }
            count = validData.length
            total = monthlyReports.length
            max = if (count != 0) validData.map(_.avg).max else Float.MinValue
            min = if (count != 0) validData.map(_.avg).min else Float.MaxValue
            overCount = validData.map(_.overCount).sum
          } yield {
            val avg = if (MonitorType.windDirList.contains(monitorType)) {
              val sum_sin = validData.map(v => Math.sin(Math.toRadians(v.avg))).sum
              val sum_cos = validData.map(v => Math.cos(Math.toRadians(v.avg))).sum
              windAvg(sum_sin, sum_cos)
            } else {
              if (count != 0) validData.map { _.avg }.sum / count else 0
            }
            MonitorTypeReport(monitorType, typeStat, Stat(avg, min, max, count, total, overCount))
          }
        IntervalReport(typeReport.toArray)
      }

      if (outputType == OutputType.html || outputType == OutputType.pdf) {
        val monitorCase = Monitor.map(monitor)
        val (title, output) =
          reportType match {
            case PeriodReport.DailyReport =>
              val dailyReport = Record.getDailyReport(monitor, startTime)
              ("日報", views.html.dailyReport(monitor, startTime, dailyReport))

            case PeriodReport.MonthlyReport =>
              val adjustStartDate = DateTime.parse(startTime.toString("YYYY-MM-1"))
              val monthlyReport = getMonthlyReport(monitor, adjustStartDate)
              val nDays = monthlyReport.typeArray(0).dataList.length
              ("月報", views.html.monthlyReport(monitorCase.name, startTime, monthlyReport, nDays))

            case PeriodReport.YearlyReport =>
              val adjustStartDate = DateTime.parse(startTime.toString("YYYY-1-1"))
              val yearlyReport = getYearlyReport(adjustStartDate)
              ("年報", views.html.yearlyReport(monitorCase.name, startTime, yearlyReport))
          }

        outputType match {
          case OutputType.html =>
            Ok(output)
          case OutputType.pdf =>
            Ok.sendFile(creatPdfWithReportHeader(title, output),
              fileName = _ =>
                play.utils.UriEncoding.encodePathSegment(Monitor.map(monitor).name + title + startTime.toString("YYYYMMdd") + ".pdf", "UTF-8"))
        }
      } else {
        import java.io.File
        import java.nio.file.Files
        val (title, excelFile) =
          reportType match {
            case PeriodReport.DailyReport =>
              val dailyReport = Record.getDailyReport(monitor, startTime)
              ("日報", ExcelUtility.createDailyReport(monitor, startTime, dailyReport))

            case PeriodReport.MonthlyReport =>
              val adjustStartDate = DateTime.parse(startTime.toString("YYYY-MM-1"))
              val monthlyReport = getMonthlyReport(monitor, adjustStartDate)
              val nDay = monthlyReport.typeArray(0).dataList.length
              ("月報", ExcelUtility.createMonthlyReport(monitor, adjustStartDate, monthlyReport, nDay))

            case PeriodReport.YearlyReport =>
              val adjustStartDate = DateTime.parse(startTime.toString("YYYY-1-1"))
              val yearlyReport = getYearlyReport(adjustStartDate)

              ("年報", ExcelUtility.createYearlyReport(monitor, adjustStartDate, yearlyReport))
          }

        Ok.sendFile(excelFile, fileName = _ =>
          play.utils.UriEncoding.encodePathSegment(Monitor.map(monitor).name + title + startTime.toString("YYYYMMdd") + ".xlsx", "UTF-8"),
          onClose = () => { Files.deleteIfExists(excelFile.toPath()) })
      }
  }

  def psiReportPrompt() = Security.Authenticated {
    implicit request =>
      val userInfo = Security.getUserinfo(request).get
      val group = Group.getGroup(userInfo.groupID).get
      Ok(views.html.psiReport(group.privilege))
  }

  def psiReportReport(monitorStr: String, reportTypeStr: String, startDateStr: String, outputTypeStr: String) = Security.Authenticated {
    implicit request =>
      import models.Realtime._

      val monitor = Monitor.withName(monitorStr)
      val reportType = PeriodReport.withName(reportTypeStr)
      val startDate = DateTime.parse(startDateStr)
      val outputType = OutputType.withName(outputTypeStr)
      if (outputType == OutputType.excel) {
        val (title, excelFile) =
          reportType match {
            case PeriodReport.DailyReport =>
              val psiList = getDailyPsiReport(monitor, startDate)
              ("PSI日報表", ExcelUtility.psiDailyReport(monitor, startDate, psiList))
            
            case PeriodReport.MonthlyReport =>
              val adjustStartDate = DateTime.parse(startDate.toString("YYYY-MM-1"))
              val monthlyPsiList = getMonitorMonthlyPSI(monitor, adjustStartDate)
              val nDays = monthlyPsiList.length
              ("PSI月報表", ExcelUtility.psiMonthlyReport(monitor, adjustStartDate, monthlyPsiList, nDays))
          }

        Ok.sendFile(excelFile, fileName = _ =>
          play.utils.UriEncoding.encodePathSegment(Monitor.map(monitor).name + title + startDate.toString("YYYYMMdd") + ".xlsx", "UTF-8"),
          onClose = () => { Files.deleteIfExists(excelFile.toPath()) })

      } else {
        val (title, output) =
          reportType match {
            case PeriodReport.DailyReport =>
              val psiList = getDailyPsiReport(monitor, startDate)
              ("PSI日報表", views.html.psiDailyReport(monitor, startDate, psiList))
            case PeriodReport.MonthlyReport =>
              val adjustStartDate = DateTime.parse(startDate.toString("YYYY-MM-1"))
              val monthlyPsiList = getMonitorMonthlyPSI(monitor, adjustStartDate)
              val nDays = monthlyPsiList.length
              ("PSI月報表", views.html.psiMonthlyReport(monitor, adjustStartDate, monthlyPsiList, nDays))
          }
        outputType match {
          case OutputType.html =>
            Ok(output)
          case OutputType.pdf =>
            Ok.sendFile(creatPdfWithReportHeader(title, output),
              fileName = _ =>
                play.utils.UriEncoding.encodePathSegment(Monitor.map(monitor).name + title + startDate.toString("YYYYMMdd") + ".pdf", "UTF-8"))
        }
      }
  }

  def effectiveQuery() = Security.Authenticated {
    implicit request =>
      val userInfo = Security.getUserinfo(request).get
      val group = Group.getGroup(userInfo.groupID).get
      Ok(views.html.effectiveReport(group.privilege))
  }

  object EffectiveReportType extends Enumeration {
    val singleSite = Value("singleSite")
    val multipleSites = Value("multipleSites")
  }

  def effectiveAnnualReport(reportTypeStr: String, startStr: String, param: String, outputTypeStr: String) = Security.Authenticated {
    implicit request =>
      val userInfo = Security.getUserinfo(request).get
      val group = Group.getGroup(userInfo.groupID).get
      import Record._
      val reportType = EffectiveReportType.withName(reportTypeStr)
      val start = DateTime.parse(startStr)
      val adjustedStart = DateTime.parse(start.toString("YYYY-1-1"))
      val outputType = OutputType.withName(outputTypeStr)

      if (outputType == OutputType.excel) {

        val (title, excelFile) =
          reportType match {
            case EffectiveReportType.singleSite =>
              val monitor = Monitor.withName(param)
              val rateList = getMonitorYearlyEffectiveRate(monitor, adjustedStart)
              val statMap = getStatMonitorEffectiveRate(rateList)
              (Monitor.map(monitor).name + "有效率年報", ExcelUtility.createSingleSiteEffectiveReport(monitor, adjustedStart, rateList, statMap))

              
            case EffectiveReportType.multipleSites =>
              val monitorType = MonitorType.withName(param)
              val rateList = getMonitorTypeYearlyEffectiveRate(monitorType, adjustedStart)
              val statMap = getStatYearlyMonthlyEffectiveRate(rateList)

              (MonitorType.map(monitorType).desp + "有效率年報", ExcelUtility.createMultipleSiteEffectiveReport(monitorType, adjustedStart, rateList, statMap))              
          }

        Ok.sendFile(excelFile, fileName = _ =>
          play.utils.UriEncoding.encodePathSegment(title + adjustedStart.toString("YYYY") + ".xlsx", "UTF-8"),
          onClose = () => { Files.deleteIfExists(excelFile.toPath()) })
      } else {
        val (title, output) =
          reportType match {
            case EffectiveReportType.singleSite =>
              val monitor = Monitor.withName(param)
              val rateList = getMonitorYearlyEffectiveRate(monitor, adjustedStart)
              val statMap = getStatMonitorEffectiveRate(rateList)
              (Monitor.map(monitor).name + "有效率年報", views.html.singleSiteEffectiveRateYearlyReport(monitor, adjustedStart, rateList, statMap))

            case EffectiveReportType.multipleSites =>
              val monitorType = MonitorType.withName(param)
              val rateList = getMonitorTypeYearlyEffectiveRate(monitorType, adjustedStart)
              val statMap = getStatYearlyMonthlyEffectiveRate(rateList)

              (MonitorType.map(monitorType).desp + "有效率年報", views.html.multipleSiteEffectiveRateYearlyReport(monitorType, adjustedStart, rateList, statMap, group.privilege))
          }

        outputType match {
          case OutputType.html =>
            Ok(output)
          case OutputType.pdf =>
            Ok.sendFile(creatPdfWithReportHeader(title, output),
              fileName = _ =>
                play.utils.UriEncoding.encodePathSegment(title + adjustedStart.toString("YYYY") + ".pdf", "UTF-8"))
        }
      }
  }
  
  def epaCompare() = Security.Authenticated {
    implicit request =>
      val userInfo = Security.getUserinfo(request).get
      val group = Group.getGroup(userInfo.groupID).get
      Ok(views.html.epaCompare(group.privilege))
  }

  def epaCompareReport(monitorStr: String, epaMonitorStr: String, reportDateStr: String, outputTypeStr: String) = Security.Authenticated {
    implicit request =>
      val monitor = Monitor.withName(monitorStr)
      val epaMonitor = EpaMonitor.withName(epaMonitorStr)
      val reportDate = DateTime.parse(reportDateStr)
      val outputType = OutputType.withName(outputTypeStr)

      val (myMap, epaMap) = Record.compareEpaReport(monitor, epaMonitor, reportDate, reportDate + 1.day)
      def getHours(current: DateTime, endTime: DateTime): List[DateTime] = {
        if (current >= endTime)
          Nil
        else
          current :: getHours(current + 1.hour, endTime)
      }

      val hours = getHours(reportDate, reportDate + 1.day)

      if (outputType == OutputType.excel) {
        val excelFile = ExcelUtility.epaCompareReport(monitor, epaMonitor, reportDate, myMap, epaMap, hours)
        
        Ok.sendFile(excelFile, fileName = _ =>
          play.utils.UriEncoding.encodePathSegment("測站比較表" + reportDate.toString("YYMMdd") + ".xlsx", "UTF-8"),
          onClose = () => { Files.deleteIfExists(excelFile.toPath()) })
      } else {
        val output = views.html.epaCompareReport(monitor, epaMonitor, reportDate, myMap, epaMap, hours)
        val title = "測站比較報表"
        outputType match {
          case OutputType.html =>
            Ok(output)
          case OutputType.pdf =>
            Ok.sendFile(creatPdfWithReportHeader(title, output),
              fileName = _ =>
                play.utils.UriEncoding.encodePathSegment(title + reportDate.toString("YYYY-MM-dd") + ".pdf", "UTF-8"))
        }
      }      
  }
}



