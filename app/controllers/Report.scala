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
import models.ModelHelper._

object PeriodReport extends Enumeration {
  val DailyReport = Value("daily")
  val MonthlyReport = Value("monthly")
  val MinMonthlyReport = Value("MinMonthly")
  val YearlyReport = Value("yearly")
  def map = Map(DailyReport -> "日報", MonthlyReport -> "月報", MinMonthlyReport -> "分鐘月報", YearlyReport -> "年報")
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

  def getDays(start: DateTime, end: DateTime) = getPeriods(start, end, 1.day)
  def getWeeks(start: DateTime, end: DateTime) = getPeriods(start, end, 1.week)
  def getMonths(start: DateTime, end: DateTime) = getPeriods(start, end, 1.month)
  def getQuarters(start: DateTime, end: DateTime) = getPeriods(start, end, 3.month)

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

    import java.sql.Timestamp
    def validFilter(t: (Timestamp, Option[Float], Option[String])) = {
      if (t._2.isEmpty)
        false
      else {
        t._3 match {
          case Some(stat) => MonitorStatus.isNormalStat(stat)
          case _          => false
        }
      }
    }

    val monthHourStats =
      for {
        (mt, mt_i) <- MonitorType.monitorReportList.zipWithIndex
        hour <- 0 to 23
        hourRecord = getHourRecord(mt_i, hour)
        validData = hourRecord.filter { hr =>
          hr._3 match {
            case Some(stat) => hr._2.isDefined && MonitorStatus.isNormalStat(stat)
            case _          => false
          }
        }.map(r => r._2.get)

        count = validData.length
        total = nDay
        overCount = 0
      } yield {

        if (count != 0) {
          val avg = if (MonitorType.windDirList.contains(mt)) {
            val (windSpeed_mt, windSpeed_mti) = MonitorType.monitorReportList.zipWithIndex.find(t => t._1 == MonitorType.C212).get
            val windSeed = getHourRecord(windSpeed_mti, hour)
            val windDir = hourRecord
            windAvg(windSeed, windDir)
          } else {
            val sum = validData.sum
            sum / count
          }
          val max = validData.max
          val min = validData.min

          Stat(Some(avg), Some(min), Some(max), count, total, overCount)
        } else
          Stat(None, None, None, count, total, overCount)
      }

    val stats = monthHourStats.filter(t => t.count != 0)
    val count = stats.map(_.count).sum
    val overallStat =
      if (count != 0) {
        val max = stats.flatMap { _.max }.max
        val min = stats.flatMap { _.min }.min
        val total = stats.map { _.total }.sum
        val avg = stats.flatMap { _.avg }.sum / count
        Stat(Some(avg), Some(min), Some(max), count, total, 0)
      } else
        Stat(None, None, None, count, 0, 0)

    MonthHourReport(monthHourStats.toArray, dailyReports.toArray, overallStat)
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
      val windSpeedDailyReports =
        for { day <- days } yield {
          Record.getDailyReport(monitor, day, List(MonitorType.C211))
        }

      def getMonthHourStats(mt: MonitorType.Value) = {
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
            if (count != 0) {
              val avg = if (MonitorType.windDirList.contains(mt)) {
                val windDir = hourRecord
                val windSpeed = windSpeedDailyReports.map { _.typeList(0).dataList(hour) }
                windAvg(windSpeed, windDir)
              } else {
                val sum = validData.sum
                if (count != 0) sum / count else 0
              }

              Stat(Some(avg), Some(min), Some(max), count, total, overCount)
            } else
              Stat(None, None, None, count, total, overCount)
          }
        monthHourStats
      }

      val monthHourStats = getMonthHourStats(monitorType)
      val stats = monthHourStats.filter(t => t.count != 0)
      val count = stats.map(_.count).sum
      val total = stats.map { _.total }.sum
      val overallStat =
        if (count != 0) {
          val max = stats.map { _.max }.max
          val min = stats.map { _.min }.min

          val avg = if (MonitorType.windDirList.contains(monitorType)) {
            val sum_sin = stats.filter { v => v.avg.isDefined }.map(v => Math.sin(Math.toRadians(v.avg.get))).sum
            val sum_cos = stats.filter { v => v.avg.isDefined }.map(v => Math.cos(Math.toRadians(v.avg.get))).sum
            windAvg(sum_sin, sum_cos)
          } else {
            stats.flatMap { _.avg }.sum / count
          }
          Stat(Some(avg), min, max, count, total, 0)
        } else {
          Stat(None, None, None, count, total, 0)
        }
      val result = MonthHourReport(monthHourStats.toArray, dailyReports.toArray, overallStat)

      val output = views.html.monthlyHourReport(monitor, monitorType, startDate, result, nDay)
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

  def getMonthlyReport(monitor: Monitor.Value, startTime: DateTime, includeTypes: List[MonitorType.Value] = MonitorType.monitorReportList,
                       filter: MonitorStatusFilter.Value = MonitorStatusFilter.ValidData) = {
    val endTime = startTime + 1.month
    val days = getPeriods(startTime, endTime, 1.day)

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
        validData = typeStat.filter { _.count >= 16 }
        count = validData.length
        total = dailyReports.length
        overCount = validData.map(_.overCount).sum
      } yield {
        val overallStat =
          if (count >= 20) {
            val avg = if (MonitorType.windDirList.contains(monitorType)) {
              val windDir = validData
              val (windSpeedMt, windSpeedPos) = includeTypes.zipWithIndex.find(t => t._1 == MonitorType.C211).get
              val windSpeed = getTypeStat(windSpeedPos).filter { _.count != 0 }
              val wind = windSpeed.zip(windDir).filter(p => p._1.avg.isDefined && p._2.avg.isDefined)
              val wind_sin = wind.map(v => v._1.avg.get * Math.sin(Math.toRadians(v._2.avg.get))).sum
              val wind_cos = wind.map(v => v._1.avg.get * Math.cos(Math.toRadians(v._2.avg.get))).sum
              windAvg(wind_sin, wind_cos)
            } else {
              validData.flatMap { _.avg }.sum / count
            }
            val max = validData.map(_.avg).max
            val min = validData.map(_.avg).min
            Stat(Some(avg), min, max, count, total, overCount)
          } else
            Stat(None, None, None, count, total, overCount)

        MonitorTypeReport(monitorType, typeStat, overallStat)
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
    while (current < endTime) {
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
      for { start <- periods } yield {
        (start -> getPeriodReport(monitor, start, period, MonitorType.mtvAllList, filter))
      }

    import scala.collection.mutable.Map
    val map = Map.empty[MonitorType.Value, Map[DateTime, (Option[Float], Option[String])]]

    for {
      (time, report) <- periodReports
      t <- report.typeArray
    } {
      val periodMap = map.getOrElse(t.monitorType, Map.empty[DateTime, (Option[Float], Option[String])])
      if(t.stat.avg.isDefined)
        periodMap.put(time, (t.stat.avg, MonitorStatusFilter.statusMap.get(filter)))
        
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
      dateMap.put(week, (t.stat.avg, Some(MonitorStatusFilter.statusMap(filter))))
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
      dateMap.put(month, (t.stat.avg, Some(MonitorStatusFilter.statusMap(filter))))
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
      dateMap.put(quarter, (t.stat.avg, Some(MonitorStatusFilter.statusMap(filter))))
      map.put(t.monitorType, dateMap)
    }
    map
  }

  def monitorReport(monitorStr: String, reportTypeStr: String, startDateStr: String, outputTypeStr: String) = Security.Authenticated {
    implicit request =>
      val userInfo = Security.getUserinfo(request).get
      val group = Group.getGroup(userInfo.groupID).get

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
            overCount = validData.map(_.overCount).sum
          } yield {
            val overallStat =
              if (count != 0) {
                val avg = if (MonitorType.windDirList.contains(monitorType)) {
                  val (windSpeedMt, windSpeed_pos) = MonitorType.monitorReportList.zipWithIndex.find(t => t._1 == MonitorType.C211).get
                  val windSpeed = getTypeStat(windSpeed_pos)
                  val windDir = typeStat
                  val wind = windSpeed.zip(windDir).filter(t => t._1.count != 0 && t._2.count != 0)
                  val wind_sin = wind.map(v => v._1.avg.get * Math.sin(Math.toRadians(v._2.avg.get))).sum
                  val wind_cos = wind.map(v => v._1.avg.get * Math.cos(Math.toRadians(v._2.avg.get))).sum
                  windAvg(wind_sin, wind_cos)
                } else
                  validData.flatMap { _.avg }.sum / count
                val max = validData.map(_.avg).max
                val min = validData.map(_.avg).min

                Stat(Some(avg), min, max, count, total, overCount)
              } else
                Stat(None, None, None, 0, 0, 0)

            MonitorTypeReport(monitorType, typeStat, overallStat)
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
              ("月報", views.html.monthlyReport(monitor, startTime, monthlyReport, nDays))

            case PeriodReport.YearlyReport =>
              val adjustStartDate = DateTime.parse(startTime.toString("YYYY-1-1"))
              val yearlyReport = getYearlyReport(adjustStartDate)
              ("年報", views.html.yearlyReport(monitor, startTime, yearlyReport))
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
              //val dailyReport = Record.getDailyReport(monitor, startTime)
              //("日報" + startTime.toString("YYYYMMdd"), ExcelUtility.createDailyReport(monitor, startTime, dailyReport))

              ("日報" + startTime.toString("YYYYMMdd"), ExcelUtility.createAllDailyReport(startTime))

            case PeriodReport.MonthlyReport =>
              val adjustStartDate = DateTime.parse(startTime.toString("YYYY-MM-1"))
              val monthlyReport = getMonthlyReport(monitor, adjustStartDate)
              val nDay = monthlyReport.typeArray(0).dataList.length
              ("月報" + startTime.toString("YYYYMM"), ExcelUtility.createMonthlyReport(monitor, adjustStartDate, monthlyReport, nDay))

            /*  
            case PeriodReport.MinMonthlyReport =>
              Logger.error("Shall not be there...")
              val adjustStartDate = DateTime.parse(startTime.toString("YYYY-MM-1"))
              val monthlyReport = getMonthlyReport(monitor, adjustStartDate)
              val nDay = monthlyReport.typeArray(0).dataList.length
              ("各站分鐘月報" + startTime.toString("YYYYMM"), ExcelUtility.minMonthlyReport(group.privilege.allowedMonitors.toList, adjustStartDate))
            */

            case PeriodReport.YearlyReport =>
              val adjustStartDate = DateTime.parse(startTime.toString("YYYY-1-1"))
              val yearlyReport = getYearlyReport(adjustStartDate)

              (Monitor.map(monitor).name + "年報" + startTime.toString("YYYY"), ExcelUtility.createYearlyReport(monitor, adjustStartDate, yearlyReport))
          }

        Ok.sendFile(excelFile, fileName = _ =>
          play.utils.UriEncoding.encodePathSegment(title + ".xlsx", "UTF-8"),
          onClose = () => { Files.deleteIfExists(excelFile.toPath()) })
      }
  }

  def getMinMonthlySocket = WebSocket.acceptWithActor[String, String] { request =>
    out =>
      MinMonthlyReportWorker.props(out)
  }

  def downloadMinMontlyReport(n: Int) = Security.Authenticated {
    val file = SystemConfig.getDownloadLink(n)
    SystemConfig.cleanDownloadLink(n)
    Ok.sendFile(file, fileName = _ =>
      play.utils.UriEncoding.encodePathSegment("分鐘月報" + ".xlsx", "UTF-8"),
      onClose = () => { Files.deleteIfExists(file.toPath()) })

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

  def calibration() = Security.Authenticated {
    implicit request =>
      val userInfo = Security.getUserinfo(request).get
      val group = Group.getGroup(userInfo.groupID).get
      Ok(views.html.calibrationReportForm(group.privilege))
  }

  object CalibrationReportType extends Enumeration {
    val Daily = Value("daily")
    val Summary = Value("summary")
    val Monthly = Value("monthly")
    val map = Map(Daily -> "日報", Summary -> "彙總表", Monthly -> "月報")
  }

  def calibrationReport(monitorStr: String, monitorTypeStr: String, reportTypeStr: String, reportDateStr: String, outputTypeStr: String) = Security.Authenticated {
    implicit request =>
      val monitors = monitorStr.split(":").map { Monitor.withName }.toList
      val monitorType = MonitorType.withName(monitorTypeStr)
      val reportType = CalibrationReportType.withName(reportTypeStr)
      val reportDate = DateTime.parse(reportDateStr)
      val outputType = OutputType.withName(outputTypeStr)

      if (outputType == OutputType.excel) {
        val (title, excelFile) =
          reportType match {
            case CalibrationReportType.Daily =>
              val title = "校正日報"
              val reports = monitors.flatMap { m => Calibration.calibrationQueryReport(m, reportDate, reportDate + 1.day) }
              (title, ExcelUtility.calibrationDailyReport(title, reportDate, reports))
            case CalibrationReportType.Summary =>
              val title = "校正彙總表"
              val reports = monitors.flatMap { m => Calibration.calibrationSummary(m, reportDate, reportDate + 1.day) }
              (title, ExcelUtility.calibrationDailyReport(title, reportDate, reports))
            case CalibrationReportType.Monthly =>
              val title = "月報"
              val adjustedDate = DateTime.parse(reportDate.toString("YYYY-MM-01"))

              val reportMap = {
                val pairs =
                  for (mt <- MonitorType.calibrationList)
                    yield mt -> Calibration.calibrationMonthly(monitors(0), mt, adjustedDate)
                Map(pairs: _*)
              }
              val days = getDays(adjustedDate, adjustedDate + 1.month)
              val nDay = days.length

              //(title, ExcelUtility.calibrationMonthlyReport(monitors(0), monitorType, reportDate, reportMap, nDay))
              (title, ExcelUtility.calibrationMonthlyAllReport(monitors(0), reportDate, reportMap, nDay))
          }
        Ok.sendFile(excelFile, fileName = _ =>
          play.utils.UriEncoding.encodePathSegment(title + reportDate.toString("YYMMdd") + ".xlsx", "UTF-8"),
          onClose = () => { Files.deleteIfExists(excelFile.toPath()) })
      } else {
        val (title, output) =
          reportType match {
            case CalibrationReportType.Daily =>
              val title = "校正日報:" + reportDate.toString("YYYY年MM月dd日")
              val reports = monitors.flatMap { m => Calibration.calibrationQueryReport(m, reportDate, reportDate + 1.day) }
              val report = views.html.calibrationQueryResult(reports, title, reportDate, reportDate)
              (title, report)
            case CalibrationReportType.Summary =>
              val title = "校正彙總表:" + reportDate.toString("YYYY年MM月dd日")
              val reports = monitors.flatMap { m => Calibration.calibrationSummary(m, reportDate, reportDate + 1.day) }
              val report = views.html.calibrationQueryResult(reports, title, reportDate, reportDate)
              (title, report)
            case CalibrationReportType.Monthly =>

              val adjustedDate = DateTime.parse(reportDate.toString("YYYY-MM-01"))
              val title = "月報:" + adjustedDate.toString("YYYY年MM月")
              val reportMap = Calibration.calibrationMonthly(monitors(0), monitorType, adjustedDate)
              val reports = reportMap.values.toList.sortBy { item => item.startTime }
              val report = views.html.calibrationQueryResult(reports, title, adjustedDate, adjustedDate)
              (title, report)
          }

        outputType match {
          case OutputType.html =>
            Ok(output)
          case OutputType.pdf =>
            Ok.sendFile(creatPdfWithReportHeader(title, output),
              fileName = _ =>
                play.utils.UriEncoding.encodePathSegment(title + reportDate.toString("YYYYMMdd") + ".pdf", "UTF-8"))
        }

      }

  }

  def monitorAbnormal() = Security.Authenticated {
    implicit request =>
      val userInfo = Security.getUserinfo(request).get
      val group = Group.getGroup(userInfo.groupID).get
      Ok(views.html.monitorAbnormal(group.privilege))
  }

  def monitorAbnormalReport(dateStr: String, outputTypeStr: String) = Security.Authenticated {
    implicit request =>
      val userInfo = Security.getUserinfo(request).get
      val group = Group.getGroup(userInfo.groupID).get
      val date = DateTime.parse(dateStr)
      val reportOpt = AbnormalReport.getReportFromDb(date)
      val report =
        if (reportOpt.isEmpty) {
          AbnormalReport.newReport(date)
          AbnormalReport(date, Seq.empty[AbnormalEntry])
        } else
          reportOpt.get

      import scala.collection.mutable.Map
      val explainMap = Map.empty[Monitor.Value, Map[MonitorType.Value, String]]
      for (ex <- report.report) {
        val mtMap = explainMap.getOrElseUpdate(ex.monitor, Map.empty[MonitorType.Value, String])
        mtMap.put(ex.monitorType, ex.explain)
      }

      val latestReport = AbnormalReport(date, AbnormalReport.generate(date))
      val reportWithExplain = latestReport.report.map {
        ex =>
          val mtMap = explainMap.getOrElse(ex.monitor, Map.empty[MonitorType.Value, String])
          val explain = mtMap.getOrElse(ex.monitorType, "")
          AbnormalEntry(ex.monitor, ex.monitorType, ex.invalidHours, explain)
      }
      val outputType = OutputType.withName(outputTypeStr)

      val title = "測站異常狀況反應表"
      val output = views.html.monitorAbnormalReport(date, reportWithExplain)

      outputType match {
        case OutputType.html =>
          Ok(output)
        case OutputType.pdf =>
          Ok.sendFile(creatPdfWithReportHeader(title, output),
            fileName = _ =>
              play.utils.UriEncoding.encodePathSegment(title + date.toString("YYYYMMdd") + ".pdf", "UTF-8"))

        case OutputType.excel =>
          val excelFile = ExcelUtility.monitorAbnormalReport(date, reportWithExplain)
          Ok.sendFile(excelFile, fileName = _ =>
            play.utils.UriEncoding.encodePathSegment(title + date.toString("YYMMdd") + ".xlsx", "UTF-8"),
            onClose = () => { Files.deleteIfExists(excelFile.toPath()) })

      }
  }

  def saveMonitorAbnormalReport(dateStr: String) = Security.Authenticated(BodyParsers.parse.json) {
    implicit request =>
      import AbnormalReport._
      val date = DateTime.parse(dateStr)
      val abEntriesResult = request.body.validate[Seq[AbnormalEntry]]

      abEntriesResult.fold(
        error => {
          Logger.error(JsError.toFlatJson(error).toString())
          BadRequest(Json.obj("ok" -> false, "msg" -> JsError.toFlatJson(error)))
        },
        entries => {
          AbnormalReport.updateReport(date, entries)
          Ok(Json.obj("ok" -> true, "nEntries" -> entries.length))
        });
  }

  def monitorAggregate = Security.Authenticated {
    implicit request =>
      val userInfo = Security.getUserinfo(request).get
      val group = Group.getGroup(userInfo.groupID).get
      Ok(views.html.monitorAbnormal(group.privilege, "/MonitorAggregateReport/"))
  }

  def monitorAggregateReport(dateStr: String, outputTypeStr: String) = Security.Authenticated {
    implicit request =>
      val userInfo = Security.getUserinfo(request).get
      val group = Group.getGroup(userInfo.groupID).get
      val date = DateTime.parse(dateStr)
      val reportOpt = AggregateReport.getReportFromDb(date)
      val report =
        if (reportOpt.isEmpty) {
          AggregateReport.newReport(date)
          AggregateReport(date, Seq.empty[MonitorSummary])
        } else
          reportOpt.get

      val explainMap = Map(report.report.map { r => (r.monitor -> r.explain) }: _*)
      val latestReport = AggregateReport(date, AggregateReport.generate(date))
      val reportWithExplain = latestReport.report.map {
        summary =>
          val explain = explainMap.getOrElse(summary.monitor, "")
          MonitorSummary(summary.monitor, summary.desc, explain)
      }

      val outputType = OutputType.withName(outputTypeStr)

      val title = "測站每日監測彙總表"
      val output = views.html.monitorAggregateReport(date, reportWithExplain)

      outputType match {
        case OutputType.html =>
          Ok(output)
        case OutputType.pdf =>
          Ok.sendFile(creatPdfWithReportHeader(title, output),
            fileName = _ =>
              play.utils.UriEncoding.encodePathSegment(title + date.toString("YYYYMMdd") + ".pdf", "UTF-8"))
        case OutputType.excel =>
          val excelFile = ExcelUtility.monitorAggregateReport(date, reportWithExplain)
          Ok.sendFile(excelFile, fileName = _ =>
            play.utils.UriEncoding.encodePathSegment(title + date.toString("YYMMdd") + ".xlsx", "UTF-8"),
            onClose = () => { Files.deleteIfExists(excelFile.toPath()) })
      }
  }

  def saveMonitorAggregateReport(dateStr: String) = Security.Authenticated(BodyParsers.parse.json) {
    implicit request =>
      import AggregateReport._
      val date = DateTime.parse(dateStr)
      val mSummaryResult = request.body.validate[Seq[MonitorSummary]]

      mSummaryResult.fold(
        error => {
          Logger.error(JsError.toFlatJson(error).toString())
          BadRequest(Json.obj("ok" -> false, "msg" -> JsError.toFlatJson(error)))
        },
        entries => {
          AggregateReport.updateReport(date, entries)
          Ok(Json.obj("ok" -> true, "nEntries" -> entries.length))
        });
  }

}



