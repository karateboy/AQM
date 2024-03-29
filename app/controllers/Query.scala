package controllers

import com.github.nscala_time.time.Imports._
import controllers.PdfUtility._
import controllers.Realtime.{AxisTitle, XAxis, YAxis}
import models.ModelHelper._
import models._
import play.api.Logger
import play.api.libs.json._
import play.api.mvc._

import java.nio.file.Files

object ReportUnit extends Enumeration {
  val Min = Value("min")
  val TenMin = Value("ten_min")
  val Hour = Value("hour")
  val EightHour = Value("eight_hour")
  val Day = Value("day")
  val Week = Value("week")
  val Month = Value("month")
  val Quarter = Value("quarter")
  val map = Map((Min -> "分"), (TenMin -> "十分"), (Hour -> "小時"), (EightHour -> "八小時"), (Day -> "日"), (Week -> "週"), (Month -> "月"), (Quarter -> "季"))
}

object Query extends Controller {

  def history() = Security.Authenticated {
    implicit request =>
      val userInfo = Security.getUserinfo(request).get
      val group = Group.getGroup(userInfo.groupID).get
      Ok(views.html.history("/HistoryQueryReport/" + false.toString + "/", group.privilege))
  }

  def auditedQuery() = Security.Authenticated {
    implicit request =>
      val userInfo = Security.getUserinfo(request).get
      val group = Group.getGroup(userInfo.groupID).get
      Ok(views.html.auditQuery(group.privilege))
  }

  def auditReport(monitorStr: String, monitorTypeStr: String, recordTypeStr: String, startStr: String, endStr: String, outputTypeStr: String, reaudit: Boolean) = Security.Authenticated {
    implicit request =>


      val monitorStrArray = monitorStr.split(':')
      val monitors = monitorStrArray.map {
        Monitor.withName
      }
      val monitorType = MonitorType.withName(monitorTypeStr)
      val tabType = TableType.withName(recordTypeStr)
      val start =
        DateTime.parse(startStr, DateTimeFormat.forPattern("YYYY-MM-dd HH:mm"))

      val end =
        DateTime.parse(endStr, DateTimeFormat.forPattern("YYYY-MM-dd HH:mm"))
      val outputType = OutputType.withName(outputTypeStr)

      var timeSet = Set.empty[DateTime]

      if (reaudit) {
        for (m <- monitors)
          Auditor.auditHourData(m, Monitor.map(m).autoAudit, start, end, reaudit)
      }
      val pairs =
        for {
          m <- monitors
          records = Record.getInvalidHourRecords(m, start, end)
          mtPreRecords = records.map { rs => (Record.timeProjection(rs).toDateTime, Record.monitorTypeProject2(monitorType)(rs)) }
          mtRecords = mtPreRecords.filter(r => r._2._1.isDefined && r._2._2.isDefined &&
            MonitorStatus.getTagInfo(r._2._2.get).statusType == StatusType.Auto)
          timeMap = Map(mtRecords: _*)
        } yield {
          timeSet ++= timeMap.keySet
          (m -> timeMap)
        }

      val recordMap = Map(pairs: _*)

      val title = "無效資料查詢"
      val output = views.html.auditedReport(false, monitors, monitorType, start, end, timeSet.toList.sorted, recordMap)
      outputType match {
        case OutputType.html =>
          Ok(output)
        case OutputType.pdf =>
          Ok.sendFile(creatPdfWithReportHeader(title, output),
            fileName = _ =>
              play.utils.UriEncoding.encodePathSegment(title + start.toString("YYMMdd") + "_" + end.toString("MMdd") + ".pdf", "UTF-8"))
      }
  }

  def historyReport(edit: Boolean, monitorStr: String, epaMonitorStr: String, monitorTypeStr: String, recordTypeStr: String,
                    startStr: String, endStr: String, mb: Boolean, outputTypeStr: String) = Security.Authenticated {
    implicit request =>


      val monitorStrArray = monitorStr.split(':')
      val monitors = monitorStrArray.map {
        Monitor.withName
      }
      val epaMonitors = if (epaMonitorStr.equalsIgnoreCase("None"))
        Array.empty[EpaMonitor.Value]
      else
        epaMonitorStr.split(':').map {
          EpaMonitor.withName
        }

      val monitorType = MonitorType.withName(monitorTypeStr)
      val tableType = TableType.withName(recordTypeStr)
      val start =
        DateTime.parse(startStr, DateTimeFormat.forPattern("YYYY-MM-dd HH:mm")).withMinuteOfHour(0)

      val end =
        DateTime.parse(endStr, DateTimeFormat.forPattern("YYYY-MM-dd HH:mm")).withMinuteOfHour(0)

      val outputType = OutputType.withName(outputTypeStr)

      val timeList = tableType match {
        case TableType.SixSec =>
          Report.getPeriods(start, end, 6.second)
        case TableType.EightHour =>
          Report.get8hrPeriods(start.withMillisOfDay(0).withHourOfDay(start.getHourOfDay()), end)
        case TableType.Hour =>
          Report.getPeriods(start.withMinuteOfHour(0).plusHours(1), end.withMinuteOfHour(0), 1.hour)
        case TableType.Min =>
          Report.getPeriods(start, end, 1.minute)
      }

      val pairs =
        for {
          m <- monitors
          records =
            tableType match {
              case TableType.EightHour=>
                Record.get8HourRecords(m, start, end)
              case TableType.Hour =>
                Record.getHourRecords(m, start, end)
              case TableType.Min =>
                Record.getMinRecords(m, start, end)
            }

          t = Record.monitorTypeProject2(monitorType)
          calibrationMap = Calibration.getCalibrationMap(m, start, end)

          mtRecords = records.map { rs =>
            import Calibration._
            if (mb && canCalibrate(monitorType, rs)(calibrationMap)) {
              val calibrated = doCalibrate(monitorType, rs)(calibrationMap)
              (Record.timeProjection(rs).toDateTime, (calibrated, t(rs)._2))
            } else if (mb && monitorType == MonitorType.A226 &&
              canCalibrate(MonitorType.A286, rs)(calibrationMap) && canCalibrate(MonitorType.A296, rs)(calibrationMap)) {
              //A296=>NMHC, A286=>CH4, A226=>THC
              val calibratedCH4 = doCalibrate(MonitorType.A286, rs)(calibrationMap)
              val calibratedNMHC = doCalibrate(MonitorType.A226, rs)(calibrationMap)
              val thc =
                for (ch4 <- calibratedCH4; nmhc <- calibratedNMHC)
                  yield ch4 + nmhc

              (Record.timeProjection(rs).toDateTime, (thc, t(rs)._2))
            } else
              (Record.timeProjection(rs).toDateTime, t(rs))
          }
          timeMap = Map(mtRecords: _*)
        } yield {
          (m -> timeMap)
        }

      val recordMap = Map(pairs: _*)

      val epa_pairs =
        for {
          epa <- epaMonitors
          records = Record.getEpaHourRecord(epa, monitorType, start, end)
          timeRecords = records.map { t => t.time -> t.value }
          timeMap = Map(timeRecords: _*)
        } yield {
          epa -> timeMap
        }
      val epaRecordMap = Map(epa_pairs: _*)
      val title = "歷史資料查詢"
      val output =
        views.html.historyReport(edit, monitors, epaMonitors, monitorType, start, end, timeList, recordMap, epaRecordMap, false, tableType.toString)
      outputType match {
        case OutputType.html =>
          Ok(output)
        case OutputType.pdf =>
          Ok.sendFile(creatPdfWithReportHeader(title, output),
            fileName = _ =>
              play.utils.UriEncoding.encodePathSegment(title + start.toString("YYMMdd") + "_" + end.toString("MMdd") + ".pdf", "UTF-8"))
        case OutputType.excel =>
          val reportUnitStr = tableType match {
            case TableType.Min=>
              ReportUnit.Min.toString

            case TableType.Hour=>
              ReportUnit.Hour.toString
            case TableType.EightHour=>
              ReportUnit.EightHour.toString
          }

          val url = if(TableType.EightHour != tableType) {
            s"/Excel/HistoryTrend/${monitorStr}/${play.utils.UriEncoding.encodePathSegment(epaMonitorStr, "UTF-8")}/${monitorTypeStr}/${reportUnitStr}/${MonitorStatusFilter.All.toString}/${startStr}/${endStr}/${mb}"
          }else{
            val s= start.toString("YYYY-MM-dd")
            val e= end.toString("YYYY-MM-dd")
            s"/Excel/HistoryTrend/${monitorStr}/${play.utils.UriEncoding.encodePathSegment(epaMonitorStr, "UTF-8")}/${monitorTypeStr}/${reportUnitStr}/${MonitorStatusFilter.All.toString}/${s}/${e}/${mb}"
          }
          Redirect(url)
      }
  }

  def historyTrend = Security.Authenticated {
    implicit request =>
      val userInfo = Security.getUserinfo(request).get
      val group = Group.getGroup(userInfo.groupID).get
      Ok(views.html.historyTrend(group.privilege))
  }

  def historyTrendChart(monitorStr: String, epaMonitorStr: String, monitorTypeStr: String,
                        reportUnitStr: String, msfStr: String, startStr: String, endStr: String, mb: Boolean, outputTypeStr: String) = Security.Authenticated {
    implicit request =>

      val monitorStrArray = monitorStr.split(':')
      val monitors = monitorStrArray.map {
        Monitor.withName
      }
      val epaMonitors = if (epaMonitorStr.equalsIgnoreCase("None"))
        Array.empty[EpaMonitor.Value]
      else
        epaMonitorStr.split(':').map {
          EpaMonitor.withName
        }
      val monitorTypeStrArray = monitorTypeStr.split(':')
      val monitorTypes = monitorTypeStrArray.map {
        MonitorType.withName
      }
      val reportUnit = ReportUnit.withName(reportUnitStr)
      val monitorStatusFilter = MonitorStatusFilter.withName(msfStr)
      val (start, end) =
        if (reportUnit == ReportUnit.Min || reportUnit == ReportUnit.TenMin || reportUnit == ReportUnit.Hour) {
          (DateTime.parse(startStr, DateTimeFormat.forPattern("YYYY-MM-dd HH:mm")),
            DateTime.parse(endStr, DateTimeFormat.forPattern("YYYY-MM-dd HH:mm")))
        } else if (reportUnit == ReportUnit.Month || reportUnit == ReportUnit.Quarter) {
          (DateTime.parse(startStr, DateTimeFormat.forPattern("YYYY-M")),
            DateTime.parse(endStr, DateTimeFormat.forPattern("YYYY-M")))
        } else {
          (DateTime.parse(startStr, DateTimeFormat.forPattern("YYYY-MM-dd")),
            DateTime.parse(endStr, DateTimeFormat.forPattern("YYYY-MM-dd")))
        }
      val outputType = OutputType.withName(outputTypeStr)

      val chart = trendHelper(monitors, epaMonitors, monitorTypes, reportUnit, monitorStatusFilter, start, end, mb)

      if (outputType == OutputType.excel) {
        val mts = monitors.flatMap { _ => monitorTypes.toList }
        val epaMts = epaMonitors.flatMap { _ => monitorTypes.toList }
        val excelFile = ExcelUtility.exportChartData(chart, mts ++ epaMts)
        val downloadFileName =
          if (chart.downloadFileName.isDefined)
            chart.downloadFileName.get
          else
            chart.title("text")

        Ok.sendFile(excelFile, fileName = _ =>

          play.utils.UriEncoding.encodePathSegment(downloadFileName + ".xlsx", "UTF-8"),
          onClose = () => {
            Files.deleteIfExists(excelFile.toPath())
          })
      } else {
        Results.Ok(Json.toJson(chart))
      }
  }

  def trendHelper(monitors: Array[Monitor.Value], epaMonitors: Array[EpaMonitor.Value],
                  monitorTypes: Array[MonitorType.Value], reportUnit: ReportUnit.Value, monitorStatusFilter: MonitorStatusFilter.Value,
                  start: DateTime, end: DateTime, mb: Boolean) = {
    def statusFilter(data: (DateTime, (Option[Float], Option[String]))): Boolean = {
      if (data._2._2.isEmpty)
        return false

      val stat = data._2._2.get

      MonitorStatusFilter.isMatched(monitorStatusFilter, stat)
    }

    import controllers.Report._
    val timeList = reportUnit match {
      case ReportUnit.Min =>
        Report.getPeriods(start, end, 1.minute)
      case ReportUnit.Hour =>
        Report.getPeriods(DateTime.parse(start.toString("YYYY-MM-dd HH"), DateTimeFormat.forPattern("YYYY-MM-dd HH")),
          DateTime.parse(end.toString("YYYY-MM-dd HH"), DateTimeFormat.forPattern("YYYY-MM-dd HH")), 1.hour)
      case ReportUnit.TenMin =>
        Report.getPeriods(DateTime.parse(start.toString("YYYY-MM-dd")), DateTime.parse(end.toString("YYYY-MM-dd")) + 1.day, 10.minute)
      case ReportUnit.EightHour =>
        Report.getPeriods(DateTime.parse(start.toString("YYYY-MM-dd")), DateTime.parse(end.toString("YYYY-MM-dd")) + 1.day, 1.hour)
      case ReportUnit.Day =>
        Report.getDays(DateTime.parse(start.toString("YYYY-MM-dd")), DateTime.parse(end.toString("YYYY-MM-dd")))
      case ReportUnit.Week =>
        Report.getWeeks(DateTime.parse(adjustWeekDay(start).toString("YYYY-MM-dd")),
          DateTime.parse(adjustWeekDay(end).toString("YYYY-MM-dd")) + 1.weeks)
      case ReportUnit.Month =>
        Report.getMonths(DateTime.parse(start.toString("YYYY-MM-01")), DateTime.parse(end.toString("YYYY-MM-01")) + 1.months)
      case ReportUnit.Quarter =>
        Report.getQuarters(DateTime.parse(s"${start.getYear}-${1 + (start.getMonthOfYear - 1) / 3 * 3}-01"),
          DateTime.parse(s"${end.getYear}-${1 + (end.getMonthOfYear - 1) / 3 * 3}-01") + 3.month)
    }

    val pairs =
      if (reportUnit == ReportUnit.Min || reportUnit == ReportUnit.Hour|| reportUnit == ReportUnit.EightHour) {
        val ps =
          for {
            m <- monitors
            records = reportUnit match {
              case ReportUnit.Min =>
                Record.getMinRecords(m, start, end)
              case ReportUnit.Hour =>
                Record.getHourRecords(m, start, end)
              case ReportUnit.EightHour=>
                Record.get8HourRecords(m, start, end).filter(p=>{
                  val dt = p.date.toDateTime
                  dt.getHourOfDay>=7
                })
            }

            mtPairs = for {
              mt <- monitorTypes
            } yield {
              val mtRecords = if (mb) {
                val calibrationMap = Calibration.getCalibrationMap(m, start, end)
                records.map { rs =>
                  import Calibration._
                  if (mb && canCalibrate(mt, rs)(calibrationMap)) {
                    val calibrated = doCalibrate(mt, rs)(calibrationMap)
                    (Record.timeProjection(rs).toDateTime, (calibrated, Record.monitorTypeProject2(mt)(rs)._2))
                  } else if (mb && mt == MonitorType.A296 &&
                    canCalibrate(MonitorType.A286, rs)(calibrationMap) && canCalibrate(MonitorType.A226, rs)(calibrationMap)) {
                    //A296=>NMHC, A286=>CH4, A226=>THC
                    val calibratedCH4 = doCalibrate(MonitorType.A286, rs)(calibrationMap)
                    val calibratedTHC = doCalibrate(MonitorType.A226, rs)(calibrationMap)
                    val interpolatedNMHC =
                      for (ch4 <- calibratedCH4; thc <- calibratedTHC)
                        yield thc - ch4

                    (Record.timeProjection(rs).toDateTime, (interpolatedNMHC, Record.monitorTypeProject2(mt)(rs)._2))
                  } else
                    (Record.timeProjection(rs).toDateTime, Record.monitorTypeProject2(mt)(rs))
                }
              } else
                records.map { rs => (Record.timeProjection(rs).toDateTime, Record.monitorTypeProject2(mt)(rs)) }
              val msfRecords = mtRecords.filter(statusFilter)
              val timeMap = Map(msfRecords: _*)
              mt -> timeMap
            }
          } yield {
            m -> Map(mtPairs: _*)
          }
        ps
      } else {
        import controllers.Report._
        val ps =
          for {
            m <- monitors
          } yield {
            reportUnit match {
              case ReportUnit.TenMin =>
                m -> getPeriodReportMap(m, start, end, monitorStatusFilter, 10.minute)
              case ReportUnit.EightHour =>
                m -> getPeriodReportMap(m, start, end, monitorStatusFilter, 8.hour)
              case ReportUnit.Day =>
                m -> getPeriodReportMap(m, start, end, monitorStatusFilter, 1.day)
              case ReportUnit.Week =>
                m -> getWeeklyReportMap(m, start, end, monitorStatusFilter)
              case ReportUnit.Month =>
                m -> getMonthlyReportMap(m, start, end, monitorStatusFilter)
              case ReportUnit.Quarter =>
                m -> getQuarterReportMap(m, start, end, monitorStatusFilter)
            }
          }
        ps
      }

    val recordMap = Map(pairs: _*)
    val timeSeq = timeList

    import Realtime._

    val windMtv = MonitorType.withName("C212")
    val local_series =
      if (monitorTypes.length > 1 && monitorTypes.contains(windMtv)) {
        //val noWinMt = monitorTypes.filter { _ != windMtv }
        for {
          m <- monitors
          mt <- monitorTypes
          timeData = timeSeq.map { time =>
            if (recordMap(m)(mt).contains(time))
              Seq(Some(time.getMillis.toDouble), recordMap(m)(mt)(time)._1.map(_.toDouble))
            else
              Seq(Some(time.getMillis.toDouble), None)
          }
          timeStatus = timeSeq.map { time =>
            if (recordMap(m)(mt).contains(time))
              recordMap(m)(mt)(time)._2
            else
              None
          }
        } yield {
          if (mt != windMtv)
            seqData(name = Monitor.map(m).name + "_" + MonitorType.map(mt).desp, data = timeData, status = Some(timeStatus),
              tooltip = ToolTip(valueDecimals = MonitorType.map(mt).prec))
          else
            seqData(name = Monitor.map(m).name + "_" + MonitorType.map(mt).desp, data = timeData, yAxis = 1, chartType = Some("scatter"),
              status = Some(timeStatus), tooltip = ToolTip(valueDecimals = MonitorType.map(mt).prec))
        }
      } else {
        for {
          m <- monitors
          mt <- monitorTypes
          timeData = timeSeq.map { time =>
            if (recordMap(m)(mt).contains(time))
              Seq(Some(time.getMillis.toDouble), recordMap(m)(mt)(time)._1.map(_.toDouble))
            else
              Seq(Some(time.getMillis.toDouble), None)
          }
          timeStatus = timeSeq.map { time =>
            if (recordMap(m)(mt).contains(time))
              recordMap(m)(mt)(time)._2
            else
              None
          }
        } yield {
          seqData(name = Monitor.map(m).name + "_" + MonitorType.map(mt).desp, data = timeData,
            status = Some(timeStatus), tooltip = ToolTip(MonitorType.map(mt).prec))
        }
      }

    def epaSeries() = {
      val epaMonitorPairs =
        for {
          m <- epaMonitors
        } yield {
          val epaMtPairs =
            for {
              mt <- monitorTypes
              epaRecord = Record.getEpaHourRecord(m, mt, start, end)
              epaPairs = epaRecord.map { r => r.time -> r.value }
              epaMap = Map(epaPairs: _*)
            } yield {
              mt -> epaMap
            }
          m -> Map(epaMtPairs: _*)
        }
      val epaRecordMap = Map(epaMonitorPairs: _*)
      for {
        m <- epaMonitors
        mt <- monitorTypes
        timeData = timeSeq.map { time =>
          if (epaRecordMap(m)(mt).contains(time))
            Seq(Some(time.getMillis.toDouble), Some(epaRecordMap(m)(mt)(time).toDouble))
          else
            Seq(Some(time.getMillis.toDouble), None)
        }
      } yield {
        if (monitorTypes.length > 1 && monitorTypes.contains(windMtv)) {
          if (mt != windMtv)
            seqData(EpaMonitor.map(m).name + "_" + MonitorType.map(mt).desp, timeData, tooltip = ToolTip(MonitorType.map(mt).prec))
          else
            seqData(EpaMonitor.map(m).name + "_" + MonitorType.map(mt).desp, timeData, 1, ToolTip(MonitorType.map(mt).prec), Some("scatter"))
        } else {
          seqData(EpaMonitor.map(m).name + "_" + MonitorType.map(mt).desp, timeData, tooltip = ToolTip(MonitorType.map(mt).prec))
        }
      }
    }

    val epa_series = epaSeries()

    val series: Array[seqData] = local_series ++ epa_series

    val downloadFileName = {
      val startName = start.toString("YYMMdd")
      val mNames = monitors.map {
        Monitor.map(_).name
      }
      val mtNames = monitorTypes.map {
        MonitorType.map(_).desp
      }
      startName + mNames.mkString + mtNames.mkString
    }

    val title = s"趨勢圖 (${start.toString("YYYY/MM/dd HH:mm")}~${end.toString("YYYY/MM/dd HH:mm")})"

    val timeStrSeq = timeSeq.map { t =>
      reportUnit match {
        case ReportUnit.Min =>
          t.toString("YYYY/MM/dd HH:mm")
        case ReportUnit.TenMin =>
          t.toString("YYYY/MM/dd HH:mm")
        case ReportUnit.Hour =>
          t.toString("YYYY/MM/dd HH:mm")
        case ReportUnit.EightHour =>
          t.toString("YYYY/MM/dd HH:mm")
        case ReportUnit.Day =>
          t.toString("YYYY/MM/dd")
        case ReportUnit.Week =>
          t.toString("YYYY/MM/dd")
        case ReportUnit.Month =>
          t.toString("YYYY/MM")
        case ReportUnit.Quarter =>
          s"${t.getYear}/${1 + (t.getMonthOfYear - 1) / 3}Q"
      }
    }

    def getAxisLines(mt: MonitorType.Value) = {
      val mtCase = MonitorType.map(mt)
      val m = monitors(0)
      val std_law_line =
        if (MonitorTypeAlert.map(m)(mt).std_law.isEmpty)
          None
        else
          Some(AxisLine("#FF0000", 2, MonitorTypeAlert.map(m)(mt).std_law.get, Some(AxisLineLabel("right", "法規值"))))

      val std_internal_line = {
        val std_internals = monitors.map {
          MonitorTypeAlert.map(_)(mt).internal
        }
        val min_std_internal = std_internals.min
        if (min_std_internal.isDefined)
          Some(AxisLine("#0000FF", 2, MonitorTypeAlert.map(m)(mt).internal.get, Some(AxisLineLabel("left", "內控值"))))
        else
          None
      }

      val lines = Seq(std_law_line, std_internal_line).filter {
        _.isDefined
      }.map {
        _.get
      }
      if (lines.length > 0)
        Some(lines)
      else
        None
    }

    val xAxis = {
      reportUnit match {
        case ReportUnit.Min =>
          XAxis(None, gridLineWidth = Some(1), None)
        case ReportUnit.TenMin =>
          XAxis(None, gridLineWidth = Some(1), None)
        case ReportUnit.Hour =>
          val duration = new Duration(start, end)
          if (duration.getStandardDays > 2)
            XAxis(None, gridLineWidth = Some(1), None)
          else
            XAxis(None)
        case ReportUnit.EightHour =>
          XAxis(None, gridLineWidth = Some(1), None)
        case ReportUnit.Day =>
          XAxis(None)
        case ReportUnit.Week =>
          XAxis(None)
        case ReportUnit.Month =>
          XAxis(None)
        case ReportUnit.Quarter =>
          XAxis(None)
      }
    }

    val windMtCase = MonitorType.map(MonitorType.withName("C212"))
    val windYaxis = YAxis(None, AxisTitle(Some(Some(s"${windMtCase.desp} (${windMtCase.unit})"))), None,
      opposite = true,
      floor = Some(0),
      ceiling = Some(360),
      min = Some(0),
      max = Some(360),
      tickInterval = Some(45),
      gridLineWidth = Some(1),
      gridLineColor = Some("#00D800"))

    val chart =
      if (monitorTypes.length == 1) {
        val mt = monitorTypes(0)
        val mtCase = MonitorType.map(monitorTypes(0))

        HighchartData(
          Map("type" -> "line"),
          Map("text" -> title),
          xAxis,
          if (!monitorTypes.contains(windMtv))
            Seq(YAxis(None, AxisTitle(Some(Some(s"${mtCase.desp} (${mtCase.unit})"))), getAxisLines(mt)))
          else
            Seq(windYaxis),
          series,
          Some(downloadFileName))
      } else {
        val yAxis =
          if (monitorTypes.contains(windMtv)) {
            if (monitorTypes.length == 2) {
              val mt = monitorTypes.filter {
                !MonitorType.windDirList.contains(_)
              }(0)
              val mtCase = MonitorType.map(monitorTypes.filter {
                !MonitorType.windDirList.contains(_)
              }(0))
              Seq(YAxis(None,
                AxisTitle(Some(Some(s"${mtCase.desp} (${mtCase.unit})"))),
                getAxisLines(mt),
                gridLineWidth = Some(0)),
                windYaxis)
            } else {
              Seq(YAxis(None, AxisTitle(Some(None)), None, gridLineWidth = Some(0)),
                windYaxis)
            }
          } else {
            Seq(YAxis(None, AxisTitle(Some(None)), None))
          }

        HighchartData(
          Map("type" -> "line"),
          Map("text" -> title),
          xAxis,
          yAxis,
          series,
          Some(downloadFileName))
      }

    chart
  }

  def psiTrend = Security.Authenticated {
    implicit request =>
      val userInfo = Security.getUserinfo(request).get
      val group = Group.getGroup(userInfo.groupID).get
      Ok(views.html.psiTrend(group.privilege))
  }

  def psiTrendChart(monitorStr: String, startStr: String, endStr: String, isDailyPsi: Boolean, outputTypeStr: String) = Security.Authenticated {
    implicit request =>

      val monitorStrArray = monitorStr.split(':')
      val monitors = monitorStrArray.map {
        Monitor.withName
      }
      val start = DateTime.parse(startStr)
      val end = DateTime.parse(endStr) + 1.day
      val outputType = OutputType.withName(outputTypeStr)

      import scala.collection.mutable.Map
      def getPsiMap(m: Monitor.Value) = {
        import models.Realtime._
        var current = start

        val map = Map.empty[DateTime, Float]
        while (current < end) {
          if (isDailyPsi) {
            val v = getMonitorDailyPSI(m, current)
            if (v.psi.isDefined) {
              val psi = v.psi.get
              map += (current -> psi)
            }
            current += 1.day
          } else {
            val v = getRealtimePSI(current, List(m))
            if (v(m)._1.isDefined) {
              val psi = v(m)._1.get
              map += (current -> psi)
            }
            current += 1.hour
          }
        }
        map
      }

      val monitorPsiMap = Map.empty[Monitor.Value, Map[DateTime, Float]]
      var timeSet = Set.empty[DateTime]

      for (m <- monitors) {
        monitorPsiMap += (m -> getPsiMap(m))
        timeSet ++= getPsiMap(m).keys.toSet
      }

      val title = "PSI歷史趨勢圖"
      val timeSeq = timeSet.toList.sorted.zipWithIndex
      import Realtime._

      val series = for {
        m <- monitors
        timeData = timeSeq.map { t =>
          val time = t._1
          val x = t._2
          if (monitorPsiMap(m).contains(time))
            Seq(Some(time.getMillis.toDouble), Some(monitorPsiMap(m)(time).toDouble))
          else
            Seq(Some(time.getMillis.toDouble), None)
        }
      } yield {
        seqData(Monitor.map(m).name, timeData, tooltip = ToolTip(0))
      }

      val timeStrSeq =
        if (isDailyPsi)
          timeSeq.map(_._1.toString("YY/MM/dd"))
        else
          timeSeq.map(_._1.toString("MM/dd HH:00"))

      val chart = HighchartData(
        scala.collection.immutable.Map("type" -> "column"),
        scala.collection.immutable.Map("text" -> title),
        XAxis(None),
        Seq(YAxis(None, AxisTitle(Some(Some(""))), None)),
        series)

      if (outputType == OutputType.excel) {
        val excelFile = ExcelUtility.exportChartData(chart, Array(0, monitors.length + 1))
        Results.Ok.sendFile(excelFile, fileName = _ =>
          play.utils.UriEncoding.encodePathSegment(chart.title("text") + ".xlsx", "UTF-8"),
          onClose = () => {
            Files.deleteIfExists(excelFile.toPath())
          })
      } else {
        Results.Ok(Json.toJson(chart))
      }

  }

  def overLawStd() = Security.Authenticated {
    implicit request =>
      val userInfo = Security.getUserinfo(request).get
      val group = Group.getGroup(userInfo.groupID).get
      Ok(views.html.overLawStd(group.privilege))
  }

  def overLawStdReport(monitorStr: String, monitorTypeStr: String, startStr: String, endStr: String, outputTypeStr: String) = Security.Authenticated {
    implicit request =>
      val userInfo = Security.getUserinfo(request).get
      val group = Group.getGroup(userInfo.groupID).get
      val outputType = OutputType.withName(outputTypeStr)
      val monitorStrArray = monitorStr.split(':')
      val monitors = monitorStrArray.map {
        Monitor.withName
      }.filter {
        group.privilege.allowedMonitors.contains
      }
      val monitorType = MonitorType.withName(monitorTypeStr)
      val mtCase = MonitorType.map(monitorType)
      val start = DateTime.parse(startStr)
      val end = DateTime.parse(endStr) + 1.day

      if (MonitorTypeAlert.map(monitors(0))(monitorType).std_law.isEmpty)
        BadRequest("法規值未定義!")
      else {
        import scala.collection.mutable.ListBuffer
        val result = ListBuffer[OverLawStdEntry]()
        for {
          m <- monitors
          records = Record.getHourRecords(m, start, end)
          typeRecords = records.map { r => (Record.timeProjection(r), Record.monitorTypeProject2(monitorType)(r)) }
          overLawRecords = typeRecords.filter {
            r =>
              (r._2._1.isDefined && r._2._2.isDefined &&
                MonitorStatus.isNormalStat(r._2._2.get) &&
                r._2._1.get >= MonitorTypeAlert.map(monitors(0))(monitorType).std_law.get)
          }
          overList = overLawRecords.map { r => OverLawStdEntry(m, r._1, r._2._1.get) }
        } {
          result ++= overList
        }

        val output = views.html.overLawStdReport(monitorType, start, end - 1.day, result)
        val title = "超過法規報表"
        outputType match {
          case OutputType.html =>
            Ok(output)
          case OutputType.pdf =>
            Ok.sendFile(creatPdfWithReportHeader(title, output),
              fileName = _ =>
                play.utils.UriEncoding.encodePathSegment(title + start.toString("YYMMdd") + "_" + end.toString("MMdd") + ".pdf", "UTF-8"))
        }
      }
  }

  def effectivePercentage() = Security.Authenticated {
    implicit request =>
      val userInfo = Security.getUserinfo(request).get
      val group = Group.getGroup(userInfo.groupID).get
      Ok(views.html.effectivePercentage(group.privilege))
  }

  def effectivePercentageReport(startStr: String, endStr: String, outputTypeStr: String) = Security.Authenticated {
    implicit request =>
      val userInfo = Security.getUserinfo(request).get
      val group = Group.getGroup(userInfo.groupID).get
      val start = DateTime.parse(startStr)
      val end = DateTime.parse(endStr) + 1.day
      val outputType = OutputType.withName(outputTypeStr)

      val reports =
        for (m <- group.privilege.allowedMonitors) yield {
          Record.getMonitorEffectiveRate(m, start, end)
        }
      val output = views.html.effectivePercentageReport(start, end - 1.day, reports, group.privilege)
      val title = "有效率報表"
      outputType match {
        case OutputType.html =>
          Ok(output)
        case OutputType.pdf =>
          Ok.sendFile(creatPdfWithReportHeader(title, output),
            fileName = _ =>
              play.utils.UriEncoding.encodePathSegment(title + start.toString("YYMMdd") + "_" + end.toString("MMdd") + ".pdf", "UTF-8"))
      }
  }

  def alarm() = Security.Authenticated {
    implicit request =>
      val userInfo = Security.getUserinfo(request).get
      val group = Group.getGroup(userInfo.groupID).get
      Ok(views.html.alarm(group.privilege))
  }

  def alarmReport(monitorStr: String, statusStr: String, startStr: String, endStr: String, outputTypeStr: String) = Security.Authenticated {
    val monitorStrArray = monitorStr.split(':')
    val monitors = monitorStrArray.map {
      Monitor.withName
    }
    val notRepair = statusStr.equalsIgnoreCase("NotRepair")

    val statusFilter = if (statusStr.equalsIgnoreCase("none")) {
      None
    } else {
      Some(statusStr.split(':').toList)
    }
    val start = DateTime.parse(startStr)
    val end = DateTime.parse(endStr) + 1.day
    val outputType = OutputType.withName(outputTypeStr)

    val records =
      if (notRepair)
        Alarm.getNotRepairAlarm(monitors, start, end)
      else
        Alarm.getAlarm(monitors, statusFilter, start, end)

    val output = views.html.alarmReport(start, end, records)
    val title = "警告報表"
    outputType match {
      case OutputType.html =>
        Ok(output)
      case OutputType.pdf =>
        Ok.sendFile(creatPdfWithReportHeader(title, output),
          fileName = _ =>
            play.utils.UriEncoding.encodePathSegment(title + start.toString("YYMMdd") + "_" + end.toString("MMdd") + ".pdf", "UTF-8"))
      case OutputType.excel =>
        val periodStr = s"${start.toString("YYYY/MM/dd")}-${end.toString("MM/dd")}"
        val title = if (notRepair)
          "未立案警報 "
        else
          "警報"
        val excel = ExcelUtility.alarmList(records, s"$title $periodStr")
        Ok.sendFile(excel,
          fileName = _ =>
            play.utils.UriEncoding.encodePathSegment(title + start.toString("YYMMdd") + "_" + end.toString("MMdd") + ".xlsx", "UTF-8"))
    }

  }

  def windRose() = Security.Authenticated {
    implicit request =>
      val userInfo = Security.getUserinfo(request).get
      val group = Group.getGroup(userInfo.groupID).get
      Ok(views.html.windRose(group.privilege))
  }

  def windRoseReport(monitorStr: String, monitorTypeStr: String, nWay: Int, startStr: String, endStr: String, outputTypeStr: String) = Security.Authenticated {
    val monitor = Monitor.withName(monitorStr)
    val monitorType = MonitorType.withName(monitorTypeStr)
    val start = DateTime.parse(startStr, DateTimeFormat.forPattern("YYYY-MM-dd HH:mm"))
    val end = DateTime.parse(endStr, DateTimeFormat.forPattern("YYYY-MM-dd HH:mm"))
    val outputType = OutputType.withName(outputTypeStr)

    assert(nWay == 8 || nWay == 16 || nWay == 32)

    try {
      val level = List(1f, 2f, 5f, 15f)
      val windMap = Record.getWindRose(monitor, start, end, level, nWay)
      val nRecord = windMap.values.map {
        _.length
      }.sum

      val dirMap =
        Map(
          (0 -> "北"), (1 -> "北北東"), (2 -> "東北"), (3 -> "東北東"), (4 -> "東"),
          (5 -> "東南東"), (6 -> "東南"), (7 -> "南南東"), (8 -> "南"),
          (9 -> "南南西"), (10 -> "西南"), (11 -> "西西南"), (12 -> "西"),
          (13 -> "西北西"), (14 -> "西北"), (15 -> "北北西"))

      val dirStrSeq =
        for {
          dir <- 0 to nWay - 1
          dirKey = if (nWay == 8)
            dir * 2
          else if (nWay == 32) {
            if (dir % 2 == 0) {
              dir / 2
            } else
              dir + 16
          } else
            dir
        } yield dirMap.getOrElse(dirKey, "")

      var last = 0f
      val speedLevel = level.flatMap { l =>
        if (l == level.head) {
          last = l
          List(s"< ${l} m/s")
        } else if (l == level.last) {
          val ret = List(s"${last}~${l} m/s", s"> ${l} m/s")
          last = l
          ret
        } else {
          val ret = List(s"${last}~${l} m/s")
          last = l
          ret
        }
      }

      import Realtime._

      val series = for {
        level <- 0 to level.length
      } yield {
        val data =
          for (dir <- 0 to nWay - 1)
            yield Seq(Some(dir.toDouble), Some(windMap(dir)(level).toDouble))

        seqData(speedLevel(level), data, tooltip = ToolTip(MonitorType.map(monitorType).prec))
      }

      val title = "風瑰圖"
      val chart = HighchartData(
        scala.collection.immutable.Map("polar" -> "true", "type" -> "column"),
        scala.collection.immutable.Map("text" -> title),
        XAxis(Some(dirStrSeq)),
        Seq(YAxis(None, AxisTitle(Some(Some(""))), None)),
        series)

      if (outputType == OutputType.excel) {
        val excelFile = ExcelUtility.exportWindRose(chart, Array.fill(nWay)(MonitorType.C211))
        Results.Ok.sendFile(excelFile, fileName = _ =>
          play.utils.UriEncoding.encodePathSegment(chart.title("text") + ".xlsx", "UTF-8"),
          onClose = () => {
            Files.deleteIfExists(excelFile.toPath())
          })
      } else {
        Results.Ok(Json.toJson(chart))
      }
    } catch {
      case e: AssertionError =>
        Logger.error(e.toString())
        BadRequest("無資料")
    }
  }

  def compareLastYear() = Security.Authenticated {
    implicit request =>
      val userInfo = Security.getUserinfo(request).get
      val group = Group.getGroup(userInfo.groupID).get
      Ok(views.html.compareLastYear(group.privilege))
  }

  def compareLastYearChart(monitorStr: String, monitorTypeStr: String, startStr: String, endStr: String) = Security.Authenticated {
    implicit request =>

      val monitor = Monitor.withName(monitorStr)
      val monitorType = MonitorType.withName(monitorTypeStr)
      val mtCase = MonitorType.map(monitorType)
      val start = DateTime.parse(startStr)
      val end = DateTime.parse(endStr) + 1.day

      try {
        val title = s"${Monitor.map(monitor).name} ${MonitorType.map(monitorType).desp}同期比較圖"
        val compareList = Record.getComparedList(monitor, monitorType, start, end, 3)

        import Realtime._

        val series =
          for (yData <- compareList.zipWithIndex)
            yield seqData((start.getYear - yData._2).toString,
              yData._1.map(i => Seq(Some((new DateTime(i._1) + yData._2.year).getMillis.toDouble), i._2._1.map {
                _.toDouble
              })), tooltip = ToolTip(mtCase.prec))

        val c = HighchartData(
          scala.collection.immutable.Map("type" -> "line"),
          scala.collection.immutable.Map("text" -> title),
          XAxis(None),
          Seq(YAxis(None, AxisTitle(Some(Some(""))), None)),
          series.toSeq)

        Results.Ok(Json.toJson(c))

      } catch {
        case ex: Exception =>
          BadRequest(ex.toString())
      }
  }

  def calculateStat() = Security.Authenticated {
    implicit request =>
      val userInfo = Security.getUserinfo(request).get
      val group = Group.getGroup(userInfo.groupID).get
      Ok(views.html.calculateStat(group.privilege))
  }

  def calculateStatReport(monitorStr: String, monitorTypeStr: String, startStr: String, endStr: String, outputTypeStr: String) = Security.Authenticated {
    implicit request =>

      val monitorStrArray = monitorStr.split(':')
      val monitors = monitorStrArray.map {
        Monitor.withName
      }
      val monitorType = MonitorType.withName(monitorTypeStr)
      val start = DateTime.parse(startStr)
      val end = DateTime.parse(endStr) + 1.day
      val outputType = OutputType.withName(outputTypeStr)

      import MonitorStatus._
      val result =
        for {
          m <- monitors
          records = Record.getHourRecords(m, start, end)
          typeRecords = records.map { r => (Record.timeProjection(r), Record.monitorTypeProject2(monitorType)(r)) }
          normalRecords = typeRecords.filter {
            r =>
              val dt = r._1
              val rec = r._2
              rec._1.isDefined && rec._2.isDefined && MonitorStatus.isNormalStat(getTagInfo(rec._2.get).toString)
          }.map(r => (r._1, r._2._1.get))
          len = normalRecords.length if (len > 0)
          avg = normalRecords.map(_._2).sum / len
          sortedRecord = normalRecords.sortBy(_._2)
          max = sortedRecord.last
          min = sortedRecord.head
          dev = sortedRecord.map(r => (r._2 - avg) * (r._2 - avg))
          sd = Math.sqrt(dev.sum / len)
        } yield {
          (m -> PeriodStat(avg, min._2, max._2, sd.toFloat, min._1, max._1))
        }

      val statMap = Map(result: _*)
      val title = "數據統計"
      val output = views.html.calculateStatReport(monitorType, start, end, statMap)
      outputType match {
        case OutputType.html =>
          Ok(output)
        case OutputType.pdf =>
          Ok.sendFile(creatPdfWithReportHeader(title, output),
            fileName = _ =>
              play.utils.UriEncoding.encodePathSegment(title + start.toString("YYMMdd") + "_" + end.toString("MMdd") + ".pdf", "UTF-8"))
      }
  }

  def regression() = Security.Authenticated {
    implicit request =>
      val userInfo = Security.getUserinfo(request).get
      val group = Group.getGroup(userInfo.groupID).get
      Ok(views.html.regression(group.privilege))
  }

  def regressionChart(monitorStr: String, monitorTypeStr: String, startStr: String, endStr: String) = Security.Authenticated {
    implicit request =>

      val monitor = Monitor.withName(monitorStr)
      val monitorType = MonitorType.withName(monitorTypeStr)
      val start = DateTime.parse(startStr)
      val end = DateTime.parse(endStr) + 1.day

      val thisYearRecord = Record.getRegressionData(monitor, monitorType, start, end)

      val title = s"${Monitor.map(monitor).name} ${MonitorType.map(monitorType).desp}趨勢分析"

      val data = thisYearRecord.map(t => Seq(Some(t._1.getTime.toDouble), t._2._1.map {
        _.toDouble
      }))

      val series = Seq(seqData2(start.getYear.toString(), data))

      implicit val hcWrite = Json.writes[RegressionChartData]
      val c = RegressionChartData(
        Map(("type" -> "scatter"),
          ("zoomType" -> "xy")),
        Map("text" -> title),
        XAxis(None),
        YAxis(None, AxisTitle(Some(Some(""))), None),
        series)

      Results.Ok(Json.toJson(c))
  }

  def calibrationQuery = Security.Authenticated {
    implicit request =>
      val userInfo = Security.getUserinfo(request).get
      val group = Group.getGroup(userInfo.groupID).get
      Ok(views.html.calibration(group.privilege))
  }

  import Realtime._

  def calibrationQueryReport(monitorStr: String, startStr: String, endStr: String, outputTypeStr: String) = Security.Authenticated {
    implicit request =>
      val monitor = Monitor.withName(monitorStr)
      val start = DateTime.parse(startStr)
      val end = DateTime.parse(endStr) + 1.day
      val outputType = OutputType.withName(outputTypeStr)
      val result = Calibration.calibrationQueryReport(monitor, start, end)
      val title = "校正報表"
      val output = views.html.calibrationQueryResult(result, title, start, end)

      outputType match {
        case OutputType.html =>
          Ok(output)
        case OutputType.pdf =>
          Ok.sendFile(creatPdfWithReportHeader(title, output),
            fileName = _ =>
              play.utils.UriEncoding.encodePathSegment(Monitor.map(monitor).name + title + start.toString("YYYYMMdd") + "_" +
                end.toString("MMdd") + ".pdf", "UTF-8"))
        case OutputType.excel =>
          val excelFile = ExcelUtility.calibrationDailyReport(title, start, result, true)
          Ok.sendFile(excelFile, fileName = _ =>
            play.utils.UriEncoding.encodePathSegment(title + start.toString("YYMMdd") + "_" + end.toString("YYMMdd") + ".xlsx", "UTF-8"),
            onClose = () => {
              Files.deleteIfExists(excelFile.toPath())
            })

      }
  }

  case class OverLawStdEntry(monitor: Monitor.Value, time: DateTime, value: Float)

  implicit val seqDataWrite = Json.writes[seqData2]

  case class PeriodStat(avg: Float, min: Float, max: Float, sd: Float, minDate: DateTime, maxDate: DateTime)

  case class seqData2(name: String, data: Seq[Seq[Option[Double]]])

  case class RegressionChartData(chart: Map[String, String],
                                 title: Map[String, String],
                                 xAxis: XAxis,
                                 yAxis: YAxis,
                                 series: Seq[seqData2])

}