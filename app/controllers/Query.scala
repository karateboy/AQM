package controllers
import play.api._
import play.api.mvc._
import play.api.Logger
import models._
import com.github.nscala_time.time.Imports._
import models.ModelHelper._
import play.api.libs.json._
import play.api.libs.functional.syntax._
import PdfUtility._
import models.MonitorStatusFilter
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
  val map = Map((Min -> "分"), (TenMin -> "十分"), (Hour -> "小時"), (EightHour -> "八小時"), (Day -> "日"), (Week -> "周"), (Month -> "月"), (Quarter -> "季"))
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
      Ok(views.html.history("/AuditedQueryReport/", group.privilege, true))
  }

  def auditedReport(monitorStr: String, monitorTypeStr: String, recordTypeStr: String, startStr: String, endStr: String, outputTypeStr: String) = Security.Authenticated {
    implicit request =>

      import scala.collection.JavaConverters._
      val monitorStrArray = monitorStr.split(':')
      val monitors = monitorStrArray.map { Monitor.withName }
      val monitorType = MonitorType.withName(monitorTypeStr)
      val tabType = TableType.withName(recordTypeStr)
      val start =
        if (tabType == TableType.Hour)
          DateTime.parse(startStr, DateTimeFormat.forPattern("YYYY-M-dd"))
        else
          DateTime.parse(startStr, DateTimeFormat.forPattern("YYYY-MM-dd HH:mm"))

      val end =
        if (tabType == TableType.Hour)
          DateTime.parse(endStr, DateTimeFormat.forPattern("YYYY-MM-dd"))
        else
          DateTime.parse(endStr, DateTimeFormat.forPattern("YYYY-MM-dd HH:mm"))
      val outputType = OutputType.withName(outputTypeStr)

      var timeSet = Set[DateTime]()
      for (m <- monitors)
        Auditor.auditHourData(m, Monitor.map(m).autoAudit, start, end)

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

  def historyReport(edit: Boolean, monitorStr: String, epaMonitorStr:String, monitorTypeStr: String, recordTypeStr: String, startStr: String, endStr: String, outputTypeStr: String) = Security.Authenticated {
    implicit request =>

      import scala.collection.JavaConverters._
      val monitorStrArray = monitorStr.split(':')
      val monitors = monitorStrArray.map { Monitor.withName }
      val epaMonitors = if (epaMonitorStr.equalsIgnoreCase("None"))
        Array.empty[EpaMonitor.Value]
      else
        epaMonitorStr.split(':').map { EpaMonitor.withName }

      val monitorType = MonitorType.withName(monitorTypeStr)
      val tableType = TableType.withName(recordTypeStr)
      val start =
        if (tableType == TableType.Hour)
          DateTime.parse(startStr, DateTimeFormat.forPattern("YYYY-M-dd"))
        else
          DateTime.parse(startStr, DateTimeFormat.forPattern("YYYY-MM-dd HH:mm"))

      val end =
        if (tableType == TableType.Hour)
          DateTime.parse(endStr, DateTimeFormat.forPattern("YYYY-MM-dd"))
        else
          DateTime.parse(endStr, DateTimeFormat.forPattern("YYYY-MM-dd HH:mm"))

      val outputType = OutputType.withName(outputTypeStr)

      var timeSet = Set[DateTime]()
      val pairs =
        if (tableType == TableType.Hour || tableType == TableType.Min) {
          for {
            m <- monitors
            records = if (tableType == TableType.Hour)
              Record.getHourRecords(m, start, end)
            else
              Record.getMinRecords(m, start, end)
            mtRecords = records.map { rs => (Record.timeProjection(rs).toDateTime, Record.monitorTypeProject2(monitorType)(rs)) }
            timeMap = Map(mtRecords: _*)
          } yield {
            timeSet ++= timeMap.keySet
            (m -> timeMap)
          }
        } else {
          for {
            m <- monitors
            records = Record.getSecRecords(m, start, end)
            mtRecords = records.flatMap { rs => Record.secRecordProject(monitorType)(rs) }
            timeMap = Map(mtRecords: _*)
          } yield {
            timeSet ++= timeMap.keySet
            (m -> timeMap)
          }
        }

      val recordMap = Map(pairs: _*)

      val epa_pairs =
        for{epa <- epaMonitors
          records = Record.getEpaHourRecord(epa, monitorType, start, end)
          timeRecords = records.map { t => t.time -> t.value }
          timeMap = Map(timeRecords: _*)
          }yield{
            epa -> timeMap
          }
      val epaRecordMap = Map(epa_pairs :_*)
      
      val title = "歷史資料查詢"
      val output =
        if (tableType == TableType.SixSec)
          views.html.historyReport(edit, monitors, epaMonitors, monitorType, start, end, timeSet.toList.sorted, recordMap, epaRecordMap, true)
        else
          views.html.historyReport(edit, monitors, epaMonitors, monitorType, start, end, timeSet.toList.sorted, recordMap, epaRecordMap)
      outputType match {
        case OutputType.html =>
          Ok(output)
        case OutputType.pdf =>
          Ok.sendFile(creatPdfWithReportHeader(title, output),
            fileName = _ =>
              play.utils.UriEncoding.encodePathSegment(title + start.toString("YYMMdd") + "_" + end.toString("MMdd") + ".pdf", "UTF-8"))
      }
  }

  def historyTrend = Security.Authenticated {
    implicit request =>
      val userInfo = Security.getUserinfo(request).get
      val group = Group.getGroup(userInfo.groupID).get
      Ok(views.html.historyTrend(group.privilege))
  }

  def trendHelper(monitors: Array[Monitor.Value], epaMonitors: Array[EpaMonitor.Value],
                  monitorTypes: Array[MonitorType.Value], reportUnit: ReportUnit.Value, monitorStatusFilter: MonitorStatusFilter.Value,
                  start: DateTime, end: DateTime) = {
    def statusFilter(data: (DateTime, (Option[Float], Option[String]))): Boolean = {
      if (data._2._2.isEmpty)
        return false

      val stat = data._2._2.get

      MonitorStatusFilter.isMatched(monitorStatusFilter, stat)
    }

    var timeSet = Set[DateTime]()
    val pairs =
      if (reportUnit == ReportUnit.Min || reportUnit == ReportUnit.Hour) {
        val ps =
          for {
            m <- monitors
            records = reportUnit match {
              case ReportUnit.Min =>
                Record.getMinRecords(m, start, end)
              case ReportUnit.Hour =>
                Record.getHourRecords(m, start, end)
            }

            mtPairs = for {
              mt <- monitorTypes
              mtRecords = records.map { rs => (Record.timeProjection(rs).toDateTime, Record.monitorTypeProject2(mt)(rs)) }
              msfRecords = mtRecords.filter(statusFilter)
            } yield {
              val timeMap = Map(msfRecords: _*)
              timeSet ++= timeMap.keySet
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
        reportUnit match {
          case ReportUnit.TenMin =>
            timeSet ++= Report.getPeriods(DateTime.parse(start.toString("YYYY-MM-dd")), DateTime.parse(end.toString("YYYY-MM-dd")) + 1.day, 10.minute)
          case ReportUnit.EightHour =>
            timeSet ++= Report.getPeriods(DateTime.parse(start.toString("YYYY-MM-dd")), DateTime.parse(end.toString("YYYY-MM-dd")) + 1.day, 8.hour)
          case ReportUnit.Day =>
            timeSet ++= Report.getDays(DateTime.parse(start.toString("YYYY-MM-dd")), DateTime.parse(end.toString("YYYY-MM-dd")))
          case ReportUnit.Week =>
            timeSet ++= Report.getWeeks(DateTime.parse(adjustWeekDay(start).toString("YYYY-MM-dd")),
              DateTime.parse(adjustWeekDay(end).toString("YYYY-MM-dd")) + 1.weeks)
          case ReportUnit.Month =>
            timeSet ++= Report.getMonths(DateTime.parse(start.toString("YYYY-MM-01")), DateTime.parse(end.toString("YYYY-MM-01")) + 1.months)
          case ReportUnit.Quarter =>
            timeSet ++= Report.getQuarters(DateTime.parse(s"${start.getYear}-${1 + (start.getMonthOfYear - 1) / 3 * 3}-01"),
              DateTime.parse(s"${end.getYear}-${1 + (end.getMonthOfYear - 1) / 3 * 3}-01") + 3.month)

        }
        ps
      }

    val recordMap = Map(pairs: _*)
    val timeSeq = timeSet.toList.sorted
    import Realtime._

    val windMtv = MonitorType.withName("C212")
    val local_series =
      if (monitorTypes.length > 1 && monitorTypes.contains(windMtv)) {
        //val noWinMt = monitorTypes.filter { _ != windMtv }
        for {
          m <- monitors
          mt <- monitorTypes
          timeData = timeSeq.map(t => recordMap(m)(mt).getOrElse(t, (Some(0f), Some(""))))
          data = timeData.map(_._1.getOrElse(0f))
        } yield {
          if (mt != windMtv)
            seqData(Monitor.map(m).name + "_" + MonitorType.map(mt).desp, data)
          else
            seqData(Monitor.map(m).name + "_" + MonitorType.map(mt).desp, data, 1, Some("scatter"))
        }
      } else {
        for {
          m <- monitors
          mt <- monitorTypes
          timeData = timeSeq.map(t => recordMap(m)(mt).getOrElse(t, (Some(0f), Some(""))))
          data = timeData.map(_._1.getOrElse(0f))
        } yield {
          seqData(Monitor.map(m).name + "_" + MonitorType.map(mt).desp, data)
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
        timeData = timeSeq.map(t => epaRecordMap(m)(mt).getOrElse(t, 0f))
      } yield {
        if (monitorTypes.length > 1 && monitorTypes.contains(windMtv)) {
          if (mt != windMtv)
            seqData(EpaMonitor.map(m).name + "_" + MonitorType.map(mt).desp, timeData)
          else
            seqData(EpaMonitor.map(m).name + "_" + MonitorType.map(mt).desp, timeData, 1, Some("scatter"))
        } else {
          seqData(EpaMonitor.map(m).name + "_" + MonitorType.map(mt).desp, timeData)
        }
      }
    }

    val epa_series = epaSeries()

    val series = local_series ++ epa_series

    val title = {
      val mNames = monitors.map { Monitor.map(_).name }
      val mtNames = monitorTypes.map { MonitorType.map(_).desp }
      mNames.mkString + mtNames.mkString + "趨勢圖"
    }

    val timeStrSeq = timeSeq.map(t =>
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
      })

    def getAxisLines(mtCase: MonitorType) = {
      if (mtCase.std_internal.isEmpty || mtCase.std_law.isEmpty)
        None
      else
        Some(Seq(AxisLine("#0000FF", 2, mtCase.std_internal.get, Some(AxisLineLabel("left", "內控值"))),
          AxisLine("#FF0000", 2, mtCase.std_law.get, Some(AxisLineLabel("right", "法規值")))))
    }

    val chart =
      if (monitorTypes.length == 1) {
        val mtCase = MonitorType.map(monitorTypes(0))

        HighchartData(
          Map("type" -> "line"),
          Map("text" -> title),
          XAxis(Some(timeStrSeq)),
          Seq(YAxis(None, AxisTitle(Some(s"${mtCase.desp} (${mtCase.unit})")), getAxisLines(mtCase))),
          series)
      } else {
        val yAxis =
          if (monitorTypes.length > 1 && monitorTypes.contains(windMtv)) {
            val windMtCase = MonitorType.map(MonitorType.withName("C212"))
            if (monitorTypes.length == 2) {
              val mtCase = MonitorType.map(monitorTypes.filter { !MonitorType.windDirList.contains(_) }(0))

              Seq(YAxis(None, AxisTitle(Some(s"${mtCase.desp} (${mtCase.unit})")), getAxisLines(mtCase)),
                YAxis(None, AxisTitle(Some(s"${windMtCase.desp} (${windMtCase.unit})")), None, true))
            } else {
              Seq(YAxis(None, AxisTitle(None), None), YAxis(None, AxisTitle(Some(s"${windMtCase.desp} (${windMtCase.unit})")), None, true))
            }
          } else {
            Seq(YAxis(None, AxisTitle(None), None))
          }

        HighchartData(
          Map("type" -> "line"),
          Map("text" -> title),
          XAxis(Some(timeStrSeq)),
          yAxis,
          series)
      }

    chart
  }

  def historyTrendChart(monitorStr: String, epaMonitorStr: String, monitorTypeStr: String, 
      reportUnitStr: String, msfStr: String, startStr: String, endStr: String, outputTypeStr:String) = Security.Authenticated {
    implicit request =>
      import scala.collection.JavaConverters._
      val monitorStrArray = monitorStr.split(':')
      val monitors = monitorStrArray.map { Monitor.withName }
      val epaMonitors = if (epaMonitorStr.equalsIgnoreCase("None"))
        Array.empty[EpaMonitor.Value]
      else
        epaMonitorStr.split(':').map { EpaMonitor.withName }
      val monitorTypeStrArray = monitorTypeStr.split(':')
      val monitorTypes = monitorTypeStrArray.map { MonitorType.withName }
      val reportUnit = ReportUnit.withName(reportUnitStr)
      val monitorStatusFilter = MonitorStatusFilter.withName(msfStr)
      val (start, end) = 
        if(reportUnit == ReportUnit.Min || reportUnit == ReportUnit.TenMin){
          (DateTime.parse(startStr, DateTimeFormat.forPattern("YYYY-MM-dd HH:mm")), 
              DateTime.parse(endStr, DateTimeFormat.forPattern("YYYY-MM-dd HH:mm")))
        }else if(reportUnit == ReportUnit.Month || reportUnit == ReportUnit.Quarter){
          (DateTime.parse(startStr, DateTimeFormat.forPattern("YYYY-M")), 
              DateTime.parse(endStr, DateTimeFormat.forPattern("YYYY-M")))
        }else
        {
          (DateTime.parse(startStr, DateTimeFormat.forPattern("YYYY-MM-dd")), 
              DateTime.parse(endStr, DateTimeFormat.forPattern("YYYY-MM-dd")))          
        }
      val outputType = OutputType.withName(outputTypeStr)


      val chart = trendHelper(monitors, epaMonitors, monitorTypes, reportUnit, monitorStatusFilter, start, end)

      if(outputType == OutputType.excel){
         val excelFile = ExcelUtility.exportChartData(chart, monitorTypes)
         Ok.sendFile(excelFile, fileName = _ =>
          play.utils.UriEncoding.encodePathSegment(chart.title("text") + ".xlsx", "UTF-8"),
          onClose = () => { Files.deleteIfExists(excelFile.toPath()) })
      }else{
         Results.Ok(Json.toJson(chart)) 
      }
  }

  
  def psiTrend = Security.Authenticated {
    implicit request =>
      val userInfo = Security.getUserinfo(request).get
      val group = Group.getGroup(userInfo.groupID).get
      Ok(views.html.psiTrend(group.privilege))
  }

  def psiTrendChart(monitorStr: String, startStr: String, endStr: String, isDailyPsi: Boolean, outputTypeStr:String) = Security.Authenticated {
    implicit request =>
      import scala.collection.JavaConverters._
      val monitorStrArray = monitorStr.split(':')
      val monitors = monitorStrArray.map { Monitor.withName }
      val start = DateTime.parse(startStr)
      val end = DateTime.parse(endStr) + 1.day
      val outputType = OutputType.withName(outputTypeStr)

      import scala.collection.mutable.Map
      def getPsiMap(m: Monitor.Value) = {
        import models.Realtime._
        var current = start

        val map = Map[DateTime, Float]()
        while (current < end) {
          if (isDailyPsi) {
            val v = getMonitorDailyPSI(m, current)
            val psi = v.psi.getOrElse(0f)
            map += (current -> psi)
            current += 1.day
          } else {
            val v = getRealtimePSI(current, List(m))
            val psi = v(m)._1.getOrElse(0f)
            map += (current -> psi)
            current += 1.hour
          }
        }
        map
      }

      val monitorPsiMap = Map[Monitor.Value, Map[DateTime, Float]]()
      for (m <- monitors) {
        monitorPsiMap += (m -> getPsiMap(m))
      }

      val title = "PSI歷史趨勢圖"
      val timeSeq = monitorPsiMap.values.head.keys.toList.sorted
      import Realtime._

      val series = for {
        m <- monitors
        timeData = timeSeq.map(t => monitorPsiMap(m)(t))
      } yield {
        seqData(Monitor.map(m).name, timeData)
      }

      val timeStrSeq =
        if (isDailyPsi)
          timeSeq.map(_.toString("YY/MM/dd"))
        else
          timeSeq.map(_.toString("MM/dd HH:00"))

      val chart = HighchartData(
        scala.collection.immutable.Map("type" -> "column"),
        scala.collection.immutable.Map("text" -> title),
        XAxis(Some(timeStrSeq)),
        Seq(YAxis(None, AxisTitle(Some("")), None)),
        series)

      
      if(outputType == OutputType.excel){
          val excelFile = ExcelUtility.exportChartData(chart, Array(2))
          Results.Ok.sendFile(excelFile, fileName = _ =>
          play.utils.UriEncoding.encodePathSegment(chart.title("text") + ".xlsx", "UTF-8"),
          onClose = () => { Files.deleteIfExists(excelFile.toPath()) })
      }else{
          Results.Ok(Json.toJson(chart))  
      }   

  }

  def overLawStd() = Security.Authenticated {
    implicit request =>
      val userInfo = Security.getUserinfo(request).get
      val group = Group.getGroup(userInfo.groupID).get
      Ok(views.html.overLawStd(group.privilege))
  }

  case class OverLawStdEntry(monitor: Monitor.Value, time: DateTime, value: Float)
  def overLawStdReport(monitorStr: String, monitorTypeStr: String, startStr: String, endStr: String, outputTypeStr: String) = Security.Authenticated {
    implicit request =>
      val userInfo = Security.getUserinfo(request).get
      val group = Group.getGroup(userInfo.groupID).get
      val outputType = OutputType.withName(outputTypeStr)

      import scala.collection.JavaConverters._
      val monitorStrArray = monitorStr.split(':')
      val monitors = monitorStrArray.map { Monitor.withName }.filter { group.privilege.allowedMonitors.contains }
      val monitorType = MonitorType.withName(monitorTypeStr)
      val mtCase = MonitorType.map(monitorType)
      val start = DateTime.parse(startStr)
      val end = DateTime.parse(endStr) + 1.day

      assert(mtCase.std_law.isDefined)

      import models.Record._
      import scala.collection.mutable.ListBuffer
      val result = ListBuffer[OverLawStdEntry]()
      for {
        m <- monitors
        records = Record.getHourRecords(m, start, end)
        typeRecords = records.map { r => (Record.timeProjection(r), Record.monitorTypeProject2(monitorType)(r)) }
        overLawRecords = typeRecords.filter {
          r => (r._2._1.isDefined && r._2._1.get > mtCase.std_law.get)
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
    val monitors = monitorStrArray.map { Monitor.withName }
    val statusFilter = if (statusStr.equalsIgnoreCase("none")) {
      None
    } else {
      Some(statusStr.split(':').toList)
    }
    val start = DateTime.parse(startStr)
    val end = DateTime.parse(endStr) + 1.day
    val outputType = OutputType.withName(outputTypeStr)

    val records = Alarm.getAlarm(monitors, statusFilter, start, end)

    val output = views.html.alarmReport(start, end, records)
    val title = "警告報表"
    outputType match {
      case OutputType.html =>
        Ok(output)
      case OutputType.pdf =>
        Ok.sendFile(creatPdfWithReportHeader(title, output),
          fileName = _ =>
            play.utils.UriEncoding.encodePathSegment(title + start.toString("YYMMdd") + "_" + end.toString("MMdd") + ".pdf", "UTF-8"))
    }

  }

  def windRose() = Security.Authenticated {
    implicit request =>
      val userInfo = Security.getUserinfo(request).get
      val group = Group.getGroup(userInfo.groupID).get
      Ok(views.html.windRose(group.privilege))
  }

  def windRoseReport(monitorStr: String, monitorTypeStr: String, nWay: Int, startStr: String, endStr: String, outputTypeStr:String) = Security.Authenticated {
    val monitor = Monitor.withName(monitorStr)
    val monitorType = MonitorType.withName(monitorTypeStr)
    val start = DateTime.parse(startStr)
    val end = DateTime.parse(endStr) + 1.day
    val outputType = OutputType.withName(outputTypeStr)
    
    assert(nWay == 8 || nWay == 16 || nWay == 32)

    try {
      val level = List(1f, 2f, 5f, 15f)
      val windMap = Record.getWindRose(monitor, start, end, level, nWay)
      val nRecord = windMap.values.map { _.length }.sum

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
            yield windMap(dir)(level)

        seqData(speedLevel(level), data)
      }

      val title = "風瑰圖"
      val chart = HighchartData(
        scala.collection.immutable.Map("polar" -> "true", "type" -> "column"),
        scala.collection.immutable.Map("text" -> title),
        XAxis(Some(dirStrSeq)),
        Seq(YAxis(None, AxisTitle(Some("")), None)),
        series)

      if(outputType == OutputType.excel){
          val excelFile = ExcelUtility.exportChartData(chart, Array.fill(nWay)(MonitorType.C211))
          Results.Ok.sendFile(excelFile, fileName = _ =>
          play.utils.UriEncoding.encodePathSegment(chart.title("text") + ".xlsx", "UTF-8"),
          onClose = () => { Files.deleteIfExists(excelFile.toPath()) })
      }else{
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
      import scala.collection.JavaConverters._
      val monitor = Monitor.withName(monitorStr)
      val monitorType = MonitorType.withName(monitorTypeStr)
      val mtCase = MonitorType.map(monitorType)
      val start = DateTime.parse(startStr)
      val end = DateTime.parse(endStr) + 1.day

      try {
        val (thisYearRecord, lastYearRecord) = Record.getLastYearCompareList(monitor, monitorType, start, end)

        val title = s"${Monitor.map(monitor).name} ${MonitorType.map(monitorType).desp}同期比較圖"

        val timeStrSeq = thisYearRecord.map(_._1.toDateTime.toString("MM-dd hh:00"))
        val (t1, v1) = thisYearRecord.unzip
        val (t2, v2) = lastYearRecord.unzip

        import Realtime._

        val series = Seq(seqData(start.getYear.toString(), v1), seqData((start - 1.year).getYear.toString(), v2))

        val c = HighchartData(
          scala.collection.immutable.Map("type" -> "line"),
          scala.collection.immutable.Map("text" -> title),
          XAxis(Some(timeStrSeq)),
          Seq(YAxis(None, AxisTitle(Some("")), None)),
          series)

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

  case class PeriodStat(avg: Float, min: Float, max: Float, sd: Float, minDate:DateTime, maxDate:DateTime)
  def calculateStatReport(monitorStr: String, monitorTypeStr: String, startStr: String, endStr: String, outputTypeStr: String) = Security.Authenticated {
    implicit request =>
      import scala.collection.JavaConverters._
      val monitorStrArray = monitorStr.split(':')
      val monitors = monitorStrArray.map { Monitor.withName }
      val monitorType = MonitorType.withName(monitorTypeStr)
      val start = DateTime.parse(startStr)
      val end = DateTime.parse(endStr) + 1.day
      val outputType = OutputType.withName(outputTypeStr)

      import models.Record._
      import MonitorStatus._
      import scala.collection.mutable.ListBuffer
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
          len = normalRecords.length if(len >0)
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

  import Realtime._

  case class seqData2(name: String, data: Seq[Seq[Float]])
  case class RegressionChartData(chart: Map[String, String],
                                 title: Map[String, String],
                                 xAxis: XAxis,
                                 yAxis: YAxis,
                                 series: Seq[seqData2])

  implicit val seqDataWrite = Json.writes[seqData2]
  implicit val hcWrite = Json.writes[RegressionChartData]

  def regressionChart(monitorStr: String, monitorTypeStr: String, startStr: String, endStr: String) = Security.Authenticated {
    implicit request =>
      import scala.collection.JavaConverters._
      val monitor = Monitor.withName(monitorStr)
      val monitorType = MonitorType.withName(monitorTypeStr)
      val mtCase = MonitorType.map(monitorType)
      val start = DateTime.parse(startStr)
      val end = DateTime.parse(endStr) + 1.day

      val thisYearRecord = Record.getRegressionData(monitor, monitorType, start, end)

      val title = s"${Monitor.map(monitor).name} ${MonitorType.map(monitorType).desp}趨勢分析"

      val timeStrSeq = thisYearRecord.map(_._1.toDateTime.toString("MM-dd hh:00"))
      val (t1, v1) = thisYearRecord.unzip

      val v2 = v1.zipWithIndex
      val v3 = v2.map { v => Seq(v._2, v._1) }

      val series = Seq(seqData2(start.getYear.toString(), v3))

      val c = RegressionChartData(
        Map(("type" -> "scatter"),
          ("zoomType" -> "xy")),
        Map("text" -> title),
        XAxis(Some(timeStrSeq)),
        YAxis(None, AxisTitle(Some("")), None),
        series)

      Results.Ok(Json.toJson(c))
  }

  def calibrationQuery = Security.Authenticated {
    implicit request =>
      val userInfo = Security.getUserinfo(request).get
      val group = Group.getGroup(userInfo.groupID).get
      Ok(views.html.calibration(group.privilege))
  }

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
      }
  }

}