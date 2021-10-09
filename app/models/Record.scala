package models
import java.sql.Date
import java.sql.Timestamp
import scalikejdbc._
import play.api._
import com.github.nscala_time.time.Imports._
import models.ModelHelper._
import models._

case class Stat(
    avg: Option[Float],
    min: Option[Float],
    max: Option[Float],
    count: Int,
    total: Int,
    overCount: Int) {
  val effectPercent = if(total != 0) Some(count.toFloat*100 / total) else None
  val overPercent = if(total != 0) Some(overCount.toFloat*100 / total) else None
}

case class MonitorTypeRecord(monitorType: MonitorType.Value, dataList: List[(Timestamp, Option[Float], Option[String])], stat: Stat)
case class DailyReport(
  typeList: Array[MonitorTypeRecord])

object TableType extends Enumeration {
  val SixSec = Value("SixSec")
  val Min = Value("Min")
  val Hour = Value("Hour")
  val EightHour = Value("EightHour")
  val map = Map(SixSec -> "六秒資料", Min -> "分鐘資料", Hour -> "小時資料", EightHour->"八小時資料")
}

object Record {
  case class HourRecord(
    name: String,
    date: Timestamp,
    chk: Option[String] = None,
    tsp: Option[Float]= None,
    tsp_stat: Option[String]= None,
    pm10: Option[Float]= None,
    pm10_stat: Option[String]= None,
    pm25: Option[Float]= None,
    pm25_stat: Option[String]= None,
    s: Option[Float]= None,
    s_stat: Option[String]= None,
    so2: Option[Float]= None,
    so2_stat: Option[String]= None,
    nox: Option[Float]= None,
    nox_stat: Option[String]= None,
    co: Option[Float]= None,
    co_stat: Option[String]= None,
    var o3: Option[Float]= None,
    var o3_stat: Option[String]= None,
    thc: Option[Float]= None,
    thc_stat: Option[String]= None,
    ammonia: Option[Float]= None,
    ammonia_stat: Option[String]= None,
    noy: Option[Float]= None,
    noy_stat: Option[String]= None,
    noy_no: Option[Float]= None,
    noy_no_stat: Option[String]= None,
    nh3: Option[Float]= None,
    nh3_stat: Option[String]= None,
    no: Option[Float]= None,
    no_stat: Option[String]= None,
    ch4: Option[Float]= None,
    ch4_stat: Option[String]= None,
    monitor_humid: Option[Float]= None,
    monitor_humid_stat: Option[String]= None,
    monitor_temp: Option[Float]= None,
    monitor_temp_stat: Option[String]= None,
    no2: Option[Float]= None,
    no2_stat: Option[String]= None,
    nmhc: Option[Float]= None,
    nmhc_stat: Option[String]= None,
    wind_speed: Option[Float]= None,
    wind_speed_stat: Option[String]= None,
    wind_dir: Option[Float]= None,
    wind_dir_stat: Option[String]= None,
    rain: Option[Float]= None,
    rain_stat: Option[String]= None,
    temp: Option[Float]= None,
    temp_stat: Option[String]= None,
    humid: Option[Float]= None,
    humid_stat: Option[String]= None,
    air_pressure: Option[Float]= None,
    air_pressure_stat: Option[String]= None,
    noy_dif: Option[Float]= None,
    noy_dif_stat: Option[String]= None,
    nh3_nt: Option[Float]= None,
    nh3_nt_stat: Option[String]= None,
    nh3_nox: Option[Float]= None,
    nh3_nox_stat: Option[String]= None,
    nh3_no: Option[Float]= None,
    nh3_no_stat: Option[String]= None,
    nh3_no2: Option[Float]= None,
    nh3_no2_stat: Option[String]= None,
    h2s_cs: Option[Float]= None,
    h2s_cs_stat: Option[String]= None,
    h2s_so2: Option[Float]= None,
    h2s_so2_stat: Option[String]= None,
    h2s: Option[Float]= None,
    h2s_stat: Option[String]= None)

  case class SixSecRecord(
    monitor: Monitor.Value,
    time: DateTime,
    winSpeed: Seq[Option[Float]],
    winSpeed_stat: Seq[Option[String]],
    winDir: Seq[Option[Float]],
    winDir_stat: Seq[Option[String]])

  type MinRecord = HourRecord

  def emptySixSecRecord(m: Monitor.Value, t: DateTime, status: String) = SixSecRecord(m, t,
    List.fill(10)(Some(0f)),
    List.fill(10)(Some(MonitorStatus.DATA_LOSS_STAT)),
    List.fill(10)(Some(0f)),
    List.fill(10)(Some(MonitorStatus.DATA_LOSS_STAT)))

  def mapper(rs: WrappedResultSet) = {
    def stringOpt(idx: Int) = {
      try {
        rs.stringOpt(idx)
      } catch {
        case ex: ResultSetExtractorException =>
          None
      }
    }
    def floatOpt(idx: Int) = {
      try {
        rs.floatOpt(idx)
      } catch {
        case ex: ResultSetExtractorException =>
          None
      }
    }

    HourRecord(rs.string(1), rs.timestamp(2), rs.stringOpt(3), rs.floatOpt(4), rs.stringOpt(5),
      rs.floatOpt(6), rs.stringOpt(7), rs.floatOpt(8), rs.stringOpt(9), rs.floatOpt(10),
      rs.stringOpt(11), rs.floatOpt(12), rs.stringOpt(13), rs.floatOpt(14), rs.stringOpt(15),
      rs.floatOpt(16), rs.stringOpt(17), rs.floatOpt(18), rs.stringOpt(19), rs.floatOpt(20),
      rs.stringOpt(21), rs.floatOpt(22), rs.stringOpt(23), rs.floatOpt(24), rs.stringOpt(25),
      rs.floatOpt(26), rs.stringOpt(27), rs.floatOpt(28), rs.stringOpt(29), rs.floatOpt(30),
      rs.stringOpt(31), rs.floatOpt(32), rs.stringOpt(33), rs.floatOpt(34), rs.stringOpt(35),
      rs.floatOpt(36), rs.stringOpt(37), rs.floatOpt(38), rs.stringOpt(39), rs.floatOpt(40),
      rs.stringOpt(41), rs.floatOpt(42), rs.stringOpt(43), rs.floatOpt(44), rs.stringOpt(45),
      rs.floatOpt(46), rs.stringOpt(47), rs.floatOpt(48), rs.stringOpt(49), rs.floatOpt(50),
      rs.stringOpt(51), rs.floatOpt(52), rs.stringOpt(53), floatOpt(54), stringOpt(55),
      floatOpt(56), stringOpt(57), floatOpt(58), stringOpt(59), floatOpt(60),
      stringOpt(61), floatOpt(62), stringOpt(63), floatOpt(64), stringOpt(65),
      floatOpt(66), stringOpt(67), floatOpt(68), stringOpt(69))
  }

  def sixSecMapper(rs: WrappedResultSet) = {
    val windSpeed =
      for (loc <- 0 to 9) yield {
        rs.floatOpt(4 + loc * 2)
      }

    val windSpeed_stat =
      for (loc <- 0 to 9) yield {
        rs.stringOpt(5 + loc * 2)
      }

    val windDir =
      for (loc <- 0 to 9) yield {
        rs.floatOpt(24 + loc * 2)
      }

    val windDir_stat =
      for (loc <- 0 to 9) yield {
        rs.stringOpt(24 + loc * 2)
      }

    SixSecRecord(Monitor.withName(rs.string(1)), rs.timestamp(2), windSpeed, windSpeed_stat, windDir, windDir_stat)
  }

  def get8HourRecords(monitor: Monitor.Value, startTime: DateTime, endTime: DateTime)(implicit session: DBSession = AutoSession): List[Record.HourRecord] = {
    val start: Timestamp = startTime
    val end: Timestamp = endTime
    val monitorName = monitor.toString()

    if (startTime == endTime)
      List.empty[HourRecord]
    else {
      val tab_name = getTabName(TableType.EightHour, startTime.getYear)
      val result = sql"""
        Select *
        From ${tab_name}
        Where DP_NO=${monitorName} and M_DateTime >= ${start} and M_DateTime < ${end}
        ORDER BY M_DateTime ASC
      """.map { rs=>
        HourRecord(name = rs.string(1), rs.timestamp(2),
          o3 = Some(rs.float(3)), o3_stat = Some(rs.string(4)) ) }.list().apply()
      if (startTime.getYear == endTime.getYear)
        result
      else
        result ++ get8HourRecords(monitor, DateTime.parse(s"${startTime.getYear + 1}-1-1"), endTime)
    }
  }
  def getHourRecords(monitor: Monitor.Value, startTime: DateTime, endTime: DateTime)(implicit session: DBSession = AutoSession): List[Record.HourRecord] = {
    val start: Timestamp = startTime
    val end: Timestamp = endTime
    val monitorName = monitor.toString()

    if (startTime == endTime)
      List.empty[HourRecord]
    else {
      val tab_name = getTabName(TableType.Hour, startTime.getYear)
      val result = sql"""
        Select * 
        From ${tab_name}
        Where DP_NO=${monitorName} and M_DateTime >= ${start} and M_DateTime < ${end}
        ORDER BY M_DateTime ASC
      """.map { mapper }.list().apply()
      if (startTime.getYear == endTime.getYear)
        result
      else
        result ++ getHourRecords(monitor, DateTime.parse(s"${startTime.getYear + 1}-1-1"), endTime)
    }
  }

  def getUncheckedHourRecords(monitor: Monitor.Value, startTime: DateTime, endTime: DateTime)(implicit session: DBSession = AutoSession): List[Record.HourRecord] = {
    val start: Timestamp = startTime
    val end: Timestamp = endTime
    val monitorName = monitor.toString()

    if (startTime == endTime)
      List.empty[HourRecord]
    else {

      val tab_name = getTabName(TableType.Hour, startTime.getYear)
      val result = sql"""
        Select * 
        From ${tab_name}
        Where DP_NO=${monitorName} and M_DateTime >= ${start} and M_DateTime < ${end} and CHK is Null
        ORDER BY M_DateTime ASC
      """.map { mapper }.list().apply()
      if (startTime.getYear == endTime.getYear)
        result
      else
        result ++ getUncheckedHourRecords(monitor, DateTime.parse(s"${startTime.getYear + 1}-1-1"), endTime)
    }
  }

  def getInvalidHourRecords(monitor: Monitor.Value, startTime: DateTime, endTime: DateTime)(implicit session: DBSession = AutoSession): List[Record.HourRecord] = {
    val start: Timestamp = startTime
    val end: Timestamp = endTime

    if (startTime == endTime)
      List.empty[HourRecord]
    else {

      val monitorName = monitor.toString()
      val tab_name = getTabName(TableType.Hour, startTime.getYear)
      val result = sql"""
        Select * 
        From ${tab_name}
        Where DP_NO=${monitorName} and M_DateTime >= ${start} and M_DateTime < ${end} and CHK = 'BAD'
        ORDER BY M_DateTime ASC
      """.map { mapper }.list().apply()
      if (startTime.getYear == endTime.getYear)
        result
      else
        result ++ getInvalidHourRecords(monitor, DateTime.parse(s"${startTime.getYear + 1}-1-1"), endTime)
    }
  }

  def getMinRecords(monitor: Monitor.Value, startTime: DateTime, endTime: DateTime)(implicit session: DBSession = AutoSession): List[Record.HourRecord] = {
    val start: Timestamp = startTime
    val end: Timestamp = endTime

    if (startTime == endTime)
      List.empty[HourRecord]
    else {

      val monitorName = monitor.toString()
      val tab_name = getTabName(TableType.Min, startTime.getYear)
      val result = sql"""
        Select * 
        From ${tab_name}
        Where DP_NO=${monitorName} and M_DateTime >= ${start} and M_DateTime < ${end}
        ORDER BY M_DateTime ASC
      """.map { mapper }.list().apply()
      if (startTime.getYear == endTime.getYear)
        result
      else
        result ++ getMinRecords(monitor, DateTime.parse(s"${startTime.getYear + 1}-1-1"), endTime)
    }
  }

  def getSecRecords(monitor: Monitor.Value, startTime: DateTime, endTime: DateTime)(implicit session: DBSession = AutoSession): List[Record.SixSecRecord] = {
    val start: Timestamp = startTime
    val end: Timestamp = endTime
    if (startTime == endTime)
      List.empty[SixSecRecord]
    else {

      val monitorName = monitor.toString()
      val tab_name = getTabName(TableType.SixSec, startTime.getYear)
      val result = sql"""
        Select * 
        From ${tab_name}
        Where DP_NO=${monitorName} and M_DateTime >= ${start} and M_DateTime < ${end}
        ORDER BY M_DateTime ASC
      """.map { sixSecMapper }.list().apply()
      if (startTime.getYear == endTime.getYear)
        result
      else
        result ++ getSecRecords(monitor, DateTime.parse(s"${startTime.getYear + 1}-1-1"), endTime)
    }
  }

  def secRecordProject(mt: MonitorType.Value) =
    (rs: SixSecRecord) => {
      assert(mt == MonitorType.withName("C211") || mt == MonitorType.withName("C212"))
      val start = rs.time
      val values =
        for (i <- 0 to 9) yield {
          if (mt == MonitorType.withName("C211")) {
            (start + (6 * i).second, (rs.winSpeed(i), rs.winSpeed_stat(i)))
          } else {
            (start + (6 * i).second, (rs.winDir(i), rs.winDir_stat(i)))
          }
        }
      values.toList
    }

  val timeProjection: (HourRecord => Timestamp) = {
    rs => rs.date
  }

  def getFieldName(tabType: TableType.Value, mt: MonitorType.Value, recordTime: Timestamp) = {
    val name = mt.toString
    val head = name.charAt(0)
    val tail = name.substring(2)

    tabType match {
      case TableType.Hour =>
        SQLSyntax.createUnsafely(s"${head}5${tail}s")
      case TableType.Min =>
        SQLSyntax.createUnsafely(s"${head}2${tail}s")
      case TableType.SixSec =>
        val idx = recordTime.toDateTime.getSecondOfMinute / 6
        SQLSyntax.createUnsafely(s"${head}9${tail}_${idx}s")
    }

  }

  def updateRecordStatus(tabType: TableType.Value, monitor: Monitor.Value, monitorType: MonitorType.Value, mill: Long, status: String)(implicit session: DBSession = AutoSession) = {
    val recordTime = new Timestamp(mill)
    val monitorName = monitor.toString()
    val tab_name = getTabName(tabType, recordTime.toDateTime.getYear)
    val field_name = getFieldName(tabType, monitorType, recordTime)
    if (tabType != TableType.SixSec) {
      sql""" 
          Update ${tab_name}
          Set ${field_name}=${status}
          Where DP_NO=${monitorName} and ${field_name} IS NOT NULL and M_DateTime = ${recordTime}        
        """.update.apply
    } else {
      val rt = recordTime.toDateTime()
      val rt1 = rt - rt.getSecondOfDay.second
      val tt: Timestamp = rt1

      sql""" 
          Update ${tab_name}
          Set ${field_name}=${status}
          Where DP_NO=${monitorName} and ${field_name} IS NOT NULL and M_DateTime = ${tt}        
        """.update.apply
    }
  }

  lazy val monitorTypeProject2: Map[MonitorType.Value, HourRecord => (Option[Float], Option[String])] = Map(
    MonitorType.withName("A213") -> (rs => (rs.tsp, rs.tsp_stat)),
    MonitorType.withName("A214") -> (rs => (rs.pm10, rs.pm10_stat)),
    MonitorType.withName("A215") -> (rs => (rs.pm25, rs.pm25_stat)),
    MonitorType.withName("A221") -> (rs => (rs.s, rs.s_stat)),
    MonitorType.withName("A222") -> (rs => (rs.so2, rs.so2_stat)),
    MonitorType.withName("A223") -> (rs => (rs.nox, rs.nox_stat)),
    MonitorType.withName("A224") -> (rs => (rs.co, rs.co_stat)),
    MonitorType.withName("A225") -> (rs => (rs.o3, rs.o3_stat)),
    MonitorType.withName("A226") -> (rs => (rs.thc, rs.thc_stat)),
    MonitorType.withName("A229") -> (rs => (rs.ammonia, rs.ammonia_stat)),
    MonitorType.withName("A232") -> (rs => (rs.noy, rs.noy_stat)),
    MonitorType.withName("A233") -> (rs => (rs.noy_no, rs.noy_no_stat)),
    MonitorType.withName("A235") -> (rs => (rs.nh3, rs.nh3_stat)),
    MonitorType.withName("A283") -> (rs => (rs.no, rs.no_stat)),
    MonitorType.withName("A286") -> (rs => (rs.ch4, rs.ch4_stat)),
    MonitorType.withName("A288") -> (rs => (rs.monitor_humid, rs.monitor_humid_stat)),
    MonitorType.withName("A289") -> (rs => (rs.monitor_temp, rs.monitor_temp_stat)),
    MonitorType.withName("A293") -> (rs => (rs.no2, rs.no2_stat)),
    MonitorType.withName("A296") -> (rs => (rs.nmhc, rs.nmhc_stat)),
    MonitorType.withName("C211") -> (rs => (rs.wind_speed, rs.wind_speed_stat)),
    MonitorType.withName("C212") -> (rs => (rs.wind_dir, rs.wind_dir_stat)),
    MonitorType.withName("C213") -> (rs => (rs.rain, rs.rain_stat)),
    MonitorType.withName("C214") -> (rs => (rs.temp, rs.temp_stat)),
    MonitorType.withName("C215") -> (rs => (rs.humid, rs.humid_stat)),
    MonitorType.withName("C216") -> (rs => (rs.air_pressure, rs.air_pressure_stat)),

    //New MT
    MonitorType.withName("A242") -> (rs => (rs.noy_dif, rs.noy_dif_stat)),
    MonitorType.withName("A236") -> (rs => (rs.nh3_nt, rs.nh3_nt_stat)),
    MonitorType.withName("A237") -> (rs => (rs.nh3_nox, rs.nh3_nox_stat)),
    MonitorType.withName("A238") -> (rs => (rs.nh3_no, rs.nh3_no_stat)),
    MonitorType.withName("A239") -> (rs => (rs.nh3_no2, rs.nh3_no2_stat)),
    MonitorType.withName("A244") -> (rs => (rs.h2s_cs, rs.h2s_cs_stat)),
    MonitorType.withName("A245") -> (rs => (rs.h2s_so2, rs.h2s_so2_stat)),
    MonitorType.withName("A234") -> (rs => (rs.h2s, rs.h2s_stat)))

  def emptyRecord(monitor: String, start: DateTime) = {
    HourRecord(
      monitor,
      start,
      None, None, None, None, None, None, None, None, None, None,
      None, None, None, None, None, None, None, None, None, None,
      None, None, None, None, None, None, None, None, None, None,
      None, None, None, None, None, None, None, None, None, None,
      None, None, None, None, None, None, None, None, None, None,
      None, None, None, None, None, None, None, None, None, None,
      None, None, None, None, None, None, None)
  }

  case class RecordValidationReport(start: DateTime, end: DateTime,
                                    hourReport: Map[Monitor.Value, Int],
                                    minReport: Map[Monitor.Value, Int],
                                    sixSecReport: Map[Monitor.Value, Int])

  def getRecordValidationReport(start: DateTime, end: DateTime) = {
    DB readOnly { implicit session =>
      val tab_name = getTabName(TableType.Hour, start.getYear)
      val hrRecords =
        sql"""
        SELECT DP_NO, count(DP_NO)
        FROM ${tab_name}
        Where M_DateTime >= ${start} and M_DateTime < ${end}
        GROUP BY DP_NO
      """.map { rs => (Monitor.withName(rs.string(1)), rs.int(2)) }.list().apply()

      val hourReport = Map(hrRecords: _*)

      val mintab_name = getTabName(TableType.Min, start.getYear)
      val minRecords =
        sql"""
        SELECT DP_NO, count(DP_NO)
        FROM ${mintab_name}
        Where M_DateTime >= ${start} and M_DateTime < ${end}
        GROUP BY DP_NO
      """.map { rs => (Monitor.withName(rs.string(1)), rs.int(2)) }.list().apply()

      val minReport = Map(minRecords: _*)

      val sectab_name = getTabName(TableType.SixSec, start.getYear)
      val sixSecRecords =
        sql"""
        SELECT DP_NO, count(DP_NO)
        FROM ${sectab_name}
        Where M_DateTime >= ${start} and M_DateTime < ${end}
        GROUP BY DP_NO
      """.map { rs => (Monitor.withName(rs.string(1)), rs.int(2)) }.list().apply()

      val sixSecReport = Map(sixSecRecords: _*)

      RecordValidationReport(start, end, hourReport, minReport, sixSecReport)
    }
  }

  def getDays(current: DateTime, endTime: DateTime): List[DateTime] = {
    if (current == endTime)
      Nil
    else
      current :: getDays(current + 1.days, endTime)
  }

  def windAvg(sum_sin: Double, sum_cos: Double) = {
    val degree = Math.toDegrees(Math.atan2(sum_sin, sum_cos)).toFloat
    if (degree >= 0)
      degree
    else
      degree + 360
  }

  type RecordT = (Timestamp, Option[Float], Option[String])
  def windAvg(windSpeed: List[RecordT], windDir: List[RecordT]): Float = {
    def validFilter(t: RecordT) = {
      if (t._2.isEmpty)
        false
      else {
        t._3 match {
          case Some(stat) => MonitorStatus.isNormalStat(stat)
          case _          => false
        }
      }
    }

    if (windSpeed.length != windDir.length)
      Logger.error(s"windSpeed #=${windSpeed.length} windDir #=${windDir.length}")

    val windRecord = windSpeed.zip(windDir)
    val validWind = windRecord.filter(t => validFilter(t._1) && validFilter(t._2)).map(r => (r._1._2.get, r._2._2.get))
    val wind_sin = validWind.map(v => v._1 * Math.sin(Math.toRadians(v._2))).sum
    val wind_cos = validWind.map(v => v._1 * Math.cos(Math.toRadians(v._2))).sum
    windAvg(wind_sin, wind_cos)
  }

  def windAvgF(windSpeed: List[Float], windDir: List[Float]): Float = {
    if (windSpeed.length != windDir.length)
      Logger.error(s"windSpeed #=${windSpeed.length} windDir #=${windDir.length}")

    val windRecord = windSpeed.zip(windDir)
    val wind_sin = windRecord.map(v => v._1 * Math.sin(Math.toRadians(v._2))).sum
    val wind_cos = windRecord.map(v => v._1 * Math.cos(Math.toRadians(v._2))).sum
    windAvg(wind_sin, wind_cos)
  }

  def getPeriodReport(monitor: Monitor.Value, start: DateTime, period: Period, includeTypes: List[MonitorType.Value] = MonitorType.monitorReportList,
                      monitorStatusFilter: MonitorStatusFilter.Value = MonitorStatusFilter.ValidData) = {
    DB localTx { implicit session =>

      val isHourRecord = (start + period >= start + 1.hour)
      val end = start + period
      val totalPeriod = new Period(start, end)

      val originalPeriodRecordList =
        if (isHourRecord)
          getHourRecords(monitor, start, start + period)
        else
          getMinRecords(monitor, start, start + period)

      val reportList =
        originalPeriodRecordList

      def statusFilter(data: (DateTime, (Option[Float], Option[String]))): Boolean = {
        if (data._2._1.isEmpty || data._2._2.isEmpty)
          return false

        val stat = data._2._2.get

        MonitorStatusFilter.isMatched(monitorStatusFilter, stat)
      }

      val usedMonitoredTypes = Monitor.map(monitor).monitorTypes.filter { includeTypes.contains(_) }

      val actualMonitoredTypes =
        if (usedMonitoredTypes.length == 0)
          includeTypes
        else
          usedMonitoredTypes

      val typeResultList =
        for {
          mt <- includeTypes
          t = monitorTypeProject2(mt)
          total = reportList.size
          projections = reportList.map(rs => (rs.date, t(rs)._1, t(rs)._2))
          validStat = { t: (Timestamp, Option[Float], Option[String]) =>
            statusFilter(t._1, (t._2, t._3))
          }

          validValues = projections.filter(validStat).map(t => t._2.getOrElse {
            Logger.error("#1 Unexpected Null value! " + t._1.toString())
            0f
          })
          count = validValues.length
        } yield {
          val avg = if (MonitorType.windDirList.contains(mt)) {
            val windDir = projections
            val wsT = monitorTypeProject2(MonitorType.C211)
            val windSpeed = reportList.map(rs => (rs.date, wsT(rs)._1, wsT(rs)._2))
            windAvg(windSpeed, windDir)
          } else {
            val sum = validValues.sum
            if (count != 0) sum / count else 0
          }

          val stat =
            if (count != 0) {
              val max = validValues.max
              val min = validValues.min
              Stat(Some(avg), Some(min), Some(max), count, total, 0)
            } else
              Stat(None, None, None, count, total, 0)

          MonitorTypeRecord(mt, projections, stat)
        }

      DailyReport(typeResultList.toArray)
    }
  }

  def getDailyReport(monitor: Monitor.Value, start: DateTime, includeTypes: List[MonitorType.Value] = MonitorType.monitorReportList,
                     monitorStatusFilter: MonitorStatusFilter.Value = MonitorStatusFilter.ValidData) = {

    DB readOnly { implicit session =>
      val originalList = getHourRecords(monitor, start, start + 1.day)
      val recordMap = Map(originalList.map { r => r.date -> r }: _*)

      val reportList =
        if (originalList.length == 24)
          originalList
        else {
          val endTime = start + 1.day
          import controllers.Report

          for (t <- Report.getPeriods(start, endTime, 1.hour))
            yield recordMap.getOrElse(t, emptyRecord(monitor.toString(), t))
        }

      def statusFilter(data: (DateTime, (Option[Float], Option[String]))): Boolean = {
        if (data._2._1.isEmpty || data._2._2.isEmpty)
          return false

        val stat = data._2._2.get

        MonitorStatusFilter.isMatched(monitorStatusFilter, stat)
      }

      val usedMonitoredTypes = Monitor.map(monitor).monitorTypes.filter { includeTypes.contains(_) }

      val actualMonitoredTypes =
        if (usedMonitoredTypes.length == 0)
          includeTypes
        else
          usedMonitoredTypes

      val typeResultList =
        for {
          mt <- includeTypes
          t = monitorTypeProject2(mt)
          total = recordMap.size
          projections = reportList.map(rs => (rs.date, t(rs)._1, t(rs)._2))
          validStat = { t: (Timestamp, Option[Float], Option[String]) =>
            statusFilter(t._1, (t._2, t._3))
          }

          validValues = projections.filter(validStat).map(t => t._2.getOrElse {
            Logger.error("#2 Unexpected Null value!")
            0f
          })
          count = validValues.length

        } yield {
          val stat =
            if (count >=16) {
              val avg = if (MonitorType.windDirList.contains(mt)) {
                val windDir = projections
                val windSpeedT = monitorTypeProject2(MonitorType.C211)
                val windSpeed = reportList.map(rs => (rs.date, windSpeedT(rs)._1, windSpeedT(rs)._2))
                windAvg(windSpeed, windDir)
              } else {
                val sum = validValues.sum
                if (count != 0) sum / count else 0
              }
              val max = validValues.max
              val min = validValues.min
              Stat(Some(avg), Some(min), Some(max), count, total, 0)
            } else
              Stat(None, None, None, count, total, 0)
              
          MonitorTypeRecord(mt, projections, stat)
        }

      DailyReport(typeResultList.toArray)
    }
  }

  case class MonitorEffectiveRate(monitor: Monitor.Value, rateMap: Map[MonitorType.Value, Float])
  def getMonitorEffectiveRate(monitor: Monitor.Value, start: DateTime): MonitorEffectiveRate = {
    val end = start + 1.month
    getMonitorEffectiveRate(monitor, start, end)
  }

  def getMonitorEffectiveRate(monitor: Monitor.Value, start: DateTime, end: DateTime) = {
    val records = Record.getHourRecords(monitor, start, end)
    val duration = new Interval(start, end).toDuration()
    val expected_count = duration.getStandardHours
    val ratePair =
      for {
        mt <- MonitorType.mtvList
        mtList = records.map(monitorTypeProject2(mt))
        count = mtList.count(r => (r._1.isDefined && r._2.isDefined && MonitorStatus.isNormalStat(r._2.get)))
      } yield {
        (mt -> count.toFloat / expected_count)
      }
    val rateMap = Map(ratePair: _*)
    MonitorEffectiveRate(monitor, rateMap)
  }

  case class MonitorTypeEffectiveRate(monitorType: MonitorType.Value, rateMap: Map[Monitor.Value, Float])
  def getMonitorTypeEffectiveRate(monitorType: MonitorType.Value, start: DateTime) = {
    val end = start + 1.month

    val duration = new Interval(start, end).toDuration()
    val expected_count = duration.getStandardHours
    val ratePair =
      for {
        m <- Monitor.mvList
        records = Record.getHourRecords(m, start, end)
        mtList = records.map(monitorTypeProject2(monitorType))
        count = mtList.count(r => (r._1.isDefined && r._2.isDefined && MonitorStatus.isNormalStat(r._2.get)))
      } yield {
        (m -> count.toFloat / expected_count)
      }
    val rateMap = Map(ratePair: _*)
    MonitorTypeEffectiveRate(monitorType, rateMap)
  }
  def getMonitorTypeYearlyEffectiveRate(monitorType: MonitorType.Value, start: DateTime) = {
    val end = start + 1.year
    var current = start
    import scala.collection.mutable.ListBuffer
    val result = ListBuffer[MonitorTypeEffectiveRate]()
    while (current < end) {
      result += getMonitorTypeEffectiveRate(monitorType, current)
      current += 1.month
    }
    result.toList
  }

  def getMonitorYearlyEffectiveRate(monitor: Monitor.Value, start: DateTime) = {
    val end = start + 1.year
    var current = start
    import scala.collection.mutable.ListBuffer
    val result = ListBuffer[MonitorEffectiveRate]()
    while (current < end) {
      result += getMonitorEffectiveRate(monitor, current)
      current += 1.month
    }
    result.toList
  }

  def getStatMonitorEffectiveRate(rateList: List[MonitorEffectiveRate]) = {
    val statList =
      for {
        mt <- MonitorType.mtvList
        mtRateList = rateList.map(r => r.rateMap(mt))
        count = mtRateList.count(r => r != 0)
        sum = mtRateList.sum
        avg = if (count != 0)
          sum / count else 0
      } yield if (count != 0)
        (mt -> Stat(Some(avg), Some(mtRateList.filter { _ != 0 }.min), Some(mtRateList.max), count, mtRateList.length, 0))
      else
        (mt -> Stat(None, None, None, count, mtRateList.length, 0))

    Map(statList: _*)
  }

  def getStatYearlyMonthlyEffectiveRate(rateList: List[MonitorTypeEffectiveRate]) = {
    val statList =
      for {
        m <- Monitor.mvList
        mRateList = rateList.map(r => r.rateMap(m))
        count = mRateList.count(r => r != 0)
        sum = mRateList.sum
        avg = if (count != 0)
          sum / count else 0
      } yield if (count != 0)
        (m -> Stat(Some(avg), Some(mRateList.filter { _ != 0 }.min), Some(mRateList.max), count, mRateList.length, 0))
      else
        (m -> Stat(None, None, None, count, mRateList.length, 0))

    Map(statList: _*)
  }

  def getWindRose(monitor: Monitor.Value, start: DateTime, end: DateTime, level: List[Float], nDiv: Int = 16) = {
    val records = getHourRecords(monitor, start, end)
    val windRecords = records.map { r => (r.wind_dir, r.wind_speed) }
    assert(windRecords.length != 0)

    val step = 360f / nDiv
    import scala.collection.mutable.ListBuffer
    val windDirPair =
      for (d <- 0 to nDiv - 1) yield {
        (d -> ListBuffer[Float]())
      }
    val windMap = Map(windDirPair: _*)

    var total = 0
    for (w <- windRecords) {
      if (w._1.isDefined && w._2.isDefined) {
        val dir = (Math.ceil((w._1.get - (step / 2)) / step).toInt) % nDiv
        windMap(dir) += w._2.get
        total += 1
      }
    }

    def winSpeedPercent(winSpeedList: ListBuffer[Float]) = {
      val count = new Array[Float](level.length + 1)
      def getIdx(v: Float): Int = {
        for (i <- 0 to level.length - 1) {
          if (v < level(i))
            return i
        }

        return level.length
      }

      for (w <- winSpeedList) {
        val i = getIdx(w)
        count(i) += 1
      }

      assert(total != 0)
      count.map(_ * 100 / total)
    }

    windMap.map(kv => (kv._1, winSpeedPercent(kv._2)))
  }

  def getComparedList(monitor: Monitor.Value, monitorType: MonitorType.Value, start: DateTime, end: DateTime, nYear: Int) = {
    val lastYearStart = start - 1.year
    val lastYearEnd = end - 1.year

    def f(r: HourRecord) = {
      (timeProjection(r), monitorTypeProject2(monitorType)(r))
    }

    val result =
      for {
        i <- 0 to nYear - 1
        records = getHourRecords(monitor, start - i.year, end - i.year)
      } yield records.map(f).filter(t => t._2._1.isDefined && t._2._2.isDefined && MonitorStatus.isNormal(t._2._2.get))

    result
  }

  def getRegressionData(monitor: Monitor.Value, monitorType: MonitorType.Value, start: DateTime, end: DateTime) = {

    val records = getHourRecords(monitor, start, end)

    val typeRecords = records.map {
      r => (timeProjection(r), monitorTypeProject2(monitorType)(r))
    }

    typeRecords
  }

  def getTabName(tab: TableType.Value, year: Int): SQLSyntax = {
    tab match {
      case TableType.EightHour=>
        Ozone8Hr.getTabName(year)
      case TableType.Hour =>
        SQLSyntax.createUnsafely(s"[P1234567_Hr_${year}]")
      case TableType.Min =>
        SQLSyntax.createUnsafely(s"[P1234567_M1_${year}]")
      case TableType.SixSec =>
        SQLSyntax.createUnsafely(s"[P1234567_S6_${year}]")
    }
  }

  case class EpaHourRecord(monitor: EpaMonitor.Value, time: DateTime, monitorType: MonitorType.Value, value: Float)
  def getEpaHourRecord(epaMonitor: EpaMonitor.Value, monitorType: MonitorType.Value, startTime: DateTime, endTime: DateTime)
                      (implicit session: DBSession = AutoSession): List[EpaHourRecord] = {
    val start: Timestamp = startTime
    val end: Timestamp = endTime
    val monitorId = EpaMonitor.map(epaMonitor).id
    val monitorTypeStrOpt = MonitorType.map(monitorType).epa_mapping
    if (monitorTypeStrOpt.isEmpty) {
      List.empty[EpaHourRecord]
    } else {
      val monitorTypeStr = monitorTypeStrOpt.get

      sql"""
        Select *
        From hour_data
        Where MStation=${monitorId} and MItem=${monitorTypeStr} and MDate >= ${start} and MDate < ${end}
        ORDER BY MDate ASC
      """.map {
        rs =>
          EpaHourRecord(EpaMonitor.idMap(rs.int(1)), rs.jodaDateTime(2), MonitorType.epaMap(rs.string(3)), rs.float(4))
      }.list().apply()
    }
  }

  def compareEpaReport(monitor: Monitor.Value, epaMonitor: EpaMonitor.Value, start: DateTime, end: DateTime) = {
    val hrRecord = getHourRecords(monitor, start, end)
    val pairs =
      for {
        mt <- MonitorType.epaReportList
        mtRecord = hrRecord.map { rs =>
          (timeProjection(rs).toDateTime, monitorTypeProject2(mt)(rs))
        }
        mtMap = Map(mtRecord: _*)
      } yield {
        val data = mtRecord.filter(
          r => r._2._1.isDefined && r._2._2.isDefined && MonitorStatus.isValid(r._2._2.get)).map(_._2._1.get)
        val count = data.length
        val stat =
          if (count != 0) {
            if (MonitorType.windDirList.contains(mt)) {
              val windDir = mtRecord.map(r => (r._1: java.sql.Timestamp, r._2._1, r._2._2))
              val wsT = monitorTypeProject2(MonitorType.C211)
              val windSpeed = hrRecord.map(rs => (rs.date: java.sql.Timestamp, wsT(rs)._1, wsT(rs)._2))
              val wind_avg = windAvg(windSpeed, windDir)
              Stat(Some(wind_avg), Some(data.min), Some(data.max), count, 24, 0)
            } else {
              Stat(Some(data.sum / count), Some(data.min), Some(data.max), count, 24, 0)
            }
          } else
            Stat(None, None, None, count, 24, 0)

        mt -> (mtMap, stat)
      }

    val localMap = Map(pairs: _*)

    val epaPairs =
      for {
        mt <- MonitorType.epaReportList
        epaRecord = getEpaHourRecord(epaMonitor, mt, start, end)
        epaPairs = epaRecord.map { r => r.time -> r }
        epaMap = Map(epaPairs: _*)
      } yield {
        val data = epaPairs.map(t => t._2.value)
        val count = data.length
        val stat =
          if (count != 0) {
            if (MonitorType.windDirList.contains(mt)) {
              val windDir = data
              val windSpeed = getEpaHourRecord(epaMonitor, MonitorType.C211, start, end).map { r => r.value }
              val wind_avg = windAvgF(windSpeed, windDir)
              Stat(Some(wind_avg), Some(data.min), Some(data.max), count, 24, 0)
            } else {
              Stat(Some(data.sum / count), Some(data.min), Some(data.max), count, 24, 0)
            }
          } else
            Stat(None, None, None, count, 24, 0)

        mt -> (epaMap, stat)
      }
    val epaMap = Map(epaPairs: _*)

    (localMap, epaMap)
  }
}