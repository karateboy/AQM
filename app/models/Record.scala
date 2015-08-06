package models
import java.sql.Date
import java.sql.Timestamp
import scalikejdbc._
import play.api._
import com.github.nscala_time.time.Imports._
import models.ModelHelper._
import models._

case class Stat(
    avg: Float,
    min: Float,
    max: Float,
    count: Int,
    total: Int,
    overCount: Int) {
  val effectPercent = count.toFloat / total
  val overPercent = overCount.toFloat / total
}

case class MonitorTypeRecord(monitorType: MonitorType.Value, dataList: List[(Timestamp, Option[Float], Option[String])], stat: Stat)
case class DailyReport(
  typeList: Array[MonitorTypeRecord])

object TableType extends Enumeration {
  val SixSec = Value("SixSec")
  val Min = Value("Min")
  val Hour = Value("Hour")
  val map = Map((SixSec -> "六秒資料"), (Min -> "分鐘資料"), (Hour -> "小時資料"))
}

object Record {
  case class HourRecord(
    name: String,
    date: Timestamp,
    chk: Option[String],
    tsp: Option[Float],
    tsp_stat: Option[String],
    pm10: Option[Float],
    pm10_stat: Option[String],
    pm25: Option[Float],
    pm25_stat: Option[String],
    s: Option[Float],
    s_stat: Option[String],
    so2: Option[Float],
    so2_stat: Option[String],
    nox: Option[Float],
    nox_stat: Option[String],
    co: Option[Float],
    co_stat: Option[String],
    o3: Option[Float],
    o3_stat: Option[String],
    thc: Option[Float],
    thc_stat: Option[String],
    ammonia: Option[Float],
    ammonia_stat: Option[String],
    noy: Option[Float],
    noy_stat: Option[String],
    noy_no: Option[Float],
    noy_no_stat: Option[String],
    nh3: Option[Float],
    nh3_stat: Option[String],
    no: Option[Float],
    no_stat: Option[String],
    ch4: Option[Float],
    ch4_stat: Option[String],
    monitor_humid: Option[Float],
    monitor_humid_stat: Option[String],
    monitor_temp: Option[Float],
    monitor_temp_stat: Option[String],
    no2: Option[Float],
    no2_stat: Option[String],
    nmhc: Option[Float],
    nmhc_stat: Option[String],
    wind_speed: Option[Float],
    wind_speed_stat: Option[String],
    wind_dir: Option[Float],
    wind_dir_stat: Option[String],
    rain: Option[Float],
    rain_stat: Option[String],
    temp: Option[Float],
    temp_stat: Option[String],
    humid: Option[Float],
    humid_stat: Option[String],
    air_pressure: Option[Float],
    air_pressure_stat: Option[String])

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
      rs.stringOpt(51), rs.floatOpt(52), rs.stringOpt(53))
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

  def getCount(start: Timestamp, end: Timestamp) = {
    DB readOnly { implicit session =>
      val query = sql"select count(*) from P1234567_Hr_2015 where M_DateTime >= ${start} and M_DateTime < ${end}"
      query.map { _.int(1) }.single().apply()
    }
  }

  def getHourRecords(monitor: Monitor.Value, startTime: DateTime, endTime: DateTime)(implicit session: DBSession = AutoSession) = {
    val start: Timestamp = startTime
    val end: Timestamp = endTime
    val monitorName = monitor.toString()
    val tab_name = getTabName(TableType.Hour, startTime.getYear)
    sql"""
        Select * 
        From ${tab_name}
        Where DP_NO=${monitorName} and M_DateTime >= ${start} and M_DateTime < ${end}
        ORDER BY M_DateTime ASC
      """.map { mapper }.list().apply()
  }

  def getMinRecords(monitor: Monitor.Value, startTime: DateTime, endTime: DateTime)(implicit session: DBSession = AutoSession) = {
    val start: Timestamp = startTime
    val end: Timestamp = endTime
    val monitorName = monitor.toString()
    val tab_name = getTabName(TableType.Min, startTime.getYear)
    sql"""
        Select * 
        From ${tab_name}
        Where DP_NO=${monitorName} and M_DateTime >= ${start} and M_DateTime < ${end}
        ORDER BY M_DateTime ASC
      """.map { mapper }.list().apply()
  }

  def getSecRecords(monitor: Monitor.Value, startTime: DateTime, endTime: DateTime)(implicit session: DBSession = AutoSession) = {
    val start: Timestamp = startTime
    val end: Timestamp = endTime
    val monitorName = monitor.toString()
    val tab_name = getTabName(TableType.SixSec, startTime.getYear)
    sql"""
        Select * 
        From ${tab_name}
        Where DP_NO=${monitorName} and M_DateTime >= ${start} and M_DateTime < ${end}
        ORDER BY M_DateTime ASC
      """.map { sixSecMapper }.list().apply()
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

  def getFieldName(mt: MonitorType.Value) = {
    val name = mt.toString
    val head = name.charAt(0)
    val tail = name.substring(2)

    SQLSyntax.createUnsafely(s"${head}5${tail}s")
  }

  def updateHourRecordStatus(monitor: Monitor.Value, monitorType: MonitorType.Value, mill: Long, status: String)(implicit session: DBSession = AutoSession) = {
    val recordTime = new Timestamp(mill)
    val monitorName = monitor.toString()
    val tab_name = getTabName(TableType.Hour, recordTime.toDateTime.getYear)
    val field_name = getFieldName(monitorType)
    sql""" 
        Update ${tab_name}
        Set ${field_name}=${status}
        Where DP_NO=${monitorName} and ${field_name} IS NOT NULL and M_DateTime = ${recordTime}        
      """.update.apply
  }

  val monitorTypeProjection: Map[MonitorType.Value, (HourRecord => Option[Float], HourRecord => Option[String])] = Map(
    MonitorType.withName("A213") -> (rs => { rs.tsp }, rs => { rs.tsp_stat }),
    MonitorType.withName("A214") -> (rs => { rs.pm10 }, rs => { rs.pm10_stat }),
    MonitorType.withName("A215") -> (rs => { rs.pm25 }, rs => { rs.pm25_stat }),
    MonitorType.withName("A221") -> (rs => { rs.s }, rs => { rs.s_stat }),
    MonitorType.withName("A222") -> (rs => { rs.so2 }, rs => { rs.so2_stat }),
    MonitorType.withName("A223") -> (rs => { rs.nox }, rs => { rs.nox_stat }),
    MonitorType.withName("A224") -> (rs => { rs.co }, rs => { rs.co_stat }),
    MonitorType.withName("A225") -> (rs => { rs.o3 }, rs => { rs.o3_stat }),
    MonitorType.withName("A226") -> (rs => { rs.thc }, rs => { rs.thc_stat }),
    MonitorType.withName("A229") -> (rs => { rs.ammonia }, rs => { rs.ammonia_stat }),
    MonitorType.withName("A232") -> (rs => { rs.noy }, rs => { rs.noy_stat }),
    MonitorType.withName("A233") -> (rs => { rs.noy_no }, rs => { rs.noy_no_stat }),
    MonitorType.withName("A235") -> (rs => { rs.nh3 }, rs => { rs.nh3_stat }),
    MonitorType.withName("A283") -> (rs => { rs.no }, rs => { rs.no_stat }),
    MonitorType.withName("A286") -> (rs => { rs.ch4 }, rs => { rs.ch4_stat }),
    MonitorType.withName("A288") -> (rs => { rs.monitor_humid }, rs => { rs.monitor_humid_stat }),
    MonitorType.withName("A289") -> (rs => { rs.monitor_temp }, rs => { rs.monitor_temp_stat }),
    MonitorType.withName("A293") -> (rs => { rs.no2 }, rs => { rs.no2_stat }),
    MonitorType.withName("A296") -> (rs => { rs.nmhc }, rs => { rs.nmhc_stat }),
    MonitorType.withName("C211") -> (rs => { rs.wind_speed }, rs => { rs.wind_speed_stat }),
    MonitorType.withName("C212") -> (rs => { rs.wind_dir }, rs => { rs.wind_dir_stat }),
    MonitorType.withName("C213") -> (rs => { rs.rain }, rs => { rs.rain_stat }),
    MonitorType.withName("C214") -> (rs => { rs.temp }, rs => { rs.temp_stat }),
    MonitorType.withName("C215") -> (rs => { rs.humid }, rs => { rs.humid_stat }),
    MonitorType.withName("C216") -> (rs => { rs.air_pressure }, rs => { rs.air_pressure_stat }))

  val monitorTypeProject2: Map[MonitorType.Value, HourRecord => (Option[Float], Option[String])] = Map(
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
    MonitorType.withName("C216") -> (rs => (rs.air_pressure, rs.air_pressure_stat)))

  def emptyRecord(monitor: String, start: DateTime) = {
    HourRecord(
      monitor,
      start,
      None, None, None, None, None, None, None, None, None, None,
      None, None, None, None, None, None, None, None, None, None,
      None, None, None, None, None, None, None, None, None, None,
      None, None, None, None, None, None, None, None, None, None,
      None, None, None, None, None, None, None, None, None, None,
      None)
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

      val minRecords =
        sql"""
        SELECT DP_NO, count(DP_NO)
        FROM [AQMSDB].[dbo].[P1234567_M1_2015]
        Where M_DateTime >= ${start} and M_DateTime < ${end}
        GROUP BY DP_NO
      """.map { rs => (Monitor.withName(rs.string(1)), rs.int(2)) }.list().apply()

      val minReport = Map(minRecords: _*)

      val sixSecRecords =
        sql"""
        SELECT DP_NO, count(DP_NO)
        FROM [AQMSDB].[dbo].[P1234567_S6_2015]
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

  def getDayReportMap(monitor: Monitor.Value, start: DateTime, end: DateTime, filter: MonitorStatusFilter.Value) = {
    val adjustStart = DateTime.parse(start.toString("YYYY-MM-dd"))
    val adjustEnd = DateTime.parse(end.toString("YYYY-MM-dd"))
    val days = getDays(adjustStart, adjustEnd)
    val nDay = days.length
    val dailyReports =
      for { day <- days } yield {
        (day -> Record.getDailyReport(monitor, day, MonitorType.mtvAllList, filter))
      }

    import scala.collection.mutable.Map
    val map = Map.empty[MonitorType.Value, Map[DateTime, (Option[Float], Option[String])]]

    for {
      (day, report) <- dailyReports
      t <- report.typeList
    } {
      val dateMap = map.getOrElse(t.monitorType, Map.empty[DateTime, (Option[Float], Option[String])])
      dateMap.put(day, (Some(t.stat.avg), Some(MonitorStatusFilter.statusMap(filter))))
      map.put(t.monitorType, dateMap)
    }
    map
  }

  def getDailyReport(monitor: Monitor.Value, start: DateTime, includeTypes: List[MonitorType.Value] = MonitorType.monitorReportList,
                     monitorStatusFilter: MonitorStatusFilter.Value = MonitorStatusFilter.Normal) = {
    DB localTx { implicit session =>
      val originalHourRecordList = getHourRecords(monitor, start, start + 1.day)
      val reportList =
        if (originalHourRecordList.length == 24)
          originalHourRecordList
        else {
          val endTime = start + 1.day
          def checkHourRecord(checkTime: DateTime, checkList: List[HourRecord]): List[HourRecord] = {
            if (checkTime >= endTime)
              Nil
            else {
              if (checkList.isEmpty || checkTime != checkList.head.date.toDateTime())
                emptyRecord(monitor.toString(), checkTime) :: checkHourRecord(checkTime + 1.hour, checkList)
              else
                checkList.head :: checkHourRecord(checkTime + 1.hour, checkList.tail)
            }
          }
          checkHourRecord(start, originalHourRecordList)
        }

      def statusFilter(data: (DateTime, (Option[Float], Option[String]))): Boolean = {
        if (data._2._2.isEmpty)
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
            Logger.error("Unexpected Null value!")
            0f
          })
          count = validValues.length

          max = if (count != 0) validValues.max else Float.MinValue
          min = if (count != 0) validValues.min else Float.MaxValue
        } yield {
          val avg = if (MonitorType.windDirList.contains(mt)) {
            val sum_sin = validValues.map(v => Math.sin(Math.toRadians(v))).sum
            val sum_cos = validValues.map(v => Math.cos(Math.toRadians(v))).sum
            val degree = Math.toDegrees(Math.atan(sum_sin / sum_cos)).toFloat
            if (degree > 0)
              degree
            else
              degree + 360
          } else {
            val sum = validValues.sum
            if (count != 0) sum / count else 0
          }

          val stat = Stat(avg, min, max, count, total, 0)
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
        (mt -> Stat(avg, mtRateList.filter { _ != 0 }.min, mtRateList.max, count, mtRateList.length, 0))
      else
        (mt -> Stat(avg, 0, 0, count, mtRateList.length, 0))

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
        (m -> Stat(avg, mRateList.filter { _ != 0 }.min, mRateList.max, count, mRateList.length, 0))
      else
        (m -> Stat(avg, 0, 0, count, mRateList.length, 0))

    Map(statList: _*)
  }

  def getWindRose(monitor: Monitor.Value, start: DateTime, end: DateTime, nDiv: Int = 16) = {
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
        val dir = (w._1.get / step).toInt
        windMap(dir) += w._2.get
        total += 1
      }
    }

    def winSpeedPercent(winSpeedList: ListBuffer[Float]) = {
      val count = new Array[Float](7)
      for (w <- winSpeedList) {
        if (w < 0.5)
          count(0) += 1
        else if (w < 2)
          count(1) += 1
        else if (w < 4)
          count(2) += 1
        else if (w < 6)
          count(3) += 1
        else if (w < 8)
          count(4) += 1
        else if (w < 10)
          count(5) += 1
        else
          count(6) += 1
      }

      assert(total != 0)
      count.map(_ * 100 / total)
    }

    windMap.map(kv => (kv._1, winSpeedPercent(kv._2)))
  }

  def getLastYearCompareList(monitor: Monitor.Value, monitorType: MonitorType.Value, start: DateTime, end: DateTime) = {
    val lastYearStart = start - 1.year
    val lastYearEnd = end - 1.year

    def checkHourRecord(checkTime: DateTime, endTime: DateTime, checkList: List[HourRecord]): List[HourRecord] = {
      if (checkTime >= endTime)
        Nil
      else {
        if (checkList.isEmpty || checkTime != checkList.head.date.toDateTime())
          emptyRecord(monitor.toString(), checkTime) :: checkHourRecord(checkTime + 1.hour, endTime, checkList)
        else
          checkList.head :: checkHourRecord(checkTime + 1.hour, endTime, checkList.tail)
      }
    }

    val records = checkHourRecord(start, end, getHourRecords(monitor, start, end))

    val typeRecords = records.map {
      r => (timeProjection(r), monitorTypeProject2(monitorType)(r)._1.getOrElse(0f))
    }

    val lastYearRecords = checkHourRecord(start, end, getHourRecords(monitor, lastYearStart, lastYearEnd))
    val typeLastYearRecords = lastYearRecords.map {
      r => (timeProjection(r), monitorTypeProject2(monitorType)(r)._1.getOrElse(0f))
    }

    (typeRecords, typeLastYearRecords)
  }

  def getRegressionData(monitor: Monitor.Value, monitorType: MonitorType.Value, start: DateTime, end: DateTime) = {

    def checkHourRecord(checkTime: DateTime, endTime: DateTime, checkList: List[HourRecord]): List[HourRecord] = {
      if (checkTime >= endTime)
        Nil
      else {
        if (checkList.isEmpty || checkTime != checkList.head.date.toDateTime())
          emptyRecord(monitor.toString(), checkTime) :: checkHourRecord(checkTime + 1.hour, endTime, checkList)
        else
          checkList.head :: checkHourRecord(checkTime + 1.hour, endTime, checkList.tail)
      }
    }

    val records = checkHourRecord(start, end, getHourRecords(monitor, start, end))

    val typeRecords = records.map {
      r => (timeProjection(r), monitorTypeProject2(monitorType)(r)._1.getOrElse(0f))
    }

    typeRecords
  }

  def getTabName(tab: TableType.Value, year: Int) = {
    tab match {
      case TableType.Hour =>
        SQLSyntax.createUnsafely(s"[AQMSDB].[dbo].[P1234567_Hr_${year}]")
      case TableType.Min =>
        SQLSyntax.createUnsafely(s"[AQMSDB].[dbo].[P1234567_M1_${year}]")
      case TableType.SixSec =>
        SQLSyntax.createUnsafely(s"[AQMSDB].[dbo].[P1234567_S6_${year}]")
    }
  }

  case class EpaHourRecord(monitor: EpaMonitor.Value, time: DateTime, monitorType: MonitorType.Value, value: Float)
  def getEpaHourRecord(epaMonitor: EpaMonitor.Value, monitorType: MonitorType.Value, startTime: DateTime, endTime: DateTime)(implicit session: DBSession = AutoSession) = {
    val start: Timestamp = startTime
    val end: Timestamp = endTime
    val monitorId = EpaMonitor.map(epaMonitor).id
    val monitorTypeStrOpt = MonitorType.map(monitorType).epa_mapping
    assert(monitorTypeStrOpt.isDefined)
    val monitorTypeStr = monitorTypeStrOpt.get
    sql"""
        Select * 
        From hour_data
        Where MStation=${monitorId} and MItem=${monitorTypeStr} and MDate >= ${start} and MDate < ${end}
        ORDER BY MDate ASC
      """.map {
      rs => EpaHourRecord(EpaMonitor.idMap(rs.int(2)), rs.timestamp(3), MonitorType.epaMap(rs.string(4)), rs.float(5))
    }.list().apply()
  }

  def compareEpaReport(monitor: Monitor.Value, epaMonitor: EpaMonitor.Value, start: DateTime, end: DateTime) = {
    val hrRecord = getHourRecords(monitor, start, end)
    val pairs =
      for {
        mt <- MonitorType.epaReportList
        mtRecord = hrRecord.map { rs => (timeProjection(rs).toDateTime, monitorTypeProject2(mt)(rs)) }
        mtMap = Map(mtRecord: _*)
      } yield {
        val data = mtRecord.filter(
          r => r._2._1.isDefined && r._2._2.isDefined && MonitorStatus.isNormal(r._2._2.get)).map(_._2._1.get)
        val count = data.length
        val stat =
          if (count != 0)
            Stat(data.sum / count, data.min, data.max, count, 24, 0)
          else
            Stat(0, 0, 0, count, 24, 0)

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
          if (count != 0)
            Stat(data.sum / count, data.min, data.max, count, 24, 0)
          else
            Stat(0, 0, 0, count, 24, 0)

        mt -> (epaMap, stat)
      }
    val epaMap = Map(epaPairs: _*)

    (localMap, epaMap)
  }
}