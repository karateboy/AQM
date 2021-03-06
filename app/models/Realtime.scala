package models
import scalikejdbc._
import java.sql.Date
import java.sql.Timestamp
import scalikejdbc._
import play.api._
import com.github.nscala_time.time.Imports._
import com.github.nscala_time.time._
import models.ModelHelper._
import models.Record._

case class RealtimeStatus(data: Map[Monitor.Value, Map[MonitorType.Value, (Option[Float], Option[String])]])
case class SixSecRecord(c911: Array[(Option[Float], Option[String])], c912: Array[(Option[Float], Option[String])])
object Realtime {
  def SixSecRecordMapper(rs: WrappedResultSet) = {
    val c911 =
      for { i <- 0 to 9 } yield {
        (rs.floatOpt(4 + 2 * i), rs.stringOpt(5 + 2 * i))
      }
    val c912 =
      for { i <- 0 to 9 } yield {
        (rs.floatOpt(24 + 2 * i), rs.stringOpt(25 + 2 * i))
      }
    SixSecRecord(c911.toArray, c912.toArray)
  }

  def getRealtimeMinStatus(current:DateTime, privilege: Privilege) = {

    DB readOnly { implicit session =>
      val tab_name = Record.getTabName(TableType.Min, current.getYear)
      val hrs =
            sql"""
              SELECT *
              FROM ${tab_name}
              WHERE M_DateTime = ${current}
             """.map { Record.mapper }.list.apply

      val rt_result =
        for { m <- privilege.allowedMonitors } yield {
          import scala.collection.mutable.Map
          val hrMap: Map[Monitor.Value, HourRecord] = Map()

          for (hr <- hrs) {
            val m = Monitor.withName(hr.name)
            if (!hrMap.contains(m))
              hrMap += (m -> hr)
          }

          val hr = hrMap.getOrElse(m, emptyRecord(Monitor.map(m).id, current))
          val type_record = monitorTypeProject2
          .map(
            t => (t._1 -> t._2(hr)))
          (m -> type_record)
        }
      Map(rt_result: _*)
    }
  }

  def statusFilter(msf: MonitorStatusFilter.Value)(data: (Option[Float], Option[String])): Boolean = {
    if (data._2.isEmpty)
      return false

    val stat = data._2.get
    MonitorStatusFilter.isMatched(msf, stat)
  }

  def getMonitorTypeAvg(monitor: Monitor.Value, monitorType: MonitorType.Value, start: DateTime, end: DateTime)(implicit session: DBSession = AutoSession) = {

    val records = getHourRecords(monitor, start, end)
    val typeValues = records.map { hr => monitorTypeProject2(monitorType)(hr) }
    val duration = new Duration(start, end)
    val nHour = duration.getStandardHours
    val validValues = typeValues.filter(statusFilter(MonitorStatusFilter.ValidData)).map(_._1.get)
    val total = validValues.length
    if (total == 0 || total != nHour)
      None
    else {
      val sum = validValues.sum
      Some(sum / total)
    }
  }


  def getMonitorTypeAvg(records: List[HourRecord], monitorType: MonitorType.Value) = {
    val typeValues = records.map { hr => monitorTypeProject2(monitorType)(hr) }
    val validValues = typeValues.filter(statusFilter(MonitorStatusFilter.ValidData)).map(_._1.get)
    val total = validValues.length
    if (total == 0)
      None
    else {
      Some(validValues.sum / total)
    }
  }

  def getMonitorTypeMax(records: List[HourRecord], monitorType: MonitorType.Value) = {
    val typeValues = records.map { hr => monitorTypeProject2(monitorType)(hr) }
    val validValues = typeValues.filter(statusFilter(MonitorStatusFilter.ValidData)).map(_._1.get)
    val total = validValues.length
    if (total == 0)
      None
    else {
      Some(validValues.max)
    }
  }

  def getMonitorType8HourAvgMax(monitor: Monitor.Value, monitorType: MonitorType.Value, start: DateTime, end: DateTime)(implicit session: DBSession = AutoSession) = {
    def EightHourAvg(start: DateTime): List[Option[Float]] = {
      if (start + 8.hour>= end)
        Nil
      else
        getMonitorTypeAvg(monitor, monitorType, start, end) :: EightHourAvg(start + 1.hours)
    }

    val avgs = EightHourAvg(start)
    
    avgs.max
  }
  

  def pm10PSI(ov: Option[Float]) = {
    if (ov.isEmpty)
      None
    else
      Some {
        val v = ov.get
        if (v >= 0 && v <= 50) {
          v
        } else if (v <= 100) {
          50 + (v - 50) * 50 / 100
        } else if (v <= 350) {
          100 + (v - 150) * 100 / (350 - 150)
        } else if (v <= 420) {
          200 + (v - 350) * 100 / (420 - 350)
        } else if (v <= 500) {
          300 + (v - 420) * 100 / (500 - 420)
        } else {
          400 + (v - 500) * 100 / 100
        }
      }
  }

  def so2PSI(ov: Option[Float]) = {
    if (ov.isEmpty)
      None
    else
      Some {
        val v = ov.get

        if (v <= 30) {
          v / 30 * 50
        } else if (v <= 140) {
          50 + (v - 30) * 50 / (140 - 30)
        } else if (v <= 300) {
          100 + (v - 140) * 100 / (300 - 140)
        } else if (v <= 600) {
          200 + (v - 300) * 100 / (600 - 300)
        } else if (v <= 800) {
          300 + (v - 600) * 100 / (800 - 600)
        } else {
          400 + (v - 800) * 100 / (1000 - 800)
        }
      }
  }

  def coPSI(ov: Option[Float]) = {
    if (ov.isEmpty)
      None
    else
      Some {
        val v = ov.get

        if (v <= 4.5) {
          v / 4.5f * 50f
        } else if (v <= 9) {
          (50 + (v - 4.5) * 50 / (9 - 4.5)).toFloat
        } else if (v <= 15) {
          100 + (v - 9) * 100 / (15 - 9)
        } else if (v <= 30) {
          200 + (v - 15) * 100 / (30 - 9)
        } else if (v <= 40) {
          300 + (v - 30) * 100 / (40 - 30)
        } else {
          400 + (v - 40) * 100 / (50 - 40)
        }
      }
  }

  def o3PSI(ov: Option[Float]) = {
    if (ov.isEmpty)
      None
    else
      Some {
        val v = ov.get

        if (v <= 60) {
          v / 60 * 50
        } else if (v <= 120) {
          50 + (v - 60) * 50 / (120 - 60)
        } else if (v <= 200) {
          100 + (v - 120) * 100 / (200 - 120)
        } else if (v <= 400) {
          200 + (v - 200) * 100 / (400 - 200)
        } else if (v <= 500) {
          300 + (v - 400) * 100 / (500 - 400)
        } else {
          400 + (v - 500) * 100 / (600 - 500)
        }
      }
  }

  def no2PSI(ov: Option[Float]) = {
    if (ov.isEmpty)
      None
    else
      Some {
        val v = ov.get

        if (v < 600) {
          0
        } else if (v <= 1200) {
          200 + (v - 600) * 100 / (1200 - 600)
        } else if (v <= 1600) {
          300 + (v - 1200) * 100 / (1600 - 1200)
        } else {
          400 + (v - 1600) * 100 / (2000 - 1600)
        }
      }
  }
  
  def getMonitorRealtimePSI(monitor: Monitor.Value, current:DateTime)(implicit session: DBSession = AutoSession) = {
    val pm10_12 = getMonitorTypeAvg(monitor, MonitorType.withName("A214"), current - 11.hour, current + 1.hour)
    val pm10_4 = getMonitorTypeAvg(monitor, MonitorType.withName("A214"), current - 3.hour, current + 1.hour)
    val pm10 = if(pm10_12.isDefined && pm10_4.isDefined)
          Some((pm10_12.get + pm10_4.get) / 2)
        else
          None
          
    val so2_24 = getMonitorTypeAvg(monitor, MonitorType.withName("A222"), current - 23.hour, current + 1.hour)
    val co_8 = getMonitorTypeAvg(monitor, MonitorType.withName("A224"), current - 7.hour, current + 1.hour)
    val o3 = getMonitorTypeAvg(monitor, MonitorType.withName("A225"), current, current + 1.hour)
    val no2 = getMonitorTypeAvg(monitor, MonitorType.withName("A293"), current, current + 1.hour)
    val result = Map[MonitorType.Value, (Option[Float], Option[Float])](
      MonitorType.withName("A214") -> (pm10, pm10PSI(pm10)),
      MonitorType.withName("A222") -> (so2_24, so2PSI(so2_24)),
      MonitorType.withName("A224") -> (co_8, coPSI(co_8)),
      MonitorType.withName("A225") -> (o3, o3PSI(o3)),
      MonitorType.withName("A293") -> (no2, no2PSI(no2)))
    val sub_psi = result.values.map(_._2)
    val psi = sub_psi.toList.max
    
    (psi, result)
  }
  
  case class PsiReport(psi:Option[Float], sub_map:Map[MonitorType.Value, (Option[Float], Option[Float])])
  
  def getMonitorMonthlyPSI(monitor: Monitor.Value, start:DateTime)={
    val end = start + 1.month
    
    def helper(day:DateTime):List[PsiReport]={
      if(day >= end)
        Nil
      else
        getMonitorDailyPSI(monitor, day)::helper(day + 1.day)
    }
    
    helper(start)
  }
  
  def getMonitorDailyPSI(monitor: Monitor.Value, current:DateTime)(implicit session: DBSession = AutoSession) = {
    val day_hr_records = getHourRecords(monitor, current, current+1.day)
    val pm10_24 = getMonitorTypeAvg(day_hr_records, MonitorType.withName("A214"))          
    val so2_24 = getMonitorTypeAvg(day_hr_records, MonitorType.withName("A222"))
    val o3 = getMonitorTypeMax(day_hr_records, MonitorType.withName("A225"))
    val no2 = getMonitorTypeMax(day_hr_records, MonitorType.withName("A293"))
    
    val co_8 = getMonitorType8HourAvgMax(monitor, MonitorType.withName("A224"), current, current+1.day)
    val result = Map[MonitorType.Value, (Option[Float], Option[Float])](
      MonitorType.withName("A214") -> (pm10_24, pm10PSI(pm10_24)),
      MonitorType.withName("A222") -> (so2_24, so2PSI(so2_24)),
      MonitorType.withName("A224") -> (co_8, coPSI(co_8)),
      MonitorType.withName("A225") -> (o3, o3PSI(o3)),
      MonitorType.withName("A293") -> (no2, no2PSI(no2)))
    val sub_psi = result.values.map(_._2)
    val psi = sub_psi.toList.max
    
    PsiReport(psi, result)
  }
  
  def getPsiLevel(v:Float)={
    if(v<=50)
      "PSI1"
    else if(v<=100)
      "PSI2"
    else if(v<=199)
      "PSI3"
    else if(v<=299)
      "PSI4"
    else
      "PSI5"      
  }
  
  def getRealtimePSI(current:DateTime, monitorList:List[Monitor.Value] = Monitor.mvList)(implicit session: DBSession = AutoSession) = {
    val result =
      for {
        m <- monitorList
      } yield {        
        m -> getMonitorRealtimePSI(m, current)
      }
    Map(result: _*)
  }

  def getDailyPsiReport(m: Monitor.Value, start: DateTime)(implicit session: DBSession = AutoSession) = {
    val end = start + 1.day
    def hourRange(start: DateTime): List[DateTime] = {
      if (start >= end)
        Nil
      else
        start :: hourRange(start + 1.hours)
    }

    for {
      hr <- hourRange(start)
    } yield {
      getMonitorRealtimePSI(m, hr)
    }
  }
  
  def getLatestRecordTime(tabType:TableType.Value)(implicit session: DBSession = AutoSession) = {
    val tab_name = Record.getTabName(tabType, DateTime.now.getYear)
    sql"""
      SELECT TOP 1 M_DateTime
      FROM ${tab_name}
      ORDER BY M_DateTime  DESC
      """.map { r=>r.timestamp(1) }.single.apply  
  }
  
  def getLatestMonitorRecordTime(tabType:TableType.Value, m:Monitor.Value)(implicit session: DBSession = AutoSession) = {
    val tab_name = Record.getTabName(tabType, DateTime.now.getYear)
    sql"""
      SELECT TOP 1 M_DateTime
      FROM ${tab_name}
      WHERE DP_NO = ${m.toString}
      ORDER BY M_DateTime  DESC
      """.map { r=>r.timestamp(1) }.single.apply  
  }
  
  def getRealtimeMonitorValueMap(mt:MonitorType.Value, current:Timestamp)(implicit session: DBSession = AutoSession) = {
    val datetime = current.toDateTime 
    val tab = Record.getTabName(TableType.Min, datetime.getYear)
    val records = sql"""
      SELECT *
      FROM ${tab}
      WHERE M_DateTime = ${current}
      """.map { Record.mapper }.list.apply

    val kvs =
      for { r <- records } yield {
        val t = Record.monitorTypeProject2(mt)(r)
        Monitor.withName(r.name) -> t
      }
    
    Map( kvs :_*)
  }
  
  def getRealtimeMonitorStatusMap(current:Timestamp)(implicit session: DBSession = AutoSession) = {
    val datetime = current.toDateTime 
    val tab = Record.getTabName(TableType.Min, datetime.getYear)
    val records = sql"""
      SELECT *
      FROM ${tab}
      WHERE M_DateTime = ${current}
      """.map { Record.mapper }.list.apply

    
    val kvs =
      for { r <- records } yield {
        val monitor = Monitor.withName(r.name)
        val statusPairs = 
          for(mt <- Monitor.map(monitor).monitorTypes)
            yield{
              mt->Record.monitorTypeProject2(mt)(r)._2
          }
        
        val statusMap = Map(statusPairs: _*) 
        monitor -> statusMap
      }
    
    Map( kvs :_*)
  }
    
  def getRealtimeWeatherMap(current:Timestamp)(implicit session: DBSession = AutoSession) = {
    val datetime = current.toDateTime 
    val tab = Record.getTabName(TableType.SixSec, datetime.getYear)
    val records = sql"""
      SELECT *
      FROM ${tab}
      WHERE M_DateTime = ${current}
      """.map { Record.sixSecMapper }.list.apply

    val kvs =
      for { r <- records } yield {
        r.monitor -> r
      }
    
    Map( kvs :_*)
  }
  
}