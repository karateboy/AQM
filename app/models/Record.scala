package models
import java.sql.Date
import java.sql.Timestamp
import scalikejdbc._
import play.api._
import com.github.nscala_time.time.Imports._
import models.ModelHelper._
import scala.collection.Map

case class HourRecord(
    monitor:String,
    startTime:Timestamp,
    monitorType:Int,
    avgValue:Float,
    minValue:Float,
    maxValue:Float,
    effectPercent:Float
    )
    
case class MinRecord(
    name:String,
    date:Timestamp,
    chk:Option[String],
    tsp:Option[Float],
    tsp_stat:Option[String],
    pm10:Option[Float],
    pm10_stat:Option[String],
    pm25:Option[Float],
    pm25_stat:Option[String],
    s:Option[Float],
    s_stat:Option[String],
    so2:Option[Float],
    so2_stat:Option[String],
    nox:Option[Float],
    nox_stat:Option[String],
    co:Option[Float],
    co_stat:Option[String],
    o3:Option[Float],
    o3_stat:Option[String],
    thc:Option[Float],
    thc_stat:Option[String],
    ammonia:Option[Float],
    ammonia_stat:Option[String],
    noy:Option[Float],
    noy_stat:Option[String],
    noy_no:Option[Float],
    noy_no_stat:Option[String],
    nh3:Option[Float],
    nh3_stat:Option[String],
    no:Option[Float],
    no_stat:Option[String],
    ch4:Option[Float],
    ch4_stat:Option[String],
    monitor_humid:Option[Float],
    monitor_humid_stat:Option[String],
    monitor_temp:Option[Float],
    monitor_temp_stat:Option[String],
    no2:Option[Float],
    no2_stat:Option[String],
    nmhc:Option[Float],
    nmhc_stat:Option[String],
    wind_speed:Option[Float],
    wind_speed_stat:Option[String],
    wind_dir:Option[Float],
    wind_dir_stat:Option[String],
    rain:Option[Float],
    rain_stat:Option[String],
    temp:Option[Float],
    temp_stat:Option[String],
    humid:Option[Float],
    humid_stat:Option[String],
    air_pressure:Option[Float],
    air_pressure_stat:Option[String]
    )
    
case class Stat(
    avgValue:Float,
    minValue:Float,
    maxValue:Float,
    effectPercent:Float
    )
    
case class HourlyReport(
    time:DateTime,
    record:Map[Int,Stat]
    )
 
case class DailyReport(
   data:List[HourlyReport],
   statMap:Map[Int, Stat]
  )
    
object MinRecord {
  val table = "P12345678_M1_2014"
  def mapper(rs: WrappedResultSet)={
    MinRecord(rs.string(1), rs.timestamp(2), rs.stringOpt(3), rs.floatOpt(4), rs.stringOpt(5), 
        rs.floatOpt(6), rs.stringOpt(7), rs.floatOpt(8), rs.stringOpt(9), rs.floatOpt(10), 
        rs.stringOpt(11), rs.floatOpt(12), rs.stringOpt(13), rs.floatOpt(14), rs.stringOpt(15), 
        rs.floatOpt(16), rs.stringOpt(17), rs.floatOpt(18), rs.stringOpt(19), rs.floatOpt(20), 
        rs.stringOpt(21), rs.floatOpt(22), rs.stringOpt(23), rs.floatOpt(24), rs.stringOpt(25), 
        rs.floatOpt(26), rs.stringOpt(27), rs.floatOpt(28), rs.stringOpt(29),  rs.floatOpt(30), 
        rs.stringOpt(31), rs.floatOpt(32), rs.stringOpt(33), rs.floatOpt(34), rs.stringOpt(35), 
        rs.floatOpt(36), rs.stringOpt(37), rs.floatOpt(38), rs.stringOpt(39),  rs.floatOpt(40), 
        rs.stringOpt(41), rs.floatOpt(42), rs.stringOpt(43), rs.floatOpt(44), rs.stringOpt(45), 
        rs.floatOpt(46), rs.stringOpt(47), rs.floatOpt(48), rs.stringOpt(49),  rs.floatOpt(50), 
        rs.stringOpt(51), rs.floatOpt(52), rs.stringOpt(53)
    )
  }
  
  def getOneHourMinRecords(monitor:Monitor.Value, startTime:DateTime)(implicit session: DBSession = AutoSession)={
    val start: Timestamp = startTime
    val end: Timestamp = startTime + 1.hour
    val monitorName = monitor.toString()
    
    DB readOnly { implicit session =>
      sql"select * from P1234567_M1_2014 where DP_NO=${monitorName} and M_DateTime >= ${start} and M_DateTime < ${end}".map { mapper }.list().apply()  
    }
  }
  
  def getCount(start:Timestamp, end:Timestamp)={
    DB readOnly { implicit session =>
      val query = sql"select count(*) from P1234567_M1_2014 where M_DateTime >= ${start} and M_DateTime < ${end}"
      Logger.info(query.toString)
      query.map { _.int(1) }.single().apply()  
    }
  }
   
  def saveHourRecord(hr:List[HourRecord])(implicit session: DBSession = AutoSession)={
    hr.foreach { h => 
      sql"""insert into HourlyReport(monitor, startTime, monitorType, avgValue, 
        minValue, maxValue, effectPercent) values 
        (${h.monitor}, ${h.startTime}, ${h.monitorType}, ${h.avgValue}, 
          ${h.minValue}, ${h.maxValue}, ${h.effectPercent})"""
        .update.apply()
    }
  }
  
  def putHourlyReport(monitor:Monitor.Value, start:DateTime)(implicit session: DBSession = AutoSession)={
      val minRecords = getOneHourMinRecords(monitor, start)
      Logger.debug("# min Record = " + minRecords.length)
      
      def genHourRecord(monitorType:MonitorType.Value,f:MinRecord=>Option[Float])={
        val rs : List[Option[Float]] = minRecords.map(f)
        var sum = 0.0f
        var count = 0
        var min = 1000.0f
        var max = -1.0f
        rs.foreach {_ match {
            case Some(v)=>
              sum += v
              count+=1
              if(min>v)
                min = v
              if(max<v)
                max = v
            case None=>
          } 
        }
        
        if(count >= 1)
          HourRecord(monitor.toString(), start, monitorType.id, sum/count, min, max, count.toFloat/60)
        else
          HourRecord(monitor.toString(), start, monitorType.id, 0, 0, 0, 0)
      }
      
      val monitorTypeHandler:List[(MonitorType.Value, MinRecord=>Option[Float])] = List(
          (MonitorType.A213, rs=>{rs.tsp}),
          (MonitorType.A214, rs=>{rs.pm10}),
          (MonitorType.A215, rs=>{rs.pm25}),
          (MonitorType.A221, rs=>{rs.s}),
          (MonitorType.A222, rs=>{rs.so2}),
          (MonitorType.A223, rs=>{rs.nox}),
          (MonitorType.A224, rs=>{rs.co}),
          (MonitorType.A225, rs=>{rs.o3}),
          (MonitorType.A226, rs=>{rs.thc}),
          (MonitorType.A229, rs=>{rs.ammonia}),
          (MonitorType.A232, rs=>{rs.noy}),
          (MonitorType.A233, rs=>{rs.noy_no}),
          (MonitorType.A235, rs=>{rs.nh3}),
          (MonitorType.A283, rs=>{rs.no}),
          (MonitorType.A286, rs=>{rs.ch4}),
          (MonitorType.A288, rs=>{rs.monitor_humid}),
          (MonitorType.A289, rs=>{rs.monitor_temp}),
          (MonitorType.A293, rs=>{rs.no2}),
          (MonitorType.A296, rs=>{rs.nmhc}),
          (MonitorType.C211, rs=>{rs.wind_speed}),
          (MonitorType.C212, rs=>{rs.wind_dir}),
          (MonitorType.C213, rs=>{rs.rain}),
          (MonitorType.C214, rs=>{rs.temp}),
          (MonitorType.C215, rs=>{rs.humid}),
          (MonitorType.C216, rs=>{rs.air_pressure})
      )
    
      val hourRecord = monitorTypeHandler.map(t=>genHourRecord(t._1, t._2))
      saveHourRecord(hourRecord)
      hourRecord
  }
  
  def getHourlyReport(monitor:Monitor.Value, start:DateTime)(implicit session: DBSession = AutoSession)={
      DB localTx { implicit session =>
        def getHourRecordFromDB()={
          val startTime:Timestamp = start
          val endTime:Timestamp = start + 1.hour
          sql"""
            select * from HourlyReport where monitor=${monitor.toString()} and startTime>=${startTime} and startTime<${endTime}
            """.map{
              rs=>HourRecord(rs.string(1), rs.timestamp(2), rs.int(3), rs.float(4), rs.float(5), rs.float(6), rs.float(7))}
            .list.apply()
        }      
        
        val hr = getHourRecordFromDB()
        if(hr.length == 0)
          putHourlyReport(monitor, start) 
        else 
          hr
      }
  } 
  
  def convertHourRecordToHourlyReport(hrs:List[HourRecord])={
    import scala.collection.mutable.HashMap
    val map = new HashMap[Int, Stat]
    for(hr<-hrs){
      map.put(hr.monitorType, Stat(hr.avgValue, hr.minValue, hr.maxValue, hr.effectPercent))
    }
    
    HourlyReport(new DateTime(hrs.head.startTime.getTime), map)
  }
  
  def getDailyReport(monitor:Monitor.Value, start:DateTime)={
    DB localTx { implicit session =>
      var t = start
      import scala.collection.mutable.HashMap
      var reportList = List[HourlyReport]()
      do{
        Logger.info("getHourlyRecord: " + t.toString())
        val hrs = getHourlyReport(monitor, t)
        reportList = reportList :+ convertHourRecordToHourlyReport(hrs) 
        Logger.info("reportList len="+reportList.length)
        t += 1.hour
      }while(t < start.plusDays(1));
      
      import scala.collection.mutable.HashMap
      val map = new HashMap[Int, Stat]()
      DailyReport(reportList, map)
    }
  }
}

object Record{
  def main(args: Array[String]) {
    val startTime = DateTime.parse("2015-04-01")
    MinRecord.getHourlyReport(Monitor.A001, startTime)
  }
}