package models
import java.sql.Date
import java.sql.Timestamp
import scalikejdbc._
import play.api._
import com.github.nscala_time.time.Imports._
import models.ModelHelper._
    
case class HourRecord(
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
    avg:Float,
    min:Float,
    max:Float,
    count:Int,
    total:Int,
    overCount:Int
    ){
  val effectPercent = count.toFloat/total
  val overPercent = overCount.toFloat/total
}
 
case class MonitorTypeRecord(monitorType:MonitorType.Value , dataList:List[(Timestamp, Option[Float], Option[String])], stat:Stat)
case class DailyReport(
   typeList:Array[MonitorTypeRecord]
  )

object HourRecord {
  val NORMAL_STAT = "010"
  val OVER_STAT = "011"
  val BELOW_STAT = "012"
  val VALID_STATS = List(NORMAL_STAT, OVER_STAT, BELOW_STAT)
  def isValidStat(s:String)={
    VALID_STATS.contains(s)
  }
  
  def mapper(rs: WrappedResultSet) = {
    HourRecord(rs.string(1), rs.timestamp(2), rs.stringOpt(3), rs.floatOpt(4), rs.stringOpt(5), 
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
  
  def getCount(start:Timestamp, end:Timestamp)={
    DB readOnly { implicit session =>
      val query = sql"select count(*) from P1234567_M1_2014 where M_DateTime >= ${start} and M_DateTime < ${end}"
      Logger.info(query.toString)
      query.map { _.int(1) }.single().apply()  
    }
  }
  
  def getHourRecords(monitor:Monitor.Value, startTime:DateTime, endTime:DateTime)(implicit session: DBSession = AutoSession)={
    val start: Timestamp = startTime
    val end: Timestamp = endTime
    val monitorName = monitor.toString()
    
    //DB readOnly { implicit session =>
      sql"""
        Select * From P1234567_M1_2014 
        Where DP_NO=${monitorName} and M_DateTime >= ${start} and M_DateTime < ${end}
        ORDER BY M_DateTime ASC
      """.map { mapper }.list().apply()  
    //}
  }
  val timeProjection:(HourRecord=>Timestamp) ={
    rs=>rs.date
  }
  
  val monitorTypeProjection:Map[MonitorType.Value, (HourRecord=>Option[Float], HourRecord=>Option[String])] = Map(
          MonitorType.withName("A213")->(rs=>{rs.tsp}, rs=>{rs.tsp_stat}),
          MonitorType.withName("A214")->(rs=>{rs.pm10}, rs=>{rs.pm10_stat}),
          MonitorType.withName("A215")->(rs=>{rs.pm25}, rs=>{rs.pm25_stat}),
          MonitorType.withName("A221")->(rs=>{rs.s}, rs=>{rs.s_stat}),
          MonitorType.withName("A222")->(rs=>{rs.so2}, rs=>{rs.so2_stat}),
          MonitorType.withName("A223")->(rs=>{rs.nox}, rs=>{rs.nox_stat}),
          MonitorType.withName("A224")->(rs=>{rs.co}, rs=>{rs.co_stat}),
          MonitorType.withName("A225")->(rs=>{rs.o3}, rs=>{rs.o3_stat}),
          MonitorType.withName("A226")->(rs=>{rs.thc}, rs=>{rs.thc_stat}),
          MonitorType.withName("A229")->(rs=>{rs.ammonia}, rs=>{rs.ammonia_stat}),
          MonitorType.withName("A232")->(rs=>{rs.noy}, rs=>{rs.noy_stat}),
          MonitorType.withName("A233")->(rs=>{rs.noy_no}, rs=>{rs.noy_no_stat}),
          MonitorType.withName("A235")->(rs=>{rs.nh3}, rs=>{rs.nh3_stat}),
          MonitorType.withName("A283")->(rs=>{rs.no}, rs=>{rs.no_stat}),
          MonitorType.withName("A286")->(rs=>{rs.ch4}, rs=>{rs.ch4_stat}),
          MonitorType.withName("A288")->(rs=>{rs.monitor_humid}, rs=>{rs.monitor_humid_stat}),
          MonitorType.withName("A289")->(rs=>{rs.monitor_temp}, rs=>{rs.monitor_temp_stat}),
          MonitorType.withName("A293")->(rs=>{rs.no2}, rs=>{rs.no2_stat}),
          MonitorType.withName("A296")->(rs=>{rs.nmhc}, rs=>{rs.nmhc_stat}),
          MonitorType.withName("C211")->(rs=>{rs.wind_speed}, rs=>{rs.wind_speed_stat}),
          MonitorType.withName("C212")->(rs=>{rs.wind_dir}, rs=>{rs.wind_dir_stat}),
          MonitorType.withName("C213")->(rs=>{rs.rain}, rs=>{rs.rain_stat}),
          MonitorType.withName("C214")->(rs=>{rs.temp}, rs=>{rs.temp_stat}),
          MonitorType.withName("C215")->(rs=>{rs.humid}, rs=>{rs.humid_stat}),
          MonitorType.withName("C216")->(rs=>{rs.air_pressure}, rs=>{rs.air_pressure_stat})
      )

  val monitorTypeProject2:Map[MonitorType.Value, HourRecord=>(Option[Float], Option[String])] = Map(
          MonitorType.withName("A213")->(rs=>(rs.tsp, rs.tsp_stat)),
          MonitorType.withName("A214")->(rs=>(rs.pm10, rs.pm10_stat)),
          MonitorType.withName("A215")->(rs=>(rs.pm25,rs.pm25_stat)),
          MonitorType.withName("A221")->(rs=>(rs.s, rs.s_stat)),
          MonitorType.withName("A222")->(rs=>(rs.so2, rs.so2_stat)),
          MonitorType.withName("A223")->(rs=>(rs.nox, rs.nox_stat)),
          MonitorType.withName("A224")->(rs=>(rs.co, rs.co_stat)),
          MonitorType.withName("A225")->(rs=>(rs.o3, rs.o3_stat)),
          MonitorType.withName("A226")->(rs=>(rs.thc, rs.thc_stat)),
          MonitorType.withName("A229")->(rs=>(rs.ammonia, rs.ammonia_stat)),
          MonitorType.withName("A232")->(rs=>(rs.noy, rs.noy_stat)),
          MonitorType.withName("A233")->(rs=>(rs.noy_no, rs.noy_no_stat)),
          MonitorType.withName("A235")->(rs=>(rs.nh3, rs.nh3_stat)),
          MonitorType.withName("A283")->(rs=>(rs.no, rs.no_stat)),
          MonitorType.withName("A286")->(rs=>(rs.ch4, rs.ch4_stat)),
          MonitorType.withName("A288")->(rs=>(rs.monitor_humid, rs.monitor_humid_stat)),
          MonitorType.withName("A289")->(rs=>(rs.monitor_temp, rs.monitor_temp_stat)),
          MonitorType.withName("A293")->(rs=>(rs.no2, rs.no2_stat)),
          MonitorType.withName("A296")->(rs=>(rs.nmhc, rs.nmhc_stat)),
          MonitorType.withName("C211")->(rs=>(rs.wind_speed, rs.wind_speed_stat)),
          MonitorType.withName("C212")->(rs=>(rs.wind_dir, rs.wind_dir_stat)),
          MonitorType.withName("C213")->(rs=>(rs.rain, rs.rain_stat)),
          MonitorType.withName("C214")->(rs=>(rs.temp, rs.temp_stat)),
          MonitorType.withName("C215")->(rs=>(rs.humid, rs.humid_stat)),
          MonitorType.withName("C216")->(rs=>(rs.air_pressure, rs.air_pressure_stat))
      )
      
  def emptyHourRecord(monitor:String, start: DateTime) = {
      HourRecord(
    monitor,
    start,
    None, None, None, None, None, None,None, None, None, None,
    None, None, None, None, None, None,None, None, None, None, 
    None, None, None, None, None, None,None, None, None, None, 
    None, None, None, None, None, None,None, None, None, None, 
    None, None, None, None, None, None,None, None, None, None,
    None)
  }

  def getDailyReport(monitor: Monitor.Value, start: DateTime, includeTypes:List[MonitorType.Value]=MonitorType.mtvList) = {
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
              if(checkList.isEmpty || checkTime != checkList.head.date)
                emptyHourRecord(monitor.toString(), checkTime) :: checkHourRecord(checkTime + 1.hour, checkList)
              else
                checkList.head :: checkHourRecord(checkTime + 1.hour, checkList.tail)
            }
          }
          checkHourRecord(start, originalHourRecordList)
        }

      val usedMonitoredTypes = MonitoredType.getUsedMonitoredType(monitor).filter { includeTypes.contains(_) }

      val actualMonitoredTypes = 
        if(usedMonitoredTypes.length == 0)
          includeTypes
        else
          usedMonitoredTypes
          
      val typeResultList =
        for {
          t <- monitorTypeProjection.filter(kv=>actualMonitoredTypes.contains(kv._1))
          monitorType = t._1
          total = reportList.size
          projections = reportList.map(rs => (rs.date, t._2._1(rs), t._2._2(rs)))
          validStat = { t: (Timestamp, Option[Float], Option[String]) =>
            {
              t._3 match {
                case Some(s) =>
                  if (isValidStat(s)) {
                    t._2 != None
                  } else
                    false
                case _ => false
              }
            }
          }

          validValues = projections.filter(validStat).map(t => t._2.getOrElse {
            Logger.error("Unexpected Null value!")
            0f
          })
          count = validValues.length
          
          max = if (count != 0) validValues.max else Float.MinValue
          min = if (count != 0) validValues.min else Float.MaxValue          
        } yield {          
          val avg = if(MonitorType.windDirList.contains(monitorType)){
            val sum_sin = validValues.map(v=>Math.sin(Math.toRadians(v))).sum
            val sum_cos = validValues.map(v=>Math.cos(Math.toRadians(v))).sum
            Math.toDegrees(Math.atan (sum_sin/sum_cos)).toFloat
          } else{
            val sum = validValues.sum
            if (count != 0) sum / count else 0
          }
            
          val stat = Stat(avg, min, max, count, total, 0)
          //Logger.info(MonitorType.map(t._1).toString() + stat.toString())
          MonitorTypeRecord(t._1, projections, stat)
        }
        
        DailyReport(typeResultList.toArray)
    }
  }
  
}