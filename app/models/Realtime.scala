package models
import scalikejdbc._
import java.sql.Date
import java.sql.Timestamp
import scalikejdbc._
import play.api._
import com.github.nscala_time.time.Imports._
import com.github.nscala_time.time._
import models.ModelHelper._
import HourRecord._

case class RealtimeStatus(data:Map[Monitor.Value, Map[MonitorType.Value, (Option[Float], Option[String])]])
case class SixSecRecord(c911:Array[(Option[Float], Option[String])], c912:Array[(Option[Float], Option[String])])
object Realtime {
  def SixSecRecordMapper(rs: WrappedResultSet) = {
    val c911 =
    for{i <- 0 to 9}
    yield{
       (rs.floatOpt(4+2*i), rs.stringOpt(5+2*i))
    }
    val c912 = 
      for{i <- 0 to 9}
        yield{
          (rs.floatOpt(24+2*i), rs.stringOpt(25+2*i))
        }
    SixSecRecord(c911.toArray, c912.toArray)
  }

  def getRealtimeStatusHour() = {
    
    DB readOnly { implicit session =>
      val rt_result =
        for { m <- Monitor.monitorList } yield {
          val optHr =
            sql"""
              SELECT TOP 1 *
              FROM [AQMDB_M1_2014].[dbo].[P1234567_M1_2014]
              WHERE DP_NO = ${m.id}
              ORDER BY M_DateTime  DESC
             """.map { HourRecord.mapper }.single.apply

          val hr = optHr.getOrElse(emptyHourRecord(m.id, DateTime.now))
          val type_record = monitorTypeProjection.map(
            t => (t._1 -> (t._2._1(hr), t._2._2(hr))))
          (Monitor.withName(m.id) -> type_record)
        }
      Logger.info(DateTime.now.toString + "rt_result")
      Map(rt_result: _*)
    }
  }
  
  def getRealtimeStatus() = {
    Logger.info(DateTime.now.toString + "Enter")
    DB readOnly { implicit session =>
      val rt_result =
        for { m <- Monitor.monitorList } yield {
          val optHr =
            sql"""
              SELECT TOP 1 *
              FROM [AQMDB_M1_2014].[dbo].[P1234567_M1_2014]
              WHERE DP_NO = ${m.id}
              ORDER BY M_DateTime  DESC
             """.map { HourRecord.mapper }.single.apply

          val hr = optHr.getOrElse(emptyHourRecord(m.id, DateTime.now))
          val type_record = monitorTypeProjection.map(
            t => (t._1 -> (t._2._1(hr), t._2._2(hr))))
          (Monitor.withName(m.id) -> type_record)
        }
      Logger.info(DateTime.now.toString + "rt_result")  
      val windMapList =
        for { m <- Monitor.monitorList } yield {
          val wind_record =
            sql"""
            SELECT TOP 1 *
            FROM [AQMDB_M1_2014].[dbo].[P1234567_S6_2014]
            WHERE DP_NO = ${m.id}
            ORDER BY M_DateTime  DESC
           """.map { SixSecRecordMapper }.single.apply.getOrElse(
              SixSecRecord(Array[(Option[Float], Option[String])]((None, None)), Array[(Option[Float], Option[String])]((None, None))))

          def getLatest(arr: Array[(Option[Float], Option[String])]): (Option[Float], Option[String]) = {
             arr.last
          }

          Monitor.withName(m.id) -> Map(MonitorType.withName("C911") -> getLatest(wind_record.c911), 
              MonitorType.withName("C912") -> getLatest(wind_record.c912))
        }
        Logger.info(DateTime.now.toString + "windMapList")
      //combine
        val hr_map = Map(rt_result: _*)
        val sec_map = Map(windMapList: _*)
        val final_list =
          for { m <- Monitor.mvList } yield {
            m->(hr_map(m) ++ sec_map(m)) 
          }
        Logger.info(DateTime.now.toString + "combined")
        Map(final_list: _*)
    }
  }
  
  def getMonitorTypeAvg(monitor:Monitor.Value, monitorType:MonitorType.Value, start:DateTime, end:DateTime)(implicit session: DBSession = AutoSession)={
    val records = getHourRecords(monitor, start, end)
    val typeValues = records.map { hr=>monitorTypeProject2(monitorType)(hr)}
    val validValues = typeValues.filter(v=>(!v._2.isEmpty)&&(HourRecord.isValidStat(v._2.get))).map(_._1.get)
    val total = validValues.length
    val sum = validValues.sum
    sum/total 
  }
  
  def pm10PSI(v:Float)={
    if(v>=0 && v<=50){
      v
    }else if(v<=100){
      50 + (v-50)*50/100
    }else if(v<=350){
      100 + (v-150)*100/(350-150)
    }else if(v<=420){
      200 + (v-350)*100/(420-350)
    }else if(v<=500){
      300 + (v-420)*100/(500-420)
    }else {
      400 + (v-500)*100/100
    }
  }
  
  def so2PSI(v:Float)={
    if(v<=30){
      v/30*50
    }else if(v<=140){
      50 + (v-30)*50/(140-30)
    }else if(v<=300){
      100 + (v-140)*100/(300-140)
    }else if(v<=600){
      200 + (v-300)*100/(600-300)
    }else if(v<=800){
      300 + (v-600)*100/(800-600)
    }else {
      400 + (v-800)*100/(1000-800)
    }
  }
  
  def coPSI(v:Float):Float={
    if(v<=4.5){
      v/4.5f*50f
    }else if(v<=9){
      (50 + (v-4.5)*50/(9-4.5)).toFloat
    }else if(v<=15){
      100 + (v-9)*100/(15-9)
    }else if(v<=30){
      200 + (v-15)*100/(30-9)
    }else if(v<=40){
      300 + (v-30)*100/(40-30)
    }else {
      400 + (v-40)*100/(50-40)
    }
  }

  def o3PSI(v:Float)={
    if(v<=60){
      v/60*50
    }else if(v<=120){
      50 + (v-60)*50/(120-60)
    }else if(v<=200){
      100 + (v-120)*100/(200-120)
    }else if(v<=400){
      200 + (v-200)*100/(400-200)
    }else if(v<=500){
      300 + (v-400)*100/(500-400)
    }else {
      400 + (v-500)*100/(600-500)
    }
  }
  
  def no2PSI(v:Float)={
    if(v<600){
      0
    }else if(v<=1200){
      200 + (v-600)*100/(1200-600)
    }else if(v<=1600){
      300 + (v-1200)*100/(1600-1200)
    }else {
      400 + (v-1600)*100/(2000-1600)
    }
  }
  
  def getMonitorRealtimePSI(monitor:Monitor.Value)(implicit session: DBSession = AutoSession)={
    val currentOpt =
    sql"""
      SELECT TOP 1 M_DateTime
      FROM [AQMDB_M1_2014].[dbo].[P1234567_M1_2014]
      WHERE DP_NO = ${monitor.toString}
      ORDER BY M_DateTime  DESC
      """.map { r=>r.timestamp(1) }.single.apply
      
      if(currentOpt.isEmpty)
        None
      else{
        val current = new DateTime(currentOpt.get.millis)
        val end = current + 1.hour
        val start = current - 11.hour
        
        val pm10_12 = getMonitorTypeAvg(monitor, MonitorType.withName("A214"), current - 11.hour, current + 1.hour)
        val pm10_4 = getMonitorTypeAvg(monitor, MonitorType.withName("A214"), current - 3.hour, current + 1.hour)
        val pm10 = (pm10_12 + pm10_4)/2
        
        val so2_24 = getMonitorTypeAvg(monitor, MonitorType.withName("A222"), current - 23.hour, current + 1.hour)
        
        val co_8 = getMonitorTypeAvg(monitor, MonitorType.withName("A224"), current - 7.hour, current + 1.hour)
        
        val o3 = getMonitorTypeAvg(monitor, MonitorType.withName("A225"), current, current + 1.hour)
        
        val no2 = getMonitorTypeAvg(monitor, MonitorType.withName("A293"), current, current + 1.hour)
        val result = Map[MonitorType.Value, (Float, Float)](
            MonitorType.withName("A214")->(pm10, pm10PSI(pm10)),
            MonitorType.withName("A222")->(so2_24, so2PSI(so2_24)),
            MonitorType.withName("A224")->(co_8, coPSI(co_8)),
            MonitorType.withName("A225")->(o3, o3PSI(o3)),
            MonitorType.withName("A293")->(no2, no2PSI(no2))
        )
        val sub_psi = result.values.map(_._2)
        val psi = sub_psi.toList.max
        Some((psi, result))
      }
  }
  def getRealtimePSI = {
    val result =
      for {
        m <- Monitor.mvList
      } yield {
        m -> getMonitorRealtimePSI(m)
      }
    Map(result: _*)
  }
  
  def realtimeMonitorTrend(monitors:Seq[Monitor.Value], monitorType:MonitorType.Value)(implicit session: DBSession = AutoSession)={
    val current = DateTime.parse("2014-10-31 23:00:00", StaticDateTimeFormat.forPattern("yyyy-MM-dd HH:mm:ss"))
    def fillMissingRecord(monitor:Monitor.Value ,data:List[HourRecord], expected:DateTime):List[HourRecord]={
      if(expected > current)
        Nil
      else if(data.isEmpty || data.head.date.millis != expected.getMillis){
        if(!data.isEmpty){
          Logger.debug("expected:"+ expected +"original:"+data.head.date.toString())          
        }
        
        HourRecord.emptyHourRecord(Monitor.map(monitor).id, expected) :: 
          fillMissingRecord(monitor, data, expected + 1.hour)
      }else{
        Logger.debug("as expected")
        data.head :: fillMissingRecord(monitor, data.tail, expected + 1.hour)
      }
    }
    
    val result = 
    for{m<-monitors
      hrList =
      sql"""
      SELECT TOP 9 *
      FROM [AQMDB_M1_2014].[dbo].[P1234567_M1_2014]
      WHERE DP_NO = ${Monitor.map(m).id}
      ORDER BY M_DateTime  DESC
      """.map {HourRecord.mapper }.list.apply
      filledList = fillMissingRecord(m, hrList.reverse, current - 8.hour)
      v = filledList.map{ monitorTypeProject2(monitorType)}
    }yield{ 
      m->v
    }
    
    Map(result :_*)
  }
}