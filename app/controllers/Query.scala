package controllers
import play.api._
import play.api.mvc._
import play.api.Logger
import models._
import com.github.nscala_time.time.Imports._
import models.ModelHelper._
import play.api.libs.json._
import play.api.libs.functional.syntax._

object Query extends Controller{

  def history() = Security.Authenticated {
    implicit request =>
    val userInfo = Security.getUserinfo(request).get  
    Ok(views.html.history(false, userInfo.groupID))
  }
  
  def historyReport(edit:Boolean, monitorStr:String, monitorTypeStr:String, startStr:String, endStr:String)=Security.Authenticated {
    implicit request =>
      
    import scala.collection.JavaConverters._
    val monitorStrArray = monitorStr.split(':')
    val monitors = monitorStrArray.map{Monitor.withName}
    val monitorType = MonitorType.withName(monitorTypeStr)
    val start = DateTime.parse(startStr)
    val end = DateTime.parse(endStr) + 1.day
    
    var timeSet = Set[DateTime]()
    val pairs =
    for{m <- monitors
      records = Record.getHourRecords(m, start, end)
      mtRecords = records.map {rs=>(Record.timeProjection(rs).toDateTime, Record.monitorTypeProject2(monitorType)(rs))}
      timeMap = Map(mtRecords :_*)
      }
    yield{
      timeSet ++= timeMap.keySet      
      (m -> timeMap)
    }
    
    val recordMap = Map(pairs :_*)
    
    Ok(views.html.historyReport(edit, monitors, monitorType, start, end, timeSet.toList.sorted, recordMap))
    
  }
  
  def historyTrend = Security.Authenticated {
    implicit request =>
      val userInfo = Security.getUserinfo(request).get
    Ok(views.html.historyTrend(userInfo.groupID))
  }

  def historyTrendChart(monitorStr: String, monitorTypeStr: String, startStr: String, endStr: String) = Security.Authenticated {
    implicit request =>
      import scala.collection.JavaConverters._
      val monitorStrArray = monitorStr.split(':')
      val monitors = monitorStrArray.map { Monitor.withName }
      val monitorType = MonitorType.withName(monitorTypeStr)
      val mtCase = MonitorType.map(monitorType)
      val start = DateTime.parse(startStr)
      val end = DateTime.parse(endStr) + 1.day

      var timeSet = Set[DateTime]()
      val pairs =
        for {
          m <- monitors
          records = Record.getHourRecords(m, start, end)
          mtRecords = records.map { rs => (Record.timeProjection(rs).toDateTime, Record.monitorTypeProject2(monitorType)(rs)) }
          timeMap = Map(mtRecords: _*)
        } yield {
          timeSet ++= timeMap.keySet
          (m -> timeMap)
        }

      val recordMap = Map(pairs: _*)
      val timeSeq = timeSet.toList.sorted
      import Realtime._
      
      val series = for {m <- monitors
        timeData = timeSeq.map(t=>recordMap(m).getOrElse(t, (Some(0f), Some(""))))
        data = timeData.map(_._1.getOrElse(0f))            
      } 
      yield {
        seqData(Monitor.map(m).name, data)
      }

      val title = mtCase.desp + "歷史趨勢圖"
      val axisLines = if (mtCase.std_internal.isEmpty || mtCase.std_law.isEmpty)
        None
      else {
        Some(Seq(AxisLine("#0000FF", 2, mtCase.std_internal.get, Some(AxisLineLabel("left", "內控值"))),
          AxisLine("#FF0000", 2, mtCase.std_law.get, Some(AxisLineLabel("right", "法規值")))))
      }

      val timeStrSeq = timeSeq.map(_.toString("YYYY/MM/dd HH:mm"))
      val c = HighchartData(
        Map("type" -> "line"),
        Map("text"->title),
        XAxis(Some(timeStrSeq)),
        YAxis(None, AxisTitle(Some(mtCase.unit)), axisLines),
        series)

      Results.Ok(Json.toJson(c))
  }
  
  def psiTrend = Security.Authenticated {
    implicit request =>
      val userInfo = Security.getUserinfo(request).get
    Ok(views.html.psiTrend(userInfo.groupID))
  }

  def psiTrendChart(monitorStr: String, startStr: String, endStr: String) = Security.Authenticated {
    implicit request =>
      import scala.collection.JavaConverters._
      val monitorStrArray = monitorStr.split(':')
      val monitors = monitorStrArray.map { Monitor.withName }
      val start = DateTime.parse(startStr)
      val end = DateTime.parse(endStr) + 1.day

      import scala.collection.mutable.Map
      def getPsiMap(m: Monitor.Value) = {
        import models.Realtime._
        var current = start

        val map = Map[DateTime, Float]()
        while (current < end) {
          val v = getMonitorDailyPSI(m, current)
          val psi = v.psi.getOrElse(0f)
          map += (current -> psi)
          current += 1.day
        }
        map
      }

      val monitorPsiMap = Map[Monitor.Value, Map[DateTime, Float]]()
      for (m <- monitors) {
        monitorPsiMap += (m -> getPsiMap(m))
      }

      val title="PSI歷史趨勢圖"
      val timeSeq = monitorPsiMap.values.head.keys.toList.sorted
      import Realtime._
      
      val series = for {
        m <- monitors
        timeData = timeSeq.map(t => monitorPsiMap(m)(t))
      } yield {
        seqData(Monitor.map(m).name, timeData)
      }

      val timeStrSeq = timeSeq.map(_.toString("YYYY/MM/dd"))
      val c = HighchartData(
        scala.collection.immutable.Map("type" -> "column"),
        scala.collection.immutable.Map("text" -> title),
        XAxis(Some(timeStrSeq)),
        YAxis(None, AxisTitle(Some("")), None),
        series)

      Results.Ok(Json.toJson(c))

  }
  
  def overLawStd() = Security.Authenticated {
    implicit request =>
      val userInfo = Security.getUserinfo(request).get
    Ok(views.html.overLawStd(userInfo.groupID))
  }
  
  case class OverLawStdEntry(monitor:Monitor.Value, time:DateTime, value:Float)
  def overLawStdReport(monitorStr:String, monitorTypeStr:String, startStr:String, endStr:String)=Security.Authenticated {
    implicit request =>
    import scala.collection.JavaConverters._
    val monitorStrArray = monitorStr.split(':')
    val monitors = monitorStrArray.map{Monitor.withName}
    val monitorType = MonitorType.withName(monitorTypeStr)
    val mtCase = MonitorType.map(monitorType)
    val start = DateTime.parse(startStr)
    val end = DateTime.parse(endStr) + 1.day
    
    assert(mtCase.sd_law.isDefined)
    
    import models.Record._
    import scala.collection.mutable.ListBuffer
    val result = ListBuffer[OverLawStdEntry]()
    for{m<-monitors
      records = Record.getHourRecords(m, start, end)
      typeRecords = records.map{r=>(Record.timeProjection(r) ,Record.monitorTypeProject2(monitorType)(r))}
      overLawRecords = typeRecords.filter{
        r=>(r._2._1.isDefined && r._2._1.get > mtCase.sd_law.get)
      }
      overList = overLawRecords.map{r=>OverLawStdEntry(m, r._1, r._2._1.get)}
    }{
      result ++= overList  
    }
      
    Ok(views.html.overLawStdReport(monitorType, start, end, result))
  }
  
  def effectivePercentage() = Security.Authenticated {
    implicit request =>
      val userInfo = Security.getUserinfo(request).get
    Ok(views.html.effectivePercentage(userInfo.groupID))
  }
  
  def effectivePercentageReport(startStr:String, endStr:String)=Security.Authenticated {
       implicit request =>
         val userInfo = Security.getUserinfo(request).get
    val start = DateTime.parse(startStr)
    val end = DateTime.parse(endStr) + 1.day
    
    val reports = 
    for(m <- Monitor.getMvList(userInfo.groupID))
      yield{
      Record.getMonitorEffectiveRate(m, start, end)
    }
    Ok(views.html.effectivePercentageReport(start, end, reports))
  }
  
  def alarm() = Security.Authenticated {
    implicit request =>
      val userInfo = Security.getUserinfo(request).get
    Ok(views.html.alarm(userInfo.groupID))
  }

  def alarmReport(monitorStr: String, statusStr: String, startStr: String, endStr: String) = Security.Authenticated {
    val monitorStrArray = monitorStr.split(':')
    val monitors = monitorStrArray.map { Monitor.withName }
    val statusFilter = if (statusStr.equalsIgnoreCase("none")) {
      None
    } else {
      Some(statusStr.split(':').toList)
    }
    val start = DateTime.parse(startStr)
    val end = DateTime.parse(endStr) + 1.day
   
    val records = Alarm.getAlarm(monitors, statusFilter, start, end)
    
    Ok(views.html.alarmReport(start, end, records))
  }
  
  def windRose() = Security.Authenticated {
    implicit request =>
      val userInfo = Security.getUserinfo(request).get
    Ok(views.html.windRose(userInfo.groupID))
  }
  
  def windRoseReport(monitorStr: String, startStr: String, endStr: String) = Security.Authenticated {
    val monitor = Monitor.withName(monitorStr)
    val start = DateTime.parse(startStr)
    val end = DateTime.parse(endStr) + 1.day
   
    val windMap = Record.getWindRose(monitor, start, end)
    val dirMap = Map(
      (0->"北"), (1->"北北東"), (2->"東北"), (3->"東北東"), (4->"東"),
      (5->"東南東"), (6->"東南"), (7->"南南東"), (8->"南"),
      (9->"南南西"), (10->"西南"), (11->"西西南"), (12->"西"),
      (13->"西北西"), (14->"西北"), (15->"北北西")
    )
    val dirStrSeq = 
      for(dir <- 0 to 15)
        yield
        dirMap(dir)
    
    val speedLevel = Array(
        "<0.5 m/s", "0.5-2 m/s", "2-4 m/s", "4-6 m/s", "6-8 m/s","8-10 m/s", ">10 m/s"
    )
      
      import Realtime._
      
      val series = for {
        level <- 0 to 6
      } yield {
        val data =
          for( dir <- 0 to 15)
            yield
            windMap(dir)(level)
            
        seqData(speedLevel(level), data)
      }

      val title="風瑰圖"
      val c = HighchartData(
        scala.collection.immutable.Map("polar" -> "true", "type"->"column"),
        scala.collection.immutable.Map("text" -> title),
        XAxis(Some(dirStrSeq)),
        YAxis(None, AxisTitle(Some("")), None),
        series)

      Results.Ok(Json.toJson(c))

  }
  
  def compareLastYear() = Security.Authenticated {
    implicit request =>
      val userInfo = Security.getUserinfo(request).get
    Ok(views.html.compareLastYear(userInfo.groupID))
  }
  
  def compareLastYearChart(monitorStr:String, monitorTypeStr:String, startStr:String, endStr:String)=Security.Authenticated {
    implicit request =>
    import scala.collection.JavaConverters._
    val monitor = Monitor.withName(monitorStr)
    val monitorType = MonitorType.withName(monitorTypeStr)
    val mtCase = MonitorType.map(monitorType)
    val start = DateTime.parse(startStr)
    val end = DateTime.parse(endStr) + 1.day
        
    val (thisYearRecord, lastYearRecord) = Record.getLastYearCompareList(monitor, monitorType, start, end)
    
    val title=s"${Monitor.map(monitor).name} ${MonitorType.map(monitorType).desp}同期比較圖"
    
    val timeStrSeq = thisYearRecord.map(_._1.toDateTime.toString("MM-dd hh:00"))
    val (t1, v1) = thisYearRecord.unzip
    val (t2, v2) = lastYearRecord.unzip
    
    import Realtime._
    
    val series = Seq( seqData(start.getYear.toString(), v1), seqData((start-1.year).getYear.toString(), v2))

      
    val c = HighchartData(
        scala.collection.immutable.Map("type" -> "line"),
        scala.collection.immutable.Map("text" -> title),
        XAxis(Some(timeStrSeq)),
        YAxis(None, AxisTitle(Some("")), None),
        series)

      Results.Ok(Json.toJson(c))
  }

  def calculateStat() = Security.Authenticated {
    implicit request =>
      val userInfo = Security.getUserinfo(request).get
    Ok(views.html.calculateStat(userInfo.groupID))
  }
  
  case class Stat(avg:Float, min:Float, max:Float, sd:Float)
  def calculateStatReport(monitorStr:String, monitorTypeStr:String, startStr:String, endStr:String)=Security.Authenticated {
    implicit request =>
    import scala.collection.JavaConverters._
    val monitorStrArray = monitorStr.split(':')
    val monitors = monitorStrArray.map{Monitor.withName}
    val monitorType = MonitorType.withName(monitorTypeStr)
    val start = DateTime.parse(startStr)
    val end = DateTime.parse(endStr) + 1.day
    
    import models.Record._
    import MonitorStatus._
    import scala.collection.mutable.ListBuffer
    val result =
    for{m<-monitors
      records = Record.getHourRecords(m, start, end)
      typeRecords = records.map{r=>Record.monitorTypeProject2(monitorType)(r)}
      normalRecords = typeRecords.filter{
        r=>(r._1.isDefined && r._2.isDefined && MonitorStatus.isNormalStat(getTagInfo(r._2.get).toString))
      }.map(_._1.get)
      len = normalRecords.length
      avg = normalRecords.sum/len
      max = if(len>0) normalRecords.max else Float.NaN
      min = if(len>0) normalRecords.min else Float.NaN
      dev = if(len>0) normalRecords.map(r=>(r-avg)*(r-avg)) else List[Float]()
      sd = if(len>0) Math.sqrt(dev.sum/len) else Double.NaN
    }
    yield
    {
      (m -> Stat(avg, max, min, sd.toFloat))
    }  
    
    val statMap = Map(result :_*)
    
    Ok(views.html.calculateStatReport(monitorType, start, end, statMap))
  }
  
  def regression() = Security.Authenticated {
    implicit request =>
      val userInfo = Security.getUserinfo(request).get
    Ok(views.html.regression(userInfo.groupID))
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
      val v3 = v2.map{v=>Seq(v._2, v._1)}

      val series = Seq(seqData2(start.getYear.toString(), v3))

      val c = RegressionChartData(
        Map(("type"->"scatter"),
                ("zoomType"->"xy")),
        Map("text" -> title),
        XAxis(Some(timeStrSeq)),
        YAxis(None, AxisTitle(Some("")), None),
        series)

      Results.Ok(Json.toJson(c))
  }

  def calibrationQuery = Security.Authenticated {
    implicit request =>
      val userInfo = Security.getUserinfo(request).get
      Ok(views.html.calibration(userInfo.groupID))
  }
  
  def calibrationQueryReport(monitorStr:String, startStr:String, endStr:String) = Security.Authenticated {
    implicit request =>
      Logger.info("calibrationQueryResult")
      val monitor = Monitor.withName(monitorStr)
      val start = DateTime.parse(startStr)
      val end = DateTime.parse(endStr) + 1.day
      val result = Calibration.calibrationQueryReport(monitor, start, end)
      Ok(views.html.calibrationQueryResult(result))
  }
  
}