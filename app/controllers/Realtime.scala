package controllers
import play.api._
import play.api.mvc._
import play.api.Logger
import models._
import models.Realtime._
import com.github.nscala_time.time.Imports._
import models.ModelHelper._
import play.api.libs.json._
import play.api.libs.functional.syntax._
import PdfUtility._
import models.ModelHelper._

object Realtime extends Controller {
  def realtimeStat(outputTypeStr: String) = Security.Authenticated {
    implicit request =>
      val userInfo = Security.getUserinfo(request).get
      val group = Group.getGroup(userInfo.groupID).get
      val outputType = OutputType.withName(outputTypeStr)

      val current = getLatestRecordTime(TableType.Min).get
      val rt_status = getRealtimeMinStatus(current, group.privilege)
      val currentHr = getLatestRecordTime(TableType.Hour).get
      val rt_psi = getRealtimePSI(currentHr)
      val output = views.html.realtimeStatus(current, rt_status, MonitorType.psiList, rt_psi, group.privilege)
      val title = "即時資訊"
      outputType match {
        case OutputType.html =>
          Ok(output)
        case OutputType.pdf =>
          Ok.sendFile(creatPdfWithReportHeader(title, output),
            fileName = _ =>
              play.utils.UriEncoding.encodePathSegment(title + current.toString("YYMMdd_hhmm") + ".pdf", "UTF-8"))
      }
  }

  def realtimeImg = Security.Authenticated {
    implicit request =>
      val userInfo = Security.getUserinfo(request).get
      val group = Group.getGroup(userInfo.groupID).get
      Ok(views.html.realtimeImage(group.privilege))
  }

  def realtimeTrend() = Security.Authenticated {
    implicit request =>
      val userInfo = Security.getUserinfo(request).get
      val group = Group.getGroup(userInfo.groupID).get      
      Ok(views.html.realtimeTrend(group.privilege, false))
  }
  
  def realtimeMinTrend() = Security.Authenticated {
    implicit request =>
     val userInfo = Security.getUserinfo(request).get
      val group = Group.getGroup(userInfo.groupID).get      
      Ok(views.html.realtimeTrend(group.privilege, true))
  }

  def realtimeHourTrendChart(monitorStr: String, monitorTypeStr: String) = Security.Authenticated{
    implicit request =>
      val monitorStrArray = monitorStr.split(':')
      val monitors = monitorStrArray.map { Monitor.withName }
      val monitorTypeStrArray = monitorTypeStr.split(':')
      val monitorTypes = monitorTypeStrArray.map { MonitorType.withName }

      val current = getLatestRecordTime(TableType.Hour).get
      val reportUnit = ReportUnit.Hour
      val monitorStatusFilter = MonitorStatusFilter.Normal_Over
      val start = current.toDateTime - 1.day
      val end = current

      import Query.trendHelper
      val chart = trendHelper(monitors, Array.empty[EpaMonitor.Value], monitorTypes, reportUnit, monitorStatusFilter, start, end)
      
      Results.Ok(Json.toJson(chart))
  }

  def realtimeMinTrendChart(monitorStr: String, monitorTypeStr: String) = Security.Authenticated{
    implicit request =>
      val monitorStrArray = monitorStr.split(':')
      val monitors = monitorStrArray.map { Monitor.withName }
      val monitorTypeStrArray = monitorTypeStr.split(':')
      val monitorTypes = monitorTypeStrArray.map { MonitorType.withName }

      val current = getLatestRecordTime(TableType.Min).get
      val reportUnit = ReportUnit.Min
      val monitorStatusFilter = MonitorStatusFilter.Normal_Over
      val start = current.toDateTime - 4.hour
      val end = current

      import Query.trendHelper
      val chart = trendHelper(monitors, Array.empty[EpaMonitor.Value], monitorTypes, reportUnit, monitorStatusFilter, start, end)
      
      Results.Ok(Json.toJson(chart))
  }

  case class XAxis(categories: Option[Seq[String]])
  case class AxisLineLabel(align: String, text: String)
  case class AxisLine(color: String, width: Int, value: Float, label: Option[AxisLineLabel])
  case class AxisTitle(text: Option[String])
  case class YAxis(labels: Option[String], title: AxisTitle, plotLines: Option[Seq[AxisLine]], opposite:Boolean=false)
  case class seqData(name: String, data: Seq[Float], yAxis:Int=0, chartType:Option[String]=None)
  case class HighchartData(chart: Map[String, String],
                           title: Map[String, String],
                           xAxis: XAxis,
                           yAxis: Seq[YAxis],
                           series: Seq[seqData])
  case class FrequencyTab(header:Seq[String], body:Seq[Seq[String]], footer:Seq[String])                         
  case class WindRoseReport(chart:HighchartData, table:FrequencyTab)
  implicit val xaWrite = Json.writes[XAxis]
  implicit val axisLineLabelWrite = Json.writes[AxisLineLabel]
  implicit val axisLineWrite = Json.writes[AxisLine]
  implicit val axisTitleWrite = Json.writes[AxisTitle]
  implicit val yaWrite = Json.writes[YAxis]
  implicit val seqDataWrite:Writes[seqData] = (
    (__ \ "name").write[String] and
    (__ \ "data").write[Seq[Float]] and
    (__ \ "yAxis").write[Int] and
    (__ \ "type").write[Option[String]]
  )(unlift(seqData.unapply))
  implicit val hcWrite = Json.writes[HighchartData]
  implicit val feqWrite = Json.writes[FrequencyTab]
  implicit val wrWrite = Json.writes[WindRoseReport]

  def highchartJson(monitorTypeStr: String) = Security.Authenticated {
    implicit request =>
      val userInfo = Security.getUserinfo(request).get
      val group = Group.getGroup(userInfo.groupID).get

      val mt = MonitorType.withName(monitorTypeStr)
      val mtCase = MonitorType.map(mt)

      val latestRecordTime = getLatestRecordTime(TableType.Min).get

      val realtimeValueMap =
        if (group.privilege.allowedMonitorTypes.contains(mt))
          getRealtimeMonitorValueMap(mt, latestRecordTime)
        else {
          Map[Monitor.Value, (Option[Float], Option[String])]()
        }

      val series = for (m <- group.privilege.allowedMonitors) yield {
        seqData(Monitor.map(m).name, Seq({
          val vOpt = realtimeValueMap.get(m)
          if (vOpt.isEmpty || vOpt.get._1.isEmpty || vOpt.get._2.isEmpty)
            0f
          else {
            val value = vOpt.get._1.get
            val status = vOpt.get._2.get
            if (MonitorStatus.isNormalStat(status))
              value
            else
              0f
          }

        }))
      }

      val title = mtCase.desp + " 即時資料"
      val axisLines = if (mtCase.std_internal.isEmpty || mtCase.std_law.isEmpty)
        None
      else {
        Some(Seq(AxisLine("#0000FF", 2, mtCase.std_internal.get, None),
          AxisLine("#FF0000", 2, mtCase.std_law.get, None)))
      }

      val c = HighchartData(
        Map("type" -> "column"),
        Map("text" -> title),
        XAxis(Some(Seq(latestRecordTime.toString("yyyy-MM-dd HH:mm")))),
        Seq(YAxis(None, AxisTitle(Some(mtCase.unit)), axisLines)),
        series)

      Ok(Json.toJson(c))
  }

  case class MonitorInfo(id: String, status: Int, winDir: Float, winSpeed: Float, statusStr: String)
  case class RealtimeMapInfo(info: Seq[MonitorInfo])

  implicit val monitorInfoWrite = Json.writes[MonitorInfo]
  implicit val mapInfoWrite = Json.writes[RealtimeMapInfo]

  def realtimeMap = Security.Authenticated {
    implicit request =>
      val current = getLatestRecordTime(TableType.SixSec).get
      val weatherMap = getRealtimeWeatherMap(current)
      val statusMap = getRealtimeMonitorStatusMap(current)

      def getStatusIndex(statusMapOpt: Option[Map[MonitorType.Value, Option[String]]]): (Int, String) = {
        val statusBuilder = new StringBuilder
        if (statusMapOpt.isEmpty)
          return (4, s"<strong>所有測項:${MonitorStatus.map(MonitorStatus.DATA_LOSS_STAT).desp}</strong>")

        val statusMap = statusMapOpt.get
        val statusIndexes = statusMap.map { mt_status =>
          val status = mt_status._2.getOrElse(MonitorStatus.NORMAL_STAT)
          if (MonitorStatus.isNormalStat(status))
            0
          else if (MonitorStatus.isCalbration(status)) {
            statusBuilder.append(s"${MonitorType.map(mt_status._1).desp}:${MonitorStatus.map(status).desp}<br/>")
            1
          } else if (MonitorStatus.isRepairing(status)) {
            statusBuilder.append(s"${MonitorType.map(mt_status._1).desp}:${MonitorStatus.map(status).desp}<br/>")
            2
          } else if (MonitorStatus.isMaintance(status)) {
            statusBuilder.append(s"${MonitorType.map(mt_status._1).desp}:${MonitorStatus.map(status).desp}<br/>")
            3
          } else {
            statusBuilder.append(s"${MonitorType.map(mt_status._1).desp}:${MonitorStatus.map(status).desp}<br/>")
            4
          }
        }
        
        if(statusIndexes.size == 0)
          (0, "")
        else
          (statusIndexes.max, statusBuilder.toString())
      }

      val mapInfos =
        for {
          m <- Monitor.mvList
          weather = weatherMap.getOrElse(m, Record.emptySixSecRecord(m, current, MonitorStatus.DATA_LOSS_STAT))
          status = statusMap.get(m)
        } yield {
          val (statusIndex, statusStr) = getStatusIndex(status)
          MonitorInfo(m.toString(), statusIndex, weather.winDir.last.getOrElse(0f), weather.winSpeed.last.getOrElse(0f), statusStr)
        }

      Ok(Json.toJson(RealtimeMapInfo(mapInfos)))
  }
}