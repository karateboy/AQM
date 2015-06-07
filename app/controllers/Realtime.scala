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

object Realtime extends Controller {
  def realtimeStat = Security.Authenticated {
    implicit request =>
      val current = getLatestRecordTime(TableType.Min).get
      val rt_status = getRealtimeMinStatus(current)
      val rt_psi = getRealtimePSI(current)
      Ok(views.html.realtimeStatus(current, rt_status, MonitorType.psiList, rt_psi))
  }

  def realtimeImg = Security.Authenticated {
    implicit request =>
      Ok(views.html.realtimeImage(""))
  }

  def realtimeTrend = Security.Authenticated {
    implicit request =>
      Ok(views.html.realtimeTrend(""))
  }

  case class RealtimeTrendParam(monitors: Seq[Monitor.Value], monitorTypes: Seq[MonitorType.Value])

  implicit val monitorTypeReader: Reads[MonitorType.Value] = (__ \ "id").read[String].map(MonitorType.withName _)
  implicit val monitorReader: Reads[Monitor.Value] = (__ \ "id").read[String].map(Monitor.withName _)
  implicit val realtimeTrendParamRBuilder: Reads[RealtimeTrendParam] =
    ((__ \ "monitor").read[Seq[Monitor.Value]] and
      (__ \ "monitorType").read[Seq[MonitorType.Value]])(RealtimeTrendParam.apply _)

  case class MorrisBarChart(data: Seq[MorrisBarChartDataElem], xkey: String, ykeys: Seq[String], labels: Seq[String], title: String)
  case class MorrisBarChartDataElem(elem: Seq[(String, String)])
  implicit val morrisBarChartDataElemWritter = new Writes[MorrisBarChartDataElem] {
    def writes(dataElem: MorrisBarChartDataElem): JsObject = {
      val ret1: Seq[(String, Json.JsValueWrapper)] = dataElem.elem.map(r => {
        val wrapper: Json.JsValueWrapper = r._2
        (r._1, wrapper)
      })
      Json.obj(ret1: _*)
    }
  }
  implicit val morrisBarChartWritter: Writes[MorrisBarChart] = (
    (__ \ "data").write[Seq[MorrisBarChartDataElem]] and
    (__ \ "xkey").write[String] and
    (__ \ "ykeys").write[Seq[String]] and
    (__ \ "labels").write[Seq[String]] and
    (__ \ "title").write[String])(unlift(MorrisBarChart.unapply))

  def realtimeTrendJSON = Security.Authenticated(BodyParsers.parse.json) {
    implicit request =>
      val realtimeTrendParam = request.body.validate[RealtimeTrendParam]

      realtimeTrendParam.fold(
        error => {
          Logger.error(JsError.toFlatJson(error).toString())
          BadRequest(Json.obj("ok" -> false, "msg" -> JsError.toFlatJson(error)))
        },
        param => {
          Logger.debug(param.toString)
          assert(param.monitorTypes.length == 1 || param.monitors.length == 1)
          val json =
            if (param.monitorTypes.length != 1) {
              Json.toJson("more than 1 types")
            } else {
              val current = getLatestRecordTime(TableType.Hour).get
              val trend =
                realtimeMonitorTrend(current, param.monitors, param.monitorTypes(0))

              val data =
                for { i <- 8 to 0 by -1 } yield {
                  val hour_data =
                    for {
                      m <- param.monitors
                      record = trend.get(m).get(i)
                    } yield {
                      val value =
                        if (record._2.isDefined)
                          "%.2f".format(record._2.get)
                        else
                          "0"

                      (m.toString() -> value)
                    }
                  //val time = trend.get(param.monitors.head).get(i)._1  
                  //MorrisBarChartDataElem(hour_data :+("time", time.getTime.toString))

                  if (i != 0)
                    MorrisBarChartDataElem(hour_data :+ ("time", "%d 小時前".format(i)))
                  else
                    MorrisBarChartDataElem(hour_data :+ ("time", "即時"))

                }

              val xkey = "time"
              val ykeys = param.monitors.map(_.toString)
              val labels = param.monitors.map(Monitor.map(_).name)
              val mt = MonitorType.map(param.monitorTypes(0))
              val title = mt.desp + "(" + mt.unit + ")趨勢圖"

              Json.toJson(MorrisBarChart(data, xkey, ykeys, labels, title))
            }
          Ok(json)
        })
  }

  case class XAxis(categories: Option[Seq[String]])
  case class AxisLineLabel(align: String, text: String)
  case class AxisLine(color: String, width: Int, value: Float, label: Option[AxisLineLabel])
  case class AxisTitle(text: Option[String])
  case class YAxis(labels: Option[String], title: AxisTitle, plotLines: Option[Seq[AxisLine]])
  case class seqData(name: String, data: Seq[Float])
  case class HighchartData(chart: Map[String, String],
                           title: Map[String, String],
                           xAxis: XAxis,
                           yAxis: YAxis,
                           series: Seq[seqData])

  implicit val xaWrite = Json.writes[XAxis]
  implicit val axisLineLabelWrite = Json.writes[AxisLineLabel]
  implicit val axisLineWrite = Json.writes[AxisLine]
  implicit val axisTitleWrite = Json.writes[AxisTitle]
  implicit val yaWrite = Json.writes[YAxis]
  implicit val seqDataWrite = Json.writes[seqData]
  implicit val hcWrite = Json.writes[HighchartData]

  def highchartJson(monitorTypeStr: String) = Security.Authenticated {
    implicit request =>
      val mt = MonitorType.withName(monitorTypeStr)
      val mtCase = MonitorType.map(mt)

      val latestRecordTime = getLatestRecordTime(TableType.Min).get

      val realtimeValueMap = getRealtimeMonitorValueMap(mt, latestRecordTime)

      val series = for (m <- Monitor.mvList) yield {
        seqData(Monitor.map(m).name, Seq(realtimeValueMap.getOrElse(m, 0f)))
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
        XAxis(Some(Seq(latestRecordTime.toString("yyyy:MM:DD HH:mm")))),
        YAxis(None, AxisTitle(Some(mtCase.unit)), axisLines),
        series)

      Ok(Json.toJson(c))
  }
  
  case class MonitorInfo(id:String, status:Int, winDir:Float, winSpeed:Float)
  case class RealtimeMapInfo(info:Seq[MonitorInfo])
  
  implicit val monitorInfoWrite = Json.writes[MonitorInfo]
  implicit val mapInfoWrite = Json.writes[RealtimeMapInfo]
  
  def realtimeMap = Security.Authenticated {
    implicit request =>
    val current = getLatestRecordTime(TableType.SixSec).get 
    val map = getRealtimeWeatherMap(current)
    
    def getStatusIndex(s:String)={
      if(MonitorStatus.isNormalStat(s))
        0
      else if(MonitorStatus.isCalbration(s))
        1
      else if(MonitorStatus.isRepairing(s))
        2
      else if(MonitorStatus.isMaintance(s))
        3
      else
        4
    }
    val mapInfos =
    for{m <- Monitor.mvList
      s = map.getOrElse(m, Record.emptySixSecRecord(m, current, MonitorStatus.DATA_LOSS_STAT))
      }
      yield
    {
      MonitorInfo(m.toString(), getStatusIndex(s.status), s.winDir, s.winSpeed)
    }
    
    Ok(Json.toJson(RealtimeMapInfo(mapInfos)))
  }
}