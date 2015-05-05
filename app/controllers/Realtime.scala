package controllers
import play.api._
import play.api.mvc._
import play.api.Logger
import models._
import models.Realtime._
import com.github.nscala_time.time.Imports._

object Realtime extends Controller {
  def trendReport = Security.Authenticated {
    Ok(views.html.trendReport())
  }

  def realtimeStat = Security.Authenticated {
    implicit request =>
      val rt_status = getRealtimeStatusHour()
      val rt_psi = getRealtimePSI
      Ok(views.html.realtimeStatus(rt_status, MonitorType.psiList, rt_psi))
  }

  def realtimeImg = Security.Authenticated {
    implicit request =>
      Ok(views.html.realtimeImage(""))
  }

  import play.api.libs.json._
  import play.api.libs.functional.syntax._

  case class WeatherStat(monitor: String, windDir: Float, windSpeed: Float)
  implicit val weatherStatWrites: Writes[WeatherStat] = (
    (JsPath \ "name").write[String] and
    (JsPath \ "windDir").write[Float] and
    (JsPath \ "windSpeed").write[Float])(unlift(WeatherStat.unapply))

  def realtimeMap = Security.Authenticated {
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

  case class MorrisBarChart(data:Seq[MorrisBarChartDataElem], xkey:String, ykeys:Seq[String], labels:Seq[String], title:String)
  case class MorrisBarChartDataElem(elem:Seq[(String, String)])
  implicit val morrisBarChartDataElemWritter = new Writes[MorrisBarChartDataElem] {
    def writes(dataElem: MorrisBarChartDataElem): JsObject = {
     val ret1: Seq[(String, Json.JsValueWrapper)] = dataElem.elem.map(r => {
        val wrapper: Json.JsValueWrapper = r._2
        (r._1, wrapper)
      })
      Json.obj(ret1: _*)
    }
  }
  implicit val morrisBarChartWritter : Writes[MorrisBarChart] = (
    (__ \ "data").write[Seq[MorrisBarChartDataElem]] and
    (__ \ "xkey").write[String] and
    (__ \ "ykeys").write[Seq[String]] and
    (__ \ "labels").write[Seq[String]] and
    (__ \ "title").write[String]
  )(unlift(MorrisBarChart.unapply))

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
              val trend =
                realtimeMonitorTrend(param.monitors, param.monitorTypes(0))
                
              val data = 
                for { i <- 8 to 0 by -1 } yield {
                  val hour_data =
                    for {
                      m <- param.monitors
                      record = trend.get(m).get(i)
                    } yield {
                       val value =
                         if(record._1.isDefined)
                           "%.2f".format(record._1.get)
                         else
                           "0"
                           
                      (m.toString() -> value)
                    }
                  if(i != 0)
                    MorrisBarChartDataElem(hour_data :+("time", "%d 小時前".format(i)))
                  else
                    MorrisBarChartDataElem(hour_data :+("time", "即時"))
                }
                
              Logger.debug(data.toString)

              val xkey = "time"
              val ykeys = param.monitors.map(_.toString)
              val labels = param.monitors.map(Monitor.map(_).name)
              val mt = MonitorType.map(param.monitorTypes(0))
              val title = mt.desp + "("+mt.unit+")趨勢圖"
                
              Json.toJson(MorrisBarChart(data, xkey, ykeys, labels, title))
            }
          Ok(json)
        })
  }
}