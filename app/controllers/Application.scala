package controllers

import play.api._
import play.api.mvc._
import play.api.Logger
import models._
import models.Realtime._
import com.github.nscala_time.time.Imports._
import models.ModelHelper._
import play.api.libs.json._
import play.api.libs.json.Json


object Application extends Controller {

  val title = "麥寮廠區空氣品質及氣象監測系統"
  
  def index = Security.Authenticated{
    implicit request =>
    Ok(views.html.index(title))
  }
  
  def monitor(monitor:String) = Security.Authenticated{
    implicit request =>
      Logger.debug("monitor=>"+monitor)
      val monitorValue = Monitor.withName(monitor)
    Ok(views.html.monitor(monitor))
  }
  
  def monitoredTypes(monitorStr:String) = Security.Authenticated{
    implicit request =>
      val monitor = Monitor.withName(monitorStr)
      val monitoredTypes = MonitoredType.getMonitoredTypes(monitor)
    Ok(views.html.monitoredTypes(monitor.toString(), monitoredTypes))
  }
  
  def setMonitoredTypes(monitorStr:String, monitoredTypeStr:String, used:Boolean) = Security.Authenticated{
    implicit request =>
      val monitor = Monitor.withName(monitorStr)
      val monitorType = MonitorType.withName(monitoredTypeStr)
      MonitoredType.setMonitoredType(monitor, monitorType, used)
      Ok("")
  }
  
  
  def monitorTypeConfig = Security.Authenticated{
    implicit request =>
    Ok(views.html.monitorTypeConfig())
  } 
  
  def realtimeStat = Security.Authenticated{
    implicit request =>
      val rt_status = Realtime.getRealtimeStatusHour()
      val rt_psi = Realtime.getRealtimePSI
      Ok(views.html.realtimeStatus(rt_status, MonitorType.psiList, rt_psi))
  } 
  
  def realtimeImg = Security.Authenticated{
    implicit request =>
    Ok(views.html.realtimeImage(""))
  } 
  
import play.api.libs.json._
import play.api.libs.functional.syntax._

  case class WeatherStat(monitor:String, windDir:Float, windSpeed:Float)
  implicit val weatherStatWrites: Writes[WeatherStat] = (
  (JsPath \ "name").write[String] and
  (JsPath \ "windDir").write[Float] and
  (JsPath \ "windSpeed").write[Float]
)(unlift(WeatherStat.unapply))

  def realtimeWeather = Security.Authenticated{
    implicit request =>
    Ok(views.html.realtimeImage(""))
  } 

  def realtimeTrend = Security.Authenticated{
    implicit request =>
    Ok(views.html.realtimeTrend(""))
  } 

  case class RealtimeTrendParam(monitors:Seq[Monitor.Value], monitorTypes:Seq[MonitorType.Value])
  
 
  implicit val monitorTypeReader : Reads[MonitorType.Value] = (__ \ "mt").read[String].map(MonitorType.withName _)
  implicit val monitorReader : Reads[Monitor.Value] = (__ \ "m").read[String].map(Monitor.withName _)
  implicit val realtimeTrendParamRBuilder: Reads[RealtimeTrendParam] = 
    ((__ \ "monitors").read[Seq[Monitor.Value]] and 
     (__ \ "monitorTypes").read[Seq[MonitorType.Value]])(RealtimeTrendParam.apply _)

  case class MorrisDataElem(y:String, x:Seq[Option[Float]])
  implicit val morrisDataElemWritter =new Writes[MorrisDataElem]{
    def writes(elem:MorrisDataElem):JsObject = {
      val y = "y"->JsString(elem.y)
      val x = 
      for{i <- 0 to elem.x.length-1
          tag = ('a' + i).toString()
        }
        yield {
          if(elem.x(i).isDefined)
            (tag -> JsNumber(elem.x(i).get))
          else
            (tag -> JsNumber(0f))
        }
      val result:Seq[(String, JsValue)] = y :: List(x :_*)
      val ret1:Seq[(String, Json.JsValueWrapper)] = result.map(r=>{
         val wrapper:Json.JsValueWrapper = r._2
         (r._1, wrapper)
      })
      Json.obj(ret1 : _*) 
    }
  }
  
  def realtimeTrendJSON = Security.Authenticated(BodyParsers.parse.json) {
    implicit request =>
      val realtimeTrendParam = request.body.validate[RealtimeTrendParam]

      realtimeTrendParam.fold(
        error => {
          Logger.error(JsError.toFlatJson(error).toString())
          BadRequest(Json.obj("ok" -> false, "msg" -> JsError.toFlatJson(error)))
        },
        param => {
          assert(param.monitorTypes.length == 1 || param.monitors.length == 1)
          val json =
          if(param.monitorTypes.length == 1){
            Json.toJson("String")
          }else{
            val trend =
            realtimeMonitorTrend(param.monitors, param.monitorTypes(0))
            Json.toJson("Todo")
          }
          Ok(json)
        })
  }
  

}
