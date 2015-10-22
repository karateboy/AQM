package models
import scalikejdbc._
import play.api._
import com.github.nscala_time.time.Imports._
import models.ModelHelper._
import models._
import play.api.libs.json._
import play.api.libs.functional.syntax._
import play.api.libs.json.Json

case class AlarmConfig(enable:Boolean, monitorFilter:Seq[Monitor.Value], statusFilter:Seq[String])
object AlarmConfig {
  val defaultConfig = AlarmConfig(false, Seq.empty[Monitor.Value], Seq.empty[String])
  implicit val acRead = Json.reads[AlarmConfig]
  implicit val acWrite = Json.writes[AlarmConfig]
  
}