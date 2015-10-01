package models
import scalikejdbc._
import play.api._
import com.github.nscala_time.time.Imports._
import ModelHelper._
import models._
import play.api.libs.json._
import play.api.libs.functional.syntax._
import play.api.libs.json.Json

case class AbnormalEntry(monitor:Monitor.Value, monitorType:MonitorType.Value, invalidHours:String, explain:String)
case class AbnormalReport(date:DateTime, report:Seq[AbnormalEntry])
object AbnormalReport {
  
}