package models
import play.api._
import play.api.mvc._
import play.api.Logger
import models.ModelHelper._
import play.api.libs.json._
import play.api.libs.functional.syntax._

object MenuRight extends Enumeration{
  val RealtimeInfo = Value("Realtime Info")
  val DataQuery = Value("Data Query")
  val Report = Value("Report")
  val Statistics = Value("Statistics")
  val SystemManagement = Value("System management")
}

case class PrivilegeJson(
  allowedMonitors:Seq[String],
  allowedMonitorTypes:Seq[String],
  allowedMenuRight:Seq[String]
  )
  
object Privilege {    
  implicit val privilegeWrite = Json.writes[PrivilegeJson]
  
}