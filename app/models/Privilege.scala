package models
import play.api._
import play.api.mvc._
import play.api.Logger
import models.ModelHelper._
import play.api.libs.json._
import play.api.libs.functional.syntax._
import scala.language.implicitConversions

object MenuRight extends Enumeration{
  val RealtimeInfo = Value("Realtime Info")
  val DataQuery = Value("Data Query")
  val Report = Value("Report")
  val Statistics = Value("Statistics")
  val SystemManagement = Value("System management")
  
  val map = Map(
        RealtimeInfo->"即時資訊",
        DataQuery->"數據查詢",
        Report->"報表查詢",
        Statistics->"統計分析",
        SystemManagement->"系統管理"
      )
  def getDisplayName(v:MenuRight.Value)={
    map(v)
  }
}

case class Privilege(
    allowedMonitors:Seq[Monitor.Value],
    allowedMonitorTypes:Seq[MonitorType.Value],
    allowedMenuRights:Seq[MenuRight.Value]
  )

case class PrivilegeJson(
  allowedMonitors:Seq[String],
  allowedMonitorTypes:Seq[String],
  allowedMenuRights:Seq[String]
  )
  
object Privilege {    
  implicit val privilegeWrite = Json.writes[PrivilegeJson]
  implicit val privilegeRead = Json.reads[PrivilegeJson]
  
  implicit def privilegeJsonConvert(jsonObj:PrivilegeJson)={
    val allowedMonitors = jsonObj.allowedMonitors.map { Monitor.withName }
    val allowedMonitorTypes = jsonObj.allowedMonitorTypes.map { MonitorType.withName }
    val allowedMenuRights = jsonObj.allowedMenuRights.map { MenuRight.withName }
    Privilege(allowedMonitors, allowedMonitorTypes, allowedMenuRights)
  } 
  implicit def privilegeConvert(pObj:Privilege)={
    val allowedMonitors = pObj.allowedMonitors.map {_.toString}
    val allowedMonitorTypes = pObj.allowedMonitorTypes.map { _.toString }
    val allowedMenuRights = pObj.allowedMenuRights.map { _.toString }
    PrivilegeJson(allowedMonitors, allowedMonitorTypes, allowedMenuRights)
  }
  
  val defaultPrivilege = Privilege(Monitor.values.toSeq, MonitorType.values.toSeq, MenuRight.values.toSeq)
}