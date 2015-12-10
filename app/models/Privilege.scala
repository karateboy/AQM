package models
import play.api._
import play.api.mvc._
import play.api.Logger
import models.ModelHelper._
import play.api.libs.json._
import play.api.libs.functional.syntax._
import scala.language.implicitConversions

object MenuRight extends Enumeration{
  implicit val mReads: Reads[MenuRight.Value] = EnumUtils.enumReads(MenuRight)
  implicit val mWrites: Writes[MenuRight.Value] = EnumUtils.enumWrites

  val RealtimeInfo = Value("Realtime Info")
  val DataQuery = Value("Data Query")
  val Report = Value("Report")
  val Statistics = Value("Statistics")
  val SystemManagement = Value("System management")
  val RepartMaintance = Value
  
  val map = Map(
        RealtimeInfo->"即時資訊",
        DataQuery->"數據查詢",
        Report->"報表查詢",
        Statistics->"統計分析",
        SystemManagement->"系統管理",
        RepartMaintance->"維修保養"
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
  
object Privilege {    
  implicit val privilegeWrite = Json.writes[Privilege]
  implicit val privilegeRead = Json.reads[Privilege]
  
  val defaultPrivilege = Privilege(Monitor.values.toSeq, MonitorType.values.toSeq, MenuRight.values.toSeq)
}