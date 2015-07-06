package models
import scala.collection.Map
import java.sql.Date
import play.api.Logger
import scalikejdbc._
import scalikejdbc.config._
import EnumUtils._
import play.api.libs.json._
import play.api.libs.functional.syntax._


case class Monitor(id:String, name:String, lat:Double, lng:Double, autoAudit:AutoAudit)
case class MonitorWithImageUrl(id:String, name:String, url:String)
object Monitor extends Enumeration{
  implicit val mReads: Reads[Monitor.Value] = EnumUtils.enumReads(Monitor)
  implicit val mWrites: Writes[Monitor.Value] = EnumUtils.enumWrites

  val monitorList:List[Monitor] =
    DB readOnly{ implicit session =>
      sql"""
        Select * 
        From Monitor
        """.map { r => 
          val autoAuditJson = r.stringOpt(9).getOrElse(Json.toJson(AutoAudit.default).toString())
          val autoAudit = Json.parse(autoAuditJson).validate[AutoAudit].get
          Monitor(r.string(1), r.string(2), r.string(6).toDouble, r.string(7).toDouble, autoAudit)}.list.apply
    }
    
  var map:Map[Value, Monitor] = Map(monitorList.map{e=>Value(e.id)->e}:_*)

  val mvList = monitorList.map{m=>Monitor.withName(m.id)}
  
  def getMvList(groupID:Int):List[Monitor.Value]={
    val groupOpt = Group.getGroup(groupID)
    if(groupOpt.isEmpty){
      List()
    }else{
      val group = groupOpt.get
      group.privilege.allowedMonitors.toList
    }
  }
  
  def getDisplayName(monitor:Value):String ={
    map.get(monitor) match{
      case Some(m) => m.name 
      case None => 
        Logger.error("Unknown monitor :" + monitor)
        ""
    }
  }

  def getImageMonitorList(groupID:Int) = {
    DB readOnly{ implicit session =>
      val monitorList =
      sql"""
        Select * 
        From Monitor
        """.map { r => MonitorWithImageUrl(r.string(1), r.string(2), r.string("imageUrl"))}.list.apply
        
      val allowedList = getMvList(groupID)
      monitorList.filter { m =>
        val mv = Monitor.withName(m.id)
        !m.url.isEmpty() && allowedList.contains(mv) 
        }
    }
  }
  
  def main(args: Array[String]) {
    for(p<-MonitorType.values.toList){
        println(p + ":" + p.id + ":" + MonitorType.map.get(p).get)
    }
      
  }
}

case class MonitorType(id:String, desp:String, unit:String, 
    std_internal:Option[Float], std_law:Option[Float], std_hour:Option[Float],
    std_day:Option[Float], std_year:Option[Float], 
    zd_internal:Option[Float], zd_law:Option[Float],
    sd_internal:Option[Float], sd_law:Option[Float])
    
object MonitorType extends Enumeration{
  implicit val mtReads: Reads[MonitorType.Value] = EnumUtils.enumReads(MonitorType)
  implicit val mtWrites: Writes[MonitorType.Value] = EnumUtils.enumWrites
  
  val mtList:List[MonitorType] =
    DB readOnly{ implicit session =>
      sql"""
        Select *
        From MonitorType
      """.map { r =>  MonitorType(id = r.string(1), 
          desp = r.string(2),
          unit = r.string(3),
          std_internal = r.floatOpt(5),
          std_law = r.floatOpt(6), 
          std_hour = r.floatOpt(7),
          std_day = r.floatOpt(8), 
          std_year = r.floatOpt(9),
          zd_internal = r.floatOpt(10),
          zd_law = r.floatOpt(11),
          sd_internal = r.floatOpt(12),
          sd_law = r.floatOpt(13)
          )}.list.apply
    }
  
  val map:Map[Value, MonitorType] = Map(mtList.map{e=>Value(e.id)->e}:_*) - MonitorType.withName("A325")
  val mtvList = mtList.map(mt=>MonitorType.withName(mt.id)).filter { !List(MonitorType.withName("A325"), MonitorType.withName("C911"), MonitorType.withName("C912")).contains(_) }
  val realtimeList = mtvList.filter { !List(MonitorType.withName("C911"), MonitorType.withName("C912")).contains(_)} 
  val psiList = List(MonitorType.withName("A214"),MonitorType.withName("A222"), MonitorType.withName("A224"), MonitorType.withName("A225"), MonitorType.withName("A293") )
  val windDirList = List(MonitorType.withName("C212"), MonitorType.withName("C912")) 
}