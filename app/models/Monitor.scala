package models
import scala.collection.Map
import java.sql.Date
import play.api.Logger
import scalikejdbc._
import scalikejdbc.config._
import EnumUtils._
import play.api.libs.json._
import play.api.libs.functional.syntax._


case class Monitor(id:String, name:String, lat:Double, lng:Double, url:String, autoAudit:AutoAudit, monitorTypes: Seq[MonitorType.Value])
object Monitor extends Enumeration{
  implicit val mReads: Reads[Monitor.Value] = EnumUtils.enumReads(Monitor)
  implicit val mWrites: Writes[Monitor.Value] = EnumUtils.enumWrites

  lazy val monitorList:List[Monitor] =
    DB readOnly{ implicit session =>
      sql"""
        Select * 
        From Monitor
        """.map { r => 
          val autoAuditJson = r.stringOpt(9).getOrElse(Json.toJson(AutoAudit.default).toString())
          val autoAudit = Json.parse(autoAuditJson).validate[AutoAudit].get
          val monitorTypesJson = r.stringOpt(10).getOrElse(Json.toJson(Seq()).toString())
          val monitorTypes = Json.parse(monitorTypesJson).validate[Seq[MonitorType.Value]].get
          Monitor(r.string(1), r.string(2), r.string(6).toDouble, r.string(7).toDouble, r.string("imageUrl"), 
              autoAudit, monitorTypes)}.list.apply
    }
    
  var map:Map[Value, Monitor] = Map(monitorList.map{e=>Value(e.id)->e}:_*)

  lazy val mvList = monitorList.map{m=>Monitor.withName(m.id)}
    
  def getDisplayName(m:Monitor.Value)={
    map(m).name
  }
  
  def updateMonitorTypes(m:Monitor.Value, mt:Seq[MonitorType.Value])={
    val oldM = map(m)
    val newM = Monitor(oldM.id, oldM.name, oldM.lat, oldM.lng, oldM.url, oldM.autoAudit, mt)
    updateMonitor(newM)
    map = map + (m -> newM)
  }
  
  def updateMonitor(m:Monitor)(implicit session: DBSession = AutoSession) = {
    sql"""
        Update Monitor
        Set autoAudit=${Json.toJson(m.autoAudit).toString}, monitorTypes=${Json.toJson(m.monitorTypes).toString}
        Where DP_NO=${m.id}  
        """.update.apply
    
    val newMap = map + (Monitor.withName(m.id)->m)
    map = newMap
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
  val mtvAllList = mtList.map(mt=>MonitorType.withName(mt.id)).filter { !List(MonitorType.withName("A325"), MonitorType.withName("C911"), MonitorType.withName("C912")).contains(_) }
  
  def mtvList = {
    var mtSet = Set.empty[MonitorType.Value]
    for(m<-Monitor.mvList){
      mtSet = mtSet.union(Monitor.map(m).monitorTypes.toSet)
    }
    
    mtvAllList.filter {mtSet.contains}
  }
  
  def myMtvList(implicit p:Privilege)={
    mtvList.filter { p.allowedMonitorTypes.contains }
  }
  
  def realtimeList = {
    var mtSet = Set.empty[MonitorType.Value]
    for(m<-Monitor.mvList){
      mtSet = mtSet.union(Monitor.map(m).monitorTypes.toSet)
    }
        
     mtSet.filter { !List(MonitorType.withName("C911"), MonitorType.withName("C912")).contains(_)}.toList 
  } 
  
  val psiList = List(MonitorType.withName("A214"),MonitorType.withName("A222"), MonitorType.withName("A224"), MonitorType.withName("A225"), MonitorType.withName("A293") )
  val windDirList = List(MonitorType.withName("C212"), MonitorType.withName("C912")) 
}