package models
import scala.collection.Map
import java.sql.Date
import play.api.Logger
import scalikejdbc._
import scalikejdbc.config._
import EnumUtils._
import play.api.libs.json._
import play.api.libs.functional.syntax._


case class Monitor(id:String, name:String, lat:Double, lng:Double, url:String, autoAudit:AutoAudit, 
    monitorTypes: Seq[MonitorType.Value], monitorTypeStds:Seq[MonitorTypeStandard])
case class MonitorTypeStandard(id:MonitorType.Value, std_internal:Float)
object Monitor extends Enumeration{
  implicit val mReads: Reads[Monitor.Value] = EnumUtils.enumReads(Monitor)
  implicit val mWrites: Writes[Monitor.Value] = EnumUtils.enumWrites
  implicit val mtStdRead = Json.reads[MonitorTypeStandard]
  implicit val mtStdWrite = Json.writes[MonitorTypeStandard]
  
  lazy val monitorList:List[Monitor] =
    DB readOnly{ implicit session =>
      sql"""
        Select * 
        From Monitor
        """.map { r => 
          val autoAuditJson = r.stringOpt(9).getOrElse(Json.toJson(AutoAudit.default).toString())
          val autoAudit = Json.parse(autoAuditJson).validate[AutoAudit].get
          val monitorTypesJson = r.stringOpt(10).getOrElse(Json.toJson(Seq[MonitorType.Value]()).toString())
          val monitorTypes = Json.parse(monitorTypesJson).validate[Seq[MonitorType.Value]].get
          val monitorTypeStdJson = r.stringOpt(11).getOrElse(Json.toJson(Seq[MonitorTypeStandard]()).toString())
          val monitorTypeStd = Json.parse(monitorTypeStdJson).validate[Seq[MonitorTypeStandard]].get
          
          Monitor(r.string(1), r.string(2), r.string(6).toDouble, r.string(7).toDouble, r.string("imageUrl"), 
              autoAudit, monitorTypes, monitorTypeStd)}.list.apply
    }
    
  var map:Map[Value, Monitor] = Map(monitorList.map{e=>Value(e.id)->e}:_*)

  lazy val mvList = monitorList.map{m=>Monitor.withName(m.id)}
   
  def myMvList(p:Privilege) = {
    mvList.filter { p.allowedMonitors.contains }
  }
  
  def instrumentMvList(p:Privilege) = {
    List(Monitor.withName("A012")).filter { p.allowedMonitors.contains }
  }
  
  def getDisplayName(m:Monitor.Value)={
    map(m).name
  }
  
  def updateMonitorTypes(m:Monitor.Value, mt:Seq[MonitorType.Value])={
    val oldM = map(m)
    val newM = Monitor(oldM.id, oldM.name, oldM.lat, oldM.lng, oldM.url, oldM.autoAudit, mt, oldM.monitorTypeStds)
    updateMonitor(newM)
    map = map + (m -> newM)
  }
  
  def updateMonitorAutoAudit(m:Monitor.Value, autoAudit:AutoAudit)={
    val oldM = map(m)
    val newM = Monitor(oldM.id, oldM.name, oldM.lat, oldM.lng, oldM.url, autoAudit, oldM.monitorTypes, oldM.monitorTypeStds)
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
  
  def updateImgUrl(m:Monitor.Value, url:String)(implicit session: DBSession = AutoSession) = {
    val oldM = map(m)
    val newM = Monitor(oldM.id, oldM.name, oldM.lat, oldM.lng, url, oldM.autoAudit, oldM.monitorTypes, oldM.monitorTypeStds)
    sql"""
        Update Monitor
        Set imageUrl=${url}
        Where DP_NO=${oldM.id}  
        """.update.apply
    val newMap = map + (m->newM)
    map = newMap
  }
}

case class MonitorType(id:String, desp:String, unit:String, 
    std_internal:Option[Float], std_law:Option[Float], std_hour:Option[Float],
    std_day:Option[Float], std_year:Option[Float], 
    zd_internal:Option[Float], zd_law:Option[Float],
    sd_internal:Option[Float], sd_law:Option[Float],
    epa_mapping:Option[String],
    prec:Int)
    
object MonitorType extends Enumeration{
  implicit val mtReads: Reads[MonitorType.Value] = EnumUtils.enumReads(MonitorType)
  implicit val mtWrites: Writes[MonitorType.Value] = EnumUtils.enumWrites
  
  private def mtList:List[MonitorType] =
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
          sd_law = r.floatOpt(13),
          epa_mapping = r.stringOpt(14),
          prec = r.int(15)
          )}.list.apply
    }
  
  var map:Map[Value, MonitorType] = Map(mtList.map{e=>Value(e.id)->e}:_*) - MonitorType.withName("A325")
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

  def updateMonitorType(mt: MonitorType.Value, colname: String, newValue: String) = {
    DB localTx { implicit session =>
      val col = SQLSyntax.createUnsafely(s"${colname}")
      sql"""
        Update MonitorType
        Set ${col}=${newValue}
        Where ITEM=${mt.toString}  
        """.update.apply

      val old = map(mt)

      val newMtOpt =
        sql"""
          Select *
          From MonitorType
          Where ITEM=${mt.toString}
        """.map { r =>
          MonitorType(id = r.string(1),
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
            sd_law = r.floatOpt(13),
            epa_mapping = r.stringOpt(14),
            prec = r.int(15))
        }.single.apply
        map = (map + (mt-> newMtOpt.get))
    }
  }
  
  val psiList = List(MonitorType.withName("A214"),MonitorType.withName("A222"), MonitorType.withName("A224"), MonitorType.withName("A225"), MonitorType.withName("A293") )
  val windDirList = List(MonitorType.withName("C212"), MonitorType.withName("C912"))
  val monitorReportList = {
    val name=List("A222", "A223", "A293", "A283", "A224", "A225", "A226", "A286", "A296", "A213", "A214", 
        "C211", "C212", "C214", "C215", "C216", "C213")
    name.map { MonitorType.withName }
  }
  
  val epaList = {
    val name=List("A214", "A215", "A222", "A223", "A224", "A225", "A226", "A283", "A286", "A293", "A296", "C211", "C212", "C213", "C214", "C215")
    name.map { MonitorType.withName }
  }
  
  val epaReportList ={
    val name=List("C212", "A222", "A293", "A224", "A225", "A214", "A226", "A296")
    name.map { MonitorType.withName }
  }
  
  val epaMap={
    map.filter(p=>p._2.epa_mapping.isDefined).map(kv=>(kv._2.epa_mapping.get, kv._1))
  }
}