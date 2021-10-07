package models

import models.EnumUtils._
import play.api.libs.json._
import scalikejdbc._

import scala.collection.Map

case class Monitor(id: String, name: String, lat: Double, lng: Double, url: String, autoAudit: AutoAudit,
                   monitorTypes: Seq[MonitorType.Value], monitorTypeStds: Seq[MonitorTypeStandard],
                   calibrationZds: Seq[MonitorTypeStandard],
                   calibrationSds: Seq[MonitorTypeStandard]) {
  private val stdMap = Map(monitorTypeStds.map { r => r.id -> r.std_internal }: _*)

  @deprecated("use MonitorTypeAlert instead", "2021-10-8") def getStdInternal(mt: MonitorType.Value) = {
    val monitorStd = stdMap.get(mt)
    if (monitorStd.isDefined)
      monitorStd
    else
      MonitorType.map(mt).std_internal_default
  }

  def getNewStd(mt: MonitorType.Value, v: Float) = {
    if (stdMap.get(mt).isEmpty)
      MonitorTypeStandard(mt, v) :: monitorTypeStds.toList
    else {
      monitorTypeStds.map { s =>
        if (s.id != mt)
          s
        else
          MonitorTypeStandard(s.id, v)
      }
    }
  }

  def zdInternal(mt: MonitorType.Value) = {
    val pair = calibrationZds map { std => std.id -> Some(std.std_internal) }
    val zdMap = pair.toMap

    zdMap.getOrElse(mt, {
      val mtCase = MonitorType.map(mt)
      mtCase.zd_internal
    })
  }

  def sdInternal(mt: MonitorType.Value) = {
    val pair = calibrationSds map { std => std.id -> Some(std.std_internal) }
    val sdMap = pair.toMap

    sdMap.getOrElse(mt, {
      val mtCase = MonitorType.map(mt)
      mtCase.sd_internal
    })
  }
}

case class MonitorTypeStandard(id: MonitorType.Value, std_internal: Float)

object Monitor extends Enumeration {
  implicit val mReads: Reads[Monitor.Value] = EnumUtils.enumReads(Monitor)
  implicit val mWrites: Writes[Monitor.Value] = EnumUtils.enumWrites
  implicit val mtStdRead = Json.reads[MonitorTypeStandard]
  implicit val mtStdWrite = Json.writes[MonitorTypeStandard]
  implicit val reads = Json.reads[Monitor]
  implicit val writes = Json.writes[Monitor]

  def monitorList: List[Monitor] =
    DB readOnly { implicit session =>
      sql"""
        Select * 
        From Monitor
        """.map { r =>
        val autoAuditJson = r.stringOpt(9).getOrElse(Json.toJson(AutoAudit.default).toString())
        val autoAudit = Json.parse(autoAuditJson).validate[AutoAudit].get
        val monitorTypesJson = r.stringOpt(10).getOrElse(Json.toJson(Seq.empty[MonitorType.Value]).toString())
        val monitorTypes = Json.parse(monitorTypesJson).validate[Seq[MonitorType.Value]].get
        val monitorTypeStdJson = r.stringOpt(11).getOrElse(Json.toJson(Seq.empty[MonitorTypeStandard]).toString())
        val monitorTypeStd = Json.parse(monitorTypeStdJson).validate[Seq[MonitorTypeStandard]].get
        val calibrationZdJson = r.stringOpt(12).getOrElse(Json.toJson(Seq.empty[MonitorTypeStandard]).toString())
        val calibrationZd = Json.parse(calibrationZdJson).validate[Seq[MonitorTypeStandard]].get
        val calibrationSdJson = r.stringOpt(13).getOrElse(Json.toJson(Seq.empty[MonitorTypeStandard]).toString())
        val calibrationSd = Json.parse(calibrationSdJson).validate[Seq[MonitorTypeStandard]].get

        Monitor(r.string(1), r.string(2), r.string(6).toDouble, r.string(7).toDouble, r.string("imageUrl"),
          autoAudit, monitorTypes, monitorTypeStd, calibrationZd, calibrationSd)
      }.list.apply
    }
  lazy val mvList = monitorList.map { m => Monitor.withName(m.id) }
  var map: Map[Value, Monitor] = Map(monitorList.map { e => Value(e.id) -> e }: _*)

  def myMvList(p: Privilege) = {
    mvList.filter {
      p.allowedMonitors.contains
    }
  }

  def instrumentMvList(p: Privilege) = {
    List(Monitor.withName("A012")).filter {
      p.allowedMonitors.contains
    }
  }

  def getDisplayName(m: Monitor.Value) = {
    map(m).name
  }

  def updateMonitorTypes(m: Monitor.Value, mt: Seq[MonitorType.Value]) = {
    val oldM = map(m)
    val newM = Monitor(oldM.id, oldM.name, oldM.lat, oldM.lng, oldM.url, oldM.autoAudit, mt,
      oldM.monitorTypeStds, oldM.calibrationZds, oldM.calibrationSds)
    updateMonitor(newM)
    map = map + (m -> newM)
  }

  def updateMonitorAutoAudit(m: Monitor.Value, autoAudit: AutoAudit) = {
    val oldM = map(m)
    val newM = Monitor(oldM.id, oldM.name, oldM.lat, oldM.lng, oldM.url, autoAudit, oldM.monitorTypes,
      oldM.monitorTypeStds, oldM.calibrationZds, oldM.calibrationSds)
    updateMonitor(newM)
    map = map + (m -> newM)
  }

  def updateMonitor(m: Monitor)(implicit session: DBSession = AutoSession) = {
    sql"""
        Update Monitor
        Set autoAudit=${Json.toJson(m.autoAudit).toString}, monitorTypes=${Json.toJson(m.monitorTypes).toString}
        Where DP_NO=${m.id}  
        """.update.apply

    val newMap = map + (Monitor.withName(m.id) -> m)
    map = newMap
  }

  def updateStdInternal(m: Monitor)(implicit session: DBSession = AutoSession) = {
    sql"""
        Update Monitor
        Set monitorTypeStd=${Json.toJson(m.monitorTypeStds).toString}
        Where DP_NO=${m.id}  
        """.update.apply

    val newMap = map + (Monitor.withName(m.id) -> m)
    map = newMap
  }

  def updateCalibrationInternalStd(m: Monitor)(implicit session: DBSession = AutoSession) = {
    sql"""
        Update Monitor
        Set calibrationZds=${Json.toJson(m.calibrationZds).toString}, 
          calibrationSds=${Json.toJson(m.calibrationSds).toString}
        Where DP_NO=${m.id}  
        """.update.apply

    val newMap = map + (Monitor.withName(m.id) -> m)
    map = newMap
  }

  def updateImgUrl(m: Monitor.Value, url: String)(implicit session: DBSession = AutoSession) = {
    val oldM = map(m)
    val newM = Monitor(oldM.id, oldM.name, oldM.lat, oldM.lng, url, oldM.autoAudit, oldM.monitorTypes,
      oldM.monitorTypeStds, oldM.calibrationZds, oldM.calibrationSds)
    sql"""
        Update Monitor
        Set imageUrl=${url}
        Where DP_NO=${oldM.id}  
        """.update.apply
    val newMap = map + (m -> newM)
    map = newMap
  }
}