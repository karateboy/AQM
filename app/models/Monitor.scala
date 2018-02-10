package models
import scala.collection.Map
import java.sql.Date
import play.api.Logger
import scalikejdbc._
import scalikejdbc.config._
import EnumUtils._
import play.api.libs.json._
import play.api.libs.functional.syntax._
import models.ModelHelper._

case class Monitor(id: String, name: String, lat: Double, lng: Double, url: String, autoAudit: AutoAudit,
                   monitorTypes: Seq[MonitorType.Value], monitorTypeStds: Seq[MonitorTypeStandard],
                   calibrationZds: Seq[MonitorTypeStandard],
                   calibrationSds: Seq[MonitorTypeStandard]) {
  private val stdMap = Map(monitorTypeStds.map { r => r.id -> r.std_internal }: _*)
  def getStdInternal(mt: MonitorType.Value) = {
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

  lazy val monitorList: List[Monitor] =
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

  var map: Map[Value, Monitor] = Map(monitorList.map { e => Value(e.id) -> e }: _*)

  lazy val mvList = monitorList.map { m => Monitor.withName(m.id) }

  def myMvList(p: Privilege) = {
    mvList.filter { p.allowedMonitors.contains }
  }

  def instrumentMvList(p: Privilege) = {
    List(Monitor.withName("A012")).filter { p.allowedMonitors.contains }
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

case class MonitorType(id: String, desp: String, unit: String,
                       std_internal_default: Option[Float], std_law: Option[Float], std_hour: Option[Float],
                       std_day: Option[Float], std_year: Option[Float],
                       zd_internal: Option[Float], zd_law: Option[Float],
                       sd_internal: Option[Float], sd_law: Option[Float],
                       epa_mapping: Option[String],
                       prec: Int, order: Int)

object MonitorType extends Enumeration {
  implicit val mtReads: Reads[MonitorType.Value] = EnumUtils.enumReads(MonitorType)
  implicit val mtWrites: Writes[MonitorType.Value] = EnumUtils.enumWrites

  val Other = Value("Oth")
  val OtherCase = MonitorType("Oth", "其他", "", None, None, None, None, None,
    None, None, None, None, None, 0, 0)

  private def mtList: List[MonitorType] =
    DB readOnly { implicit session =>
      sql"""
        Select *
        From MonitorType
      """.map { r =>
        MonitorType(id = r.string(1),
          desp = r.string(2),
          unit = r.string(3),
          std_internal_default = r.floatOpt(5),
          std_law = r.floatOpt(6),
          std_hour = r.floatOpt(7),
          std_day = r.floatOpt(8),
          std_year = r.floatOpt(9),
          zd_internal = r.floatOpt(10),
          zd_law = r.floatOpt(11),
          sd_internal = r.floatOpt(12),
          sd_law = r.floatOpt(13),
          epa_mapping = r.stringOpt(14),
          prec = r.int(15),
          order = r.int(16))
      }.list.apply
    }

  var map: Map[Value, MonitorType] = Map(mtList.map { e => Value(e.id) -> e }: _*) ++ Map(Other -> OtherCase) - MonitorType.withName("A325")
  val mtvAllList = mtList.map(mt => MonitorType.withName(mt.id)).filter { !List(MonitorType.withName("A325")).contains(_) }

  def mtvList = {
    var mtSet = Set.empty[MonitorType.Value]
    for (m <- Monitor.mvList) {
      mtSet = mtSet.union(Monitor.map(m).monitorTypes.toSet)
    }

    mtvAllList.filter { mtSet.contains }.sortBy { map(_).order }
  }

  def myMtvList(implicit p: Privilege) = {
    mtvList.filter { p.allowedMonitorTypes.contains }.sortBy { map(_).order }
  }

  def maintainMonitorTypeList = (Other :: mtvList.reverse).reverse

  def realtimeList = {
    var mtSet = Set.empty[MonitorType.Value]
    for (m <- Monitor.mvList) {
      mtSet = mtSet.union(Monitor.map(m).monitorTypes.toSet)
    }

    mtSet.toList.sortBy { map(_).order }
  }

  def updateMonitorType(mt: MonitorType.Value, colname: String, newValue: String) = {
    DB localTx { implicit session =>
      val updateValue =
        if (newValue == "-")
          None
        else {
          import java.lang.Float
          val v = Float.parseFloat(newValue)
          Some(v)
        }

      val col = SQLSyntax.createUnsafely(s"${colname}")
      sql"""
        Update MonitorType
        Set ${col}=${updateValue}
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
            std_internal_default = r.floatOpt(5),
            std_law = r.floatOpt(6),
            std_hour = r.floatOpt(7),
            std_day = r.floatOpt(8),
            std_year = r.floatOpt(9),
            zd_internal = r.floatOpt(10),
            zd_law = r.floatOpt(11),
            sd_internal = r.floatOpt(12),
            sd_law = r.floatOpt(13),
            epa_mapping = r.stringOpt(14),
            prec = r.int(15),
            order = r.int(16))
        }.single.apply
      map = (map + (mt -> newMtOpt.get))
    }
  }

  val A213 = MonitorType.withName("A213")
  val A214 = MonitorType.withName("A214")
  val A215 = MonitorType.withName("A215")
  val A221 = MonitorType.withName("A221")
  val A222 = MonitorType.withName("A222")
  val A223 = MonitorType.withName("A223")
  val A224 = MonitorType.withName("A224")
  val A225 = MonitorType.withName("A225")
  val A226 = MonitorType.withName("A226")
  val A229 = MonitorType.withName("A229")
  val A232 = MonitorType.withName("A232")
  val A233 = MonitorType.withName("A233")
  val A235 = MonitorType.withName("A235")
  val A283 = MonitorType.withName("A283")
  val A286 = MonitorType.withName("A286")
  val A288 = MonitorType.withName("A288")
  val A289 = MonitorType.withName("A289")
  val A293 = MonitorType.withName("A293")
  val A296 = MonitorType.withName("A296")
  val C211 = MonitorType.withName("C211")
  val C212 = MonitorType.withName("C212")
  val C213 = MonitorType.withName("C213")
  val C214 = MonitorType.withName("C214")
  val C215 = MonitorType.withName("C215")
  val C216 = MonitorType.withName("C216")
  //New Mt
  val A234 = MonitorType.withName("A234")
  val A236 = MonitorType.withName("A236")
  val A237 = MonitorType.withName("A237")
  val A238 = MonitorType.withName("A238")
  val A239 = MonitorType.withName("A239")
  val A242 = MonitorType.withName("A242")
  val A244 = MonitorType.withName("A244")
  val A245 = MonitorType.withName("A245")

  val psiList = List(A214, A222, A224, A225, A293)
  val windDirList = List(MonitorType.withName("C212"))

  /*  val monitorReportList = 
    List(A222, A223, A293, A283, A224, A225, A226, A286, A296, A229, A232, A233, A235, A221,
        A213, A214, A215,        
        C211, C212, C214, C215, C216, C213)
  */

  val monitorReportList =
    List(A222, A223, A293, A283, A224, A225, A226, A286, A296, A229, A232, A233, A236, A242, A235, A237, A238, A239, A234, A244, A245,
      A221, A213, A214, A215,
      C211, C212, C214, C215, C216, C213)

  val calibrationList = List(A222, A223, A293, A283, A224, A225, A226, A286, A296, A229, A221,
    A213, A214, A215,
    A288, A289)

  val epaList =
    List(A214, A215, A222, A223, A224, A225, A226, A283, A286, A293, A296, C211, C212, C213, C214, C215)

  val epaReportList =
    List(C212, C211, A222, A293, A224, A225, A214, A226, A296)

  val epaMap = {
    map.filter(p => p._2.epa_mapping.isDefined).map(kv => (kv._2.epa_mapping.get, kv._1))
  }
  import com.github.nscala_time.time.Imports._
  import java.sql.Timestamp
  def getManualAuditTooltip(m: Monitor.Value, mt: MonitorType.Value, v: (Option[Float], Option[String]),
                            dataTime: DateTime, tabType: TableType.Value = TableType.Hour): String = {
    if (v._1.isEmpty || v._2.isEmpty)
      return ""

    val tagInfo = MonitorStatus.getTagInfo(v._2.get)
    if (tagInfo.statusType != StatusType.Manual)
      return ""

    val auditLogOpt = ManualAuditLog.getLog(tabType, m, dataTime, mt)
    if (auditLogOpt.isEmpty)
      return ""

    val log = auditLogOpt.get
    val reason = if (log.reason.isDefined)
      log.reason.get
    else
      "無"

    return s"""
      title=${log.modified_time.toString("YYYY-MM-dd-HH:mm")}-${log.operator}註記-理由:${reason}
      data-toggle=tooltip data-container=body data-trigger=hover
      """
  }
  def getStyleStr(m: Monitor.Value, mt: MonitorType.Value, v: (Option[Float], Option[String])) = {
    val mtCase = map(mt)
    if (v._1.isEmpty || v._2.isEmpty)
      s"Color:Black;background-color:White"
    else {
      val value = v._1.get
      val status = v._2.get

      val internal_std = Monitor.map(m).getStdInternal(mt)
      val overInternal =
        if (internal_std.isDefined && (value > internal_std.get))
          true
        else
          false

      val overLaw =
        if (mtCase.std_law.isDefined && (value > mtCase.std_law.get))
          true
        else
          false

      MonitorStatus.getCssStyleStr(status, overInternal, overLaw)
    }
  }

  def format(mt: MonitorType.Value, v: Option[Float]) = {
    if (v.isEmpty)
      "-"
    else {
      val prec = map(mt).prec
      s"%.${prec}f".format(v.get)
    }
  }

  def formatAvg(avg: Option[Float]) = {
    if (avg.isEmpty)
      "-"
    else {
      s"%.0f".format(avg.get * 100)
    }
  }

  def formatValue(v: Option[Float]) = {
    if (v.isEmpty)
      "-"
    else {
      s"%.0f".format(v.get)
    }
  }
}