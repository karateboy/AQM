package models

import play.api.libs.json.{Reads, Writes}
import scalikejdbc._
import scala.collection.Map
import models.ModelHelper._

case class MonitorType(id: String, desp: String, unit: String,
                       zd_internal: Option[Float], zd_law: Option[Float],
                       sd_internal: Option[Float], sd_law: Option[Float],
                       epa_mapping: Option[String],
                       prec: Int, order: Int)

object MonitorType extends Enumeration {
  implicit val mtReads: Reads[MonitorType.Value] = EnumUtils.enumReads(MonitorType)
  implicit val mtWrites: Writes[MonitorType.Value] = EnumUtils.enumWrites

  val Other = Value("Oth")
  val OtherCase = MonitorType("Oth", "其他", "", None, None, None, None, None, 0, 0)

  private def mtList: List[MonitorType] =
    DB readOnly { implicit session =>
      sql"""
        Select *
        From MonitorType
      """.map { r =>
        MonitorType(id = r.string(1),
          desp = r.string(2),
          unit = r.string(3),
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
        else if (colname.equalsIgnoreCase("DESP") || colname.equalsIgnoreCase("UNIT")) {
          Some(newValue)
        } else {
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
  val eapIdMap = {
    val pairs =
      for(epaMt <- epaList if MonitorType.map(epaMt).epa_mapping.isDefined)
        yield {
          MonitorType.map(epaMt).epa_mapping.get.toInt -> epaMt
        }
    pairs.toMap
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
    if (v._1.isEmpty || v._2.isEmpty)
      s"Color:Black;background-color:White"
    else {
      val value = v._1.get
      val status = v._2.get

      val internal_std = MonitorTypeAlert.map(m)(mt).internal
      val overInternal =
        if (internal_std.isDefined && (value >= internal_std.get))
          true
        else
          false


      val overLaw =
        if (MonitorTypeAlert.map(m)(mt).std_law.isDefined && (value >= MonitorTypeAlert.map(m)(mt).std_law.get))
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
      if(prec != 0)
        s"%.${prec}f".format(v.get)
      else
        s"%d".format(Math.round(v.get))
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