package models

import scalikejdbc._

object StatusType extends Enumeration {
  val Internal = Value("0")
  val Auto = Value("A")
  val Manual = Value("M")

  def map = Map(Internal -> "系統", Auto -> "自動註記", Manual -> "人工註記")
}

case class MonitorStatus(info: TagInfo, desp: String)

case class TagInfo(statusType: StatusType.Value, auditRule: Option[Char], id: String) {
  override def toString = {
    if ((statusType == StatusType.Auto || statusType == StatusType.Manual)
      && auditRule.isDefined)
      auditRule.get + id
    else
      statusType + id
  }
}

object MonitorStatus {
  val UNAVOIDABLE_STAT = "000"
  val NORMAL_STAT = "010"
  val WARN_STAT = "011"
  val OVER_STAT = "016"
  val BELOW_STAT = "012"
  val CALBRATION_STAT = "020"
  val CALBRATION_SPAN_STAT = "021"
  val CALBRATION_BACK_STAT = "026"
  val CALBRATION_DIVERSION_STAT = "022"
  val CALBRATION_FAILED = "030"
  val REPAIR = "031"
  val OVERRANGE_DATA = "032"
  val DATA_LOSS_STAT = "036"
  val MAINTANCE_STAT = "038"
  val DATA_VALID_HIGH = "099"
  val OverInternalColor = "Blue"
  val OverLawColor = "Red"
  val NormalColor = "White"
  val CalibrationColor = "Chartreuse"
  val RepairColor = "DarkOrchid"
  val MaintanceColor = "DarkOrange"
  val AbnormalColor = "DarkRed"
  val AutoAuditColor = "Cyan"
  val ManualAuditColor = "Gold"
  def msList: List[MonitorStatus] =
    DB readOnly { implicit session =>
      sql"""
        SELECT [statusNo],[statusName]
        FROM [Infor_Status]
        Order by [statusNo] Asc
      """.map { r =>
        val tagInfo = getTagInfo(r.string(1))
        MonitorStatus(tagInfo, r.string(2).trim())
      }.list.apply
    }

  lazy val msvList = msList.map { r => r.info.toString }
  lazy val manualMonitorStatusList = {
    msvList.filter {
      _map(_).info.statusType == StatusType.Manual
    }
  }
  val alarmList = List("000", "016", "031", "033", "035", "036",
    "043",
    "050", "052", "053", "054", "056", "057", "058", "059", "060",
    "a10", "b10", "c10", "d10", "e10", "f10", "g10", "h10", "i10", "j10", "k10")
  private var _map: Map[String, MonitorStatus] = refreshMap

  def switchTagToInternal(tag: String) = {
    val info = getTagInfo(tag)
    '0' + info.id
  }

  def getAutoAuditTagStr(lead: Char, info: TagInfo) = {

  }

  def getExplainStr(tag: String) = {
    val tagInfo = getTagInfo(tag)
    if (tagInfo.statusType == StatusType.Auto) {
      val t = tagInfo.auditRule.get
      AutoAudit.map(t.toLower)
    } else {
      val ms = map(tag)
      ms.desp
    }
  }

  def map(key: String) = {
    _map.getOrElse(key, {
      val tagInfo = getTagInfo(key)
      tagInfo.statusType match {
        case StatusType.Auto =>
          val ruleId = tagInfo.auditRule.get.toLower
          MonitorStatus(tagInfo, s"自動註記:${AutoAudit.map(ruleId)}")
        case StatusType.Manual =>
          MonitorStatus(tagInfo, "人工註記")
        case StatusType.Internal =>
          //FIXME
          if (key == "011")
            MonitorStatus(tagInfo, "舊超限")
          else
            MonitorStatus(tagInfo, "未知:" + key)
      }

    })
  }

  def isInvalidOrCalibration(s: String) = {
    val tagInfo = getTagInfo(s)
    val targetStats = List("030", "035", "026").map(getTagInfo)
    tagInfo.statusType match {
      case StatusType.Internal =>
        targetStats.contains(getTagInfo(s))
      case StatusType.Auto =>
        false
      case _ =>
        false
    }
  }

  def isValid(s: String) = {
    val tagInfo = getTagInfo(s)
    val VALID_STATS = List(NORMAL_STAT, WARN_STAT, OVER_STAT, BELOW_STAT, CALBRATION_DIVERSION_STAT, OVERRANGE_DATA, DATA_VALID_HIGH).map(getTagInfo)

    tagInfo.statusType match {
      case StatusType.Internal =>
        VALID_STATS.contains(tagInfo)
      case StatusType.Auto =>
        if (SystemConfig.getConfig(SystemConfig.AutoAuditAsNormal, "True").toBoolean)
          true
        else {
          if (tagInfo.auditRule.isDefined && tagInfo.auditRule.get.isLower)
            true
          else
            false
        }
      case StatusType.Manual =>
          false
    }
  }

  def isNormal(s: String) = {
    getTagInfo(NORMAL_STAT) == getTagInfo(s)
  }

  def isOver(s: String): Boolean = {
    val overStatId = List(OVER_STAT, WARN_STAT).map(getTagInfo).map(_.id)
    getTagInfo(s) match {
      case TagInfo(_, _, id)=>
        overStatId.contains(id)
    }
  }

  def isCalbrating(s: String) = {
    val CALBRATION_STATS = List(CALBRATION_STAT, CALBRATION_SPAN_STAT, CALBRATION_BACK_STAT).map(getTagInfo)
    CALBRATION_STATS.contains(getTagInfo(s))
  }

  def isOverInternal(s: String) = {
    getTagInfo(CALBRATION_DIVERSION_STAT) == getTagInfo(s)
  }

  def getTagInfo(tag: String): TagInfo = {
    if (tag.length() == 2)
      TagInfo(StatusType.Internal, None, tag)
    else {
      val id = tag.substring(1)
      val t = tag.charAt(0)
      if (t == '0')
        TagInfo(StatusType.Internal, None, id)
      else if (t == 'm' || t == 'M') {
        TagInfo(StatusType.Manual, Some(t), id)
      } else if (t.isLetter)
        TagInfo(StatusType.Auto, Some(t), id)
      else
        throw new Exception("Unknown type:" + t)
    }
  }

  def isCalbrationFailed(s: String) = {
    getTagInfo(CALBRATION_FAILED) == getTagInfo(s)
  }

  def isError(s: String) = {
    !(isNormalStat(s) || isCalbration(s) || isRepairing(s) || isMaintance(s))
  }

  def isDataLost(s: String) = {
    getTagInfo(DATA_LOSS_STAT) == getTagInfo(s)
  }

  def getCssStyleStr(tag: String, overInternal: Boolean = false, overLaw: Boolean = false) = {
    val bkColor = getBkColorStr(tag)
    val fgColor =
      if (overLaw)
        "Red"
      else if (overInternal)
        "Blue"
      else
        "Black"
    s"Color:${fgColor};background-color:${bkColor}"
  }

  def getBkColorStr(tag: String) = {
    val info = getTagInfo(tag)
    info.statusType match {
      case StatusType.Internal => {
        if (isNormalStat(tag))
          NormalColor
        else if (isCalbration(tag))
          CalibrationColor
        else if (isRepairing(tag))
          RepairColor
        else if (isMaintance(tag))
          MaintanceColor
        else
          AbnormalColor
      }
      case StatusType.Auto =>
        AutoAuditColor
      case StatusType.Manual =>
        ManualAuditColor
    }
  }

  def isNormalStat(s: String) = {
    val tagInfo = getTagInfo(s)
    val VALID_STATS = List(NORMAL_STAT, WARN_STAT, OVER_STAT, BELOW_STAT, CALBRATION_DIVERSION_STAT,
      OVERRANGE_DATA,  DATA_VALID_HIGH
    ).map(getTagInfo)

    tagInfo.statusType match {
      case StatusType.Internal =>
        VALID_STATS.contains(getTagInfo(s))
      case StatusType.Auto =>
        if (tagInfo.auditRule.isDefined && tagInfo.auditRule.get.isLower)
          true
        else
          false
      case _ =>
        false
    }
  }

  def isCalbration(s: String) = {
    val CALBRATION_STATS = List(CALBRATION_STAT, CALBRATION_SPAN_STAT, CALBRATION_BACK_STAT).map(getTagInfo)
    CALBRATION_STATS.contains(getTagInfo(s))
  }

  def isRepairing(s: String) = {
    getTagInfo(REPAIR) == getTagInfo(s)
  }

  def isMaintance(s: String) = {
    getTagInfo(MAINTANCE_STAT) == getTagInfo(s)
  }

  def update(tag: String, desp: String) = {
    DB localTx { implicit session =>
      sql"""
        Update Infor_Status
        Set statusName=${desp}
        Where statusNo=${tag}
        """.update.apply
    }
    refreshMap
  }

  private def refreshMap() = {
    _map = Map(msList.map { s => s.info.toString() -> s }: _*)
    _map
  }
}
