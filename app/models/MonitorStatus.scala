package models

import play.api.Logger
import scalikejdbc._
import scalikejdbc.config._

object StatusType extends Enumeration{
  val Internal = Value("0")
  val Auto     = Value("A")
  val Manual   = Value("M")
  def map= Map(Internal->"系統", Auto->"自動註記", Manual->"人工註記")
}

case class MonitorStatus(statusType:StatusType.Value, id:String, desp:String, outage:Boolean, valid:Boolean)
case class TagInfo(statusType:StatusType.Value, auditRule:Option[Char], id:String){
  override def toString={
    if(statusType == StatusType.Auto  && auditRule.isDefined)
      auditRule.get + id
    else
      statusType + id
  }
}

object MonitorStatus {
  def msList:List[MonitorStatus] =
    DB readOnly{ implicit session =>
      sql"""
        SELECT [statusNo],[statusName],[isOutage],[isValid]
        FROM [AQMSDB].[dbo].[Infor_Status]
        Order by [statusNo] Asc
      """.map { r =>  
        val tagInfo = getTagInfo(r.string(1)) 
        MonitorStatus(tagInfo.statusType, tagInfo.id, r.string(2).trim(), r.boolean(3), r.boolean(4)    
      )}.list.apply
    }

  def getTagInfo(tag: String) = {
    if (tag.length() == 2)
      TagInfo(StatusType.Internal, None, tag)
    else {
      val id = tag.substring(1)
      val t = tag.charAt(0)
      if (t == '0')
        TagInfo(StatusType.Internal, None, id)
      else if (t == 'm' || t == 'M') {
        TagInfo(StatusType.Manual, None, id)
      } else if (t.isLetter)
        TagInfo(StatusType.Auto, Some(t), id)
      else
        throw new Exception("Unknown type:" + t)
    }
  }

  def getAutoAuditTagStr(lead:Char, info:TagInfo)={
    
  }
    
  def getExplainStr(tag:String)={
    val tagInfo = getTagInfo(tag)
    if(tagInfo.statusType == StatusType.Auto){
      val t = tag.charAt(0)
      AutoAudit.map(t.toLower)
    }else {
      val ms = map(tag)
      ms.desp
    }
  }
  
  val UNAVOIDABLE_STAT = "000"
  val NORMAL_STAT = "010"
  val OVER_STAT = "011"
  val BELOW_STAT = "012"

  def isNormalStat(s: String) = {
    val tagInfo = getTagInfo(s)
    val VALID_STATS = List(NORMAL_STAT, OVER_STAT, BELOW_STAT, CALBRATION_DIVERSION_STAT).map(getTagInfo)
    
    tagInfo.statusType match {
      case StatusType.Internal =>
        VALID_STATS.contains(getTagInfo(s))
      case StatusType.Auto=>
        if(tagInfo.auditRule.isDefined && tagInfo.auditRule.get.isLower)
          true
        else
          false
      case _ =>
        false
    }
  }

  def isValid(s: String) = {
    val tagInfo = getTagInfo(s)
    val VALID_STATS = List(NORMAL_STAT, OVER_STAT, BELOW_STAT, CALBRATION_DIVERSION_STAT).map(getTagInfo)
    
    tagInfo.statusType match {
      case StatusType.Internal =>
        VALID_STATS.contains(getTagInfo(s))
      case StatusType.Auto=>
        if(tagInfo.auditRule.isDefined && tagInfo.auditRule.get.isLower)
          true
        else
          false
      case _ =>
        false
    }
  }
  
  def isNormal(s:String)={
    getTagInfo(NORMAL_STAT) == getTagInfo(s)
  }
  
  def isOver(s:String)={
    getTagInfo(OVER_STAT) == getTagInfo(s)
  }
  
  val CALBRATION_STAT = "020"
  val CALBRATION_SPAN_STAT = "021"
  val CALBRATION_BACK_STAT = "026"
  val CALBRATION_DIVERSION_STAT = "022"
  
  val CALBRATION_FAILED = "030"
  
  def isCalbration(s: String) = {
    val CALBRATION_STATS = List(CALBRATION_STAT, CALBRATION_SPAN_STAT, CALBRATION_BACK_STAT).map(getTagInfo)
    CALBRATION_STATS.contains(getTagInfo(s))
  }

  def isCalbrating(s: String)={
   val CALBRATION_STATS = List(CALBRATION_STAT, CALBRATION_SPAN_STAT, CALBRATION_BACK_STAT).map(getTagInfo)
    CALBRATION_STATS.contains(getTagInfo(s))
  }
  
  def isOverInternal(s:String)={
    getTagInfo(CALBRATION_DIVERSION_STAT) == getTagInfo(s)
  }
  
  def isCalbrationFailed(s:String)={
    getTagInfo(CALBRATION_FAILED) == getTagInfo(s)
  }
  
  val REPAIR = "031"
  val INVALID_DATA = "032"
  
  def isRepairing(s: String)={
    getTagInfo(REPAIR) == getTagInfo(s)
  }
  
  def isInvalidData(s:String)={
    getTagInfo(INVALID_DATA) == getTagInfo(s)
  }
  
  val DATA_LOSS_STAT = "036"
  val MAINTANCE_STAT = "038"
  
  def isMaintance(s: String)={  
    getTagInfo(MAINTANCE_STAT) == getTagInfo(s)
  }
  
  def isError(s: String)={
    !(isNormalStat(s)||isCalbration(s)||isRepairing(s)||isMaintance(s))  
  }
  
  def isDataLost(s:String)={
    getTagInfo(DATA_LOSS_STAT) == getTagInfo(s)
  }
  
  def getTagStr(s:MonitorStatus)=TagInfo(s.statusType, None, s.id).toString()
  
  def getCssStyleStr(tag:String, overInternal:Boolean=false, overLaw:Boolean=false)={
   val bkColor =  getBkColorStr(tag)
   val fgColor = 
     if(overLaw)
       "Red"
     else if(overInternal)
       "Blue"
     else
       "Black"
    s"Color:${fgColor};background-color:${bkColor}"
  }
  
  val OverInternalColor = "Blue"
  val OverLawColor = "Red"
  val NormalColor = "White"
  val CalibrationColor = "Chartreuse"
  val RepairColor = "DarkOrchid"
  val MaintanceColor = "DarkOrange"
  val AbnormalColor = "DarkRed"
  val AutoAuditColor = "Cyan"
  val ManualAuditColor = "Gold"
  
  def getBkColorStr(tag:String)={
    val info=getTagInfo(tag)
    info.statusType match {
      case StatusType.Internal=>
        {
          if(isNormalStat(tag))
            NormalColor
          else if(isCalbration(tag))
            CalibrationColor
          else if(isRepairing(tag))
            RepairColor
          else if(isMaintance(tag))
            MaintanceColor
          else 
            AbnormalColor
        }
      case StatusType.Auto=>
        AutoAuditColor
      case StatusType.Manual=>
        ManualAuditColor
    }
  }
  def update(tag:String, desp:String)={
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
    _map = Map(msList.map{s=>getTagStr(s)->s}:_*)
    _map
  }
  private var _map:Map[String, MonitorStatus] = refreshMap
  val msvList = msList.map {r=>getTagStr(r)}
  val manualMonitorStatusList = {msvList.filter { _map(_).statusType == StatusType.Manual }}
  val alarmList = List("011","030", "033", "034", "035",  
      "043",
      "050", "051", "052", "053", "054", "055", "056", "057", "058", "059")

  def map(key: String) = {
    _map.getOrElse(key, {
      val tagInfo = getTagInfo(key)
      tagInfo.statusType match {
        case StatusType.Auto =>
          MonitorStatus(tagInfo.statusType, tagInfo.id, "自動註記", false, false)
        case StatusType.Manual =>
          MonitorStatus(tagInfo.statusType, tagInfo.id, "人工註記", false, false)
        case StatusType.Internal =>
          MonitorStatus(tagInfo.statusType, tagInfo.id, "未知:" + key, false, false)
      }

    })
  }
}
