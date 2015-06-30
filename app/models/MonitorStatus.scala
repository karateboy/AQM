package models

import play.api.Logger
import scalikejdbc._
import scalikejdbc.config._

object StatusType extends Enumeration{
  val Internal = Value("0")
  val Auto     = Value("A")
  val Manual   = Value("M")
}

case class MonitorStatus(statusType:StatusType.Value, id:String, desp:String, outage:Boolean, valid:Boolean)
case class TagInfo(statusType:StatusType.Value, id:String){
  override def toString={
    statusType + id
  }
}

object MonitorStatus extends Enumeration{
  private val msList:List[MonitorStatus] =
    DB readOnly{ implicit session =>
      sql"""
        SELECT [statusNo],[statusName],[isOutage],[isValid]
        FROM [AQMSDB].[dbo].[Infor_Status]
      """.map { r =>  
        val tagInfo = getTagInfo(r.string(1)) 
        MonitorStatus(tagInfo.statusType, tagInfo.id, r.string(2), r.boolean(3), r.boolean(4)    
      )}.list.apply
    }
  
  def getTagInfo(tag:String)= {
    if(tag.length() == 2)
      TagInfo(StatusType.Internal, tag)
    else{
      val statusType = StatusType.withName(tag.charAt(0).toString)
      val id = tag.substring(1)
      TagInfo(statusType, id)
    }
  }

  val NORMAL_STAT = "010"

  def isNormalStat(s: String) = {
    val OVER_STAT = "011"
    val BELOW_STAT = "012"

    val VALID_STATS = List(NORMAL_STAT, OVER_STAT, BELOW_STAT).map(getTagInfo)
    VALID_STATS.contains(getTagInfo(s))
  }

  def isCalbration(s: String) = {
    val CALBRATION_STAT = "020"
    val CALBRATION_DIVERSION_STAT = "022"

    val CALBRATION_STATS = List(CALBRATION_STAT, CALBRATION_DIVERSION_STAT).map(getTagInfo)
    CALBRATION_STATS.contains(getTagInfo(s))
  }
  
  def isRepairing(s: String)={
    val REPAIR = "031"
    REPAIR == getTagInfo(s)
  }
  
  val DATA_LOSS_STAT = "036"
  val REPAIR_STAT = "031"
  def isMaintance(s: String)={
    REPAIR_STAT == getTagInfo(s)
  }
  
  def isError(s: String)={
    !(isNormalStat(s)||isCalbration(s)||isRepairing(s)||isMaintance(s))  
  }
  
  def getTagStr(s:MonitorStatus)=TagInfo(s.statusType, s.id).toString()
  
  def getCssColorStr(tag:String)={
    val info=getTagInfo(tag)
    info.statusType match {
      case StatusType.Internal=>
        {
          if(isNormalStat(tag))
            "White"
          else if(isCalbration(tag))
            "Chartreuse"
          else if(isRepairing(tag))
            "DarkOrchid"
          else if(isMaintance(tag))
            "DarkOrange"
          else 
            "Crimson"
        }
      case StatusType.Auto=>
        "Cyan"
      case StatusType.Manual=>
        "Gold"
    }
  }
  
  val map:Map[Value, MonitorStatus] = Map(msList.map{s=>Value(getTagStr(s))->s}:_*)
  val msvList = msList.map {r=>MonitorStatus.withName(getTagStr(r))}
  val alarmList = msvList.filter { _ != MonitorStatus.withName(getTagInfo(NORMAL_STAT).toString) }
}
