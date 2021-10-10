package models
import java.sql.Date
import java.sql.Timestamp
import scalikejdbc._
import play.api._
import com.github.nscala_time.time.Imports._
import models.ModelHelper._
import models._
import play.api.libs.json.Json

object Alarm {

  case class Alarm(monitor: Monitor.Value, mItem: String, time: DateTime, mVal: Float, code: String, ticket: Option[String] = None)
  implicit val alarmWrite = Json.writes[Alarm]
  implicit val alarmRead = Json.reads[Alarm]

  def getAlarm(monitors: Seq[Monitor.Value], statusFilter: Option[Seq[String]], start: DateTime, end: DateTime)(implicit session: DBSession = AutoSession): List[Alarm] = {
    val mStr = SQLSyntax.createUnsafely(monitors.mkString("('", "','", "')"))
    val startT: Timestamp = start
    val endT: Timestamp = end
    val tab = getTabName(start.getYear)
    assert(start <= end)
    val result =
      if (statusFilter.isEmpty) {
        sql"""
        Select *
        From ${tab}
        Where DP_NO in ${mStr} and M_DateTime>=${startT} and M_DateTime<${end}
        ORDER BY M_DateTime ASC
        """.map {
          rs =>
            Alarm(Monitor.withName(rs.string(1)), rs.string(2), rs.timestamp(3), rs.float(4),
              MonitorStatus.getTagInfo(rs.string(5).trim()).toString, rs.stringOpt(6))
        }.list.apply
      } else {
        val sfilter = SQLSyntax.createUnsafely(statusFilter.get.mkString("('", "','", "')"))

        sql"""
        Select *
        From ${tab}
        Where DP_NO in ${mStr} and M_DateTime>=${startT} and M_DateTime<${end} and CODE2 in ${sfilter}
        ORDER BY M_DateTime ASC
        """.map {
          rs =>
            Alarm(Monitor.withName(rs.string(1)), rs.string(2), rs.timestamp(3), rs.float(4),
              MonitorStatus.getTagInfo(rs.string(5).trim()).toString, rs.stringOpt(6))
        }.list.apply
      }

    if (start.getYear == end.getYear)
      result
    else
      result ++ getAlarm(monitors, statusFilter, DateTime.parse(s"${start.getYear + 1}-1-1"), end)

  }

  def getNotRepairAlarm(monitors: Seq[Monitor.Value], start: DateTime, end: DateTime)(implicit session: DBSession = AutoSession): List[Alarm] = {
    val mStr = SQLSyntax.createUnsafely(monitors.mkString("('", "','", "')"))
    val startT: Timestamp = start
    val endT: Timestamp = end
    val tab = getTabName(start.getYear)
    assert(start <= end)
    val result =
        sql"""
        Select *
        From ${tab}
        Where DP_NO in ${mStr} and M_DateTime>=${startT} and M_DateTime<${end} and CHK = 'NO'
        ORDER BY M_DateTime ASC
        """.map {
          rs =>
            Alarm(Monitor.withName(rs.string(1)), rs.string(2), rs.timestamp(3), rs.float(4),
              MonitorStatus.getTagInfo(rs.string(5).trim()).toString, rs.stringOpt(6))
        }.list.apply

    if (start.getYear == end.getYear)
      result
    else
      result ++ getNotRepairAlarm(monitors, DateTime.parse(s"${start.getYear + 1}-1-1"), end)

  }

  def getAlarmOpt(monitor: Monitor.Value, mItem: String, time: DateTime)(implicit session: DBSession = AutoSession) = {
    val tab = getTabName(time.getYear)
    val timeT : Timestamp = time
    sql"""
        Select *
        From ${tab}
        Where DP_NO = ${monitor.toString} and M_DateTime=${timeT} and M_ITEM = ${mItem}
        """.map {
      rs =>
        Alarm(Monitor.withName(rs.string(1)), rs.string(2), rs.timestamp(3), rs.float(4),
          MonitorStatus.getTagInfo(rs.string(5).trim()).toString, rs.stringOpt(6))
    }.single.apply
  }

  def getAlarmNoTicketList(start: DateTime)(implicit session: DBSession = AutoSession): List[Alarm] = {
    val tab = getTabName(start.getYear)
    val startT: Timestamp = start
    sql"""
        Select *
        From ${tab}
        Where M_DateTime>${startT} and CHK is NULL
        ORDER BY M_DateTime ASC
        """.map {
      rs =>
        Alarm(Monitor.withName(rs.string(1)), rs.string(2), rs.timestamp(3), rs.float(4),
          MonitorStatus.getTagInfo(rs.string(5).trim()).toString, rs.stringOpt(6))
    }.list.apply
  }

  def getAlarmAutoTicketList(start: DateTime)(implicit session: DBSession = AutoSession): List[Alarm] = {
    val tab = getTabName(start.getYear)
    val startT: Timestamp = start
    sql"""
        Select *
        From ${tab}
        Where M_DateTime>=${startT} and CHK = 'ATO'
        ORDER BY M_DateTime ASC
        """.map {
      rs =>
        Alarm(Monitor.withName(rs.string(1)), rs.string(2), rs.timestamp(3), rs.float(4),
          MonitorStatus.getTagInfo(rs.string(5).trim()).toString, rs.stringOpt(6))
    }.list.apply
  }
/*
  def getFirstAlarmNoTicket(start: DateTime)(implicit session: DBSession = AutoSession) = {
    val tab = getTabName(start.getYear)
    val startT: Timestamp = start
    sql"""
        Select Top 1 *
        From ${tab}
        Where M_DateTime>=${startT} and CHK is NULL
        ORDER BY M_DateTime ASC
        """.map {
      rs =>
        Alarm(Monitor.withName(rs.string(1)), rs.string(2), rs.timestamp(3), rs.float(4),
          MonitorStatus.getTagInfo(rs.string(5).trim()).toString, rs.stringOpt(6))
    }.single.apply
  }
*/
  
  def findSameAlarm(monitor: Monitor.Value, mItem: String, code: String)(start:DateTime, end:DateTime)(implicit session: DBSession = AutoSession) = {
    val tab = getTabName(start.getYear)
    val startT: Timestamp = start
    val endT: Timestamp = end
    
    sql"""
        Select Count(*)
        From $tab
        Where DP_NO = ${monitor.toString()} and M_ITEM = ${mItem} and 
        CODE2 = ${code} and M_DateTime >= $startT and M_DateTime < $endT and (CHK = 'YES' or CHK = 'NO' or CHK is NULL)        
        """.map { rs => rs.int(1) }.single.apply()
  }
  
  def updateAlarmTicketState(monitor: Monitor.Value, mItem: String, time: DateTime, state: String)(implicit session: DBSession = AutoSession) = {
    val tab = getTabName(time.getYear)
    val timeT: Timestamp = time
    sql"""
        Update ${tab}
        Set CHK = $state
        Where DP_NO = ${monitor.toString()} and M_DateTime = ${timeT} and M_ITEM = ${mItem}
        """.update.apply
  }

  def insertAlarm(ar: Alarm): Int = {
    val tab = getTabName(ar.time.getYear)
    def checkExist() = {
      import java.sql.Timestamp
      val time: Timestamp = ar.time

      DB readOnly { implicit session =>
        sql"""
          Select *
          From ${tab}
          Where DP_NO = ${ar.monitor.toString} and M_DateTime = ${time} and M_ITEM = ${ar.mItem}          
          """.map(rs =>
          Alarm(Monitor.withName(rs.string(1)), rs.string(2), rs.timestamp(3), rs.float(4),
            MonitorStatus.getTagInfo(rs.string(5).trim()).toString, rs.stringOpt(6))).single.apply
      }
    }
    val arOpt = checkExist()
    if (arOpt.isDefined)
      0
    else {
      DB localTx { implicit session =>
        sql"""
      INSERT INTO ${tab}
           ([DP_NO]
           ,[M_ITEM]
           ,[M_DateTime]
           ,[M_VAL]
           ,[CODE2]
           ,[CHK])
     VALUES
           (${ar.monitor.toString}
           ,${ar.mItem}
           ,${ar.time}
           ,${ar.mVal}
           ,${ar.code}
           ,NULL)
      """.update.apply
      }

    }
  }

  def getTabName(year: Int) = {
    SQLSyntax.createUnsafely(s"[P1234567_Alm_${year}]")
  }

  case class AlarmItem(id: String, desp: String)
  private def arList: List[AlarmItem] =
    DB readOnly { implicit session =>
      sql"""
        SELECT *
        FROM [alarmCode]
      """.map { r => AlarmItem(r.string(1), r.string(2))
      }.list.apply
    }
  
  val ticketMap = TicketType.map map {kv => kv._1.toString -> (kv._2 + "未執行") }

  private var _map: Map[String, String] = Map(arList.map { r => (r.id -> r.desp) }: _*) ++ ticketMap
  

  private def insertAlarmCode(ar:AlarmItem)={
    DB localTx {
      implicit session=>
        sql"""
          INSERT INTO [dbo].[alarmCode]
           ([ITEM]
           ,[DESP])
     VALUES
           ( ${ar.id}, ${ar.desp})
          """.update.apply
    }
  }
  
  def updateMap(ar:AlarmItem)={
    if(!_map.contains(ar.id)){
      insertAlarmCode(ar)
      _map = _map + (ar.id->ar.desp)
    }
  }
  
  private def deletePartAlarmCode(id:String)={
    val pattern = s"${id}_%"
    DB localTx {
      implicit session=>
        sql"""
          DELETE FROM [dbo].[alarmCode]
          WHERE ITEM like ${pattern}
          """.update.apply
    }
  }
  
  def removePartFromMap(id:String) = {
    deletePartAlarmCode(id)
    _map = _map flatMap {
      kv=> if(kv._1.startsWith(id))
        None
      else
        Some(kv._1->kv._2)
    }
  }
  
  def map(key: String) = {
    _map.getOrElse(key, "未知的警告代碼:" + key)
  }

  def getItem(ar:Alarm) = {
    val tokens = ar.mItem.split("-")
    if((ar.code == MonitorStatus.OVER_STAT || ar.code == MonitorStatus.WARN_STAT) && tokens.length == 3)
      _map.getOrElse(tokens(0), "未知的警告代碼:" + ar.mItem)
    else
      _map.getOrElse(ar.mItem, "未知的警告代碼:" + ar.mItem)
  }

  def getReason(ar:Alarm) = {
    val tokens = ar.mItem.split("-")
    if((ar.code == MonitorStatus.OVER_STAT || ar.code == MonitorStatus.WARN_STAT) && tokens.length == 3){
      val dataType = tokens(1) match {
        case DataType.Hour=>
          "小時值"
        case DataType.EightHour=>
          "8小時平均值"
        case DataType.Day=>
          "日平均值"
        case DataType.TwentyFourHour=>
          "24小時值"
        case DataType.Year=>
          "年平均值"
      }
      val alarmLevel = tokens(2) match {
        case AlarmLevel.Internal=>
          "內控值"
        case AlarmLevel.Warn=>
          "警戒值"
        case AlarmLevel.Law=>
          "法規值"
      }
      s"超出${dataType}${alarmLevel}"
    }else
      MonitorStatus.map(ar.code).desp
  }

  def getMonitorTypeValue(ar:Alarm)={
    val tokens = ar.mItem.split("-")
    if(ar.code == MonitorStatus.OVER_STAT && tokens.length == 3){
      val mt = MonitorType.withName(tokens(0))
      val mtCase = MonitorType.map(mt)
      s"${MonitorType.format(mt, Some(ar.mVal))}${mtCase.unit}"
    }else {
      if(ar.mVal == 0)
        "解除"
      else
        "觸發"
    }
  }
}