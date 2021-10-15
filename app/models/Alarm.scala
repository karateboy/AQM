package models

import com.github.nscala_time.time.Imports._
import models.ModelHelper._
import play.api.libs.json.Json
import scalikejdbc._

import java.sql.Timestamp

object Alarm {
  val ticketMap = TicketType.map map { kv => kv._1.toString -> (kv._2 + "未執行") }

  implicit val alarmWrite = Json.writes[Alarm]
  implicit val alarmRead = Json.reads[Alarm]
  private var _map: Map[String, String] = Map(arList.map { r => (r.id -> r.desp) }: _*) ++ ticketMap

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
        Where DP_NO in ${mStr} and M_DateTime>=${startT} and M_DateTime<${endT}
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
        Where DP_NO in ${mStr} and M_DateTime>=${startT} and M_DateTime<${end} and CHK = 'NO' or CHK is NULL
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
    val timeT: Timestamp = time
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

  def findSameAlarm(monitor: Monitor.Value, mItem: String, code: String)(start: DateTime, end: DateTime)(implicit session: DBSession = AutoSession) = {
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

  def insertAlarm(ar: Alarm)(implicit session: DBSession = AutoSession): Int = {
    val tab = getTabName(ar.time.getYear)

    def checkExist() = {
      import java.sql.Timestamp
      val time: Timestamp = ar.time
      sql"""
          Select *
          From ${tab}
          Where DP_NO = ${ar.monitor.toString} and M_DateTime = ${time} and M_ITEM = ${ar.mItem}
          """.map(rs =>
        Alarm(Monitor.withName(rs.string(1)), rs.string(2), rs.timestamp(3), rs.float(4),
          MonitorStatus.getTagInfo(rs.string(5).trim()).toString, rs.stringOpt(6))).single.apply
    }

    val arOpt = checkExist()
    if (arOpt.isDefined)
      0
    else {
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

  def getTabName(year: Int) = {
    SQLSyntax.createUnsafely(s"[P1234567_Alm_${year}]")
  }

  def updateMap(ar: AlarmItem) = {
    if (!_map.contains(ar.id)) {
      insertAlarmCode(ar)
      _map = _map + (ar.id -> ar.desp)
    }
  }

  private def insertAlarmCode(ar: AlarmItem)(implicit session: DBSession = AutoSession) = {
        sql"""
          INSERT INTO [dbo].[alarmCode]
           ([ITEM]
           ,[DESP])
     VALUES
           ( ${ar.id}, ${ar.desp})
          """.update.apply

  }

  def removePartFromMap(id: String) = {
    deletePartAlarmCode(id)
    _map = _map flatMap {
      kv =>
        if (kv._1.startsWith(id))
          None
        else
          Some(kv._1 -> kv._2)
    }
  }

  private def deletePartAlarmCode(id: String)(implicit session: DBSession = AutoSession) = {
    val pattern = s"${id}_%"
        sql"""
          DELETE FROM [dbo].[alarmCode]
          WHERE ITEM like ${pattern}
          """.update.apply

  }

  def getItem(ar: Alarm) = {
    val tokens = ar.mItem.split("-")
    if ((ar.code == MonitorStatus.OVER_STAT || ar.code == MonitorStatus.WARN_STAT) && tokens.length == 3) {
      _map.getOrElse(tokens(0), "未知的警告代碼:" + ar.mItem)
    } else
      _map.getOrElse(ar.mItem, "未知的警告代碼:" + ar.mItem)
  }

  def isOverStd(ar: Alarm): Boolean = {
    val tokens = ar.mItem.split("-")
    (ar.code == MonitorStatus.OVER_STAT || ar.code == MonitorStatus.WARN_STAT) && tokens.length == 3
  }

  def getOverStdLevel(ar: Alarm): AlarmLevel.Value = {
    val tokens = ar.mItem.split("-")
    AlarmLevel.withName(tokens(2))
  }

  def getOverStdLevelCode(ar: Alarm): Int = {
    val tokens = ar.mItem.split("-")
    val overStdLevel = AlarmLevel.withName(tokens(2))
    AlarmLevel.map(overStdLevel).code
  }

  def getReason(ar: Alarm) = {
    val tokens = ar.mItem.split("-")
    if ((ar.code == MonitorStatus.OVER_STAT || ar.code == MonitorStatus.WARN_STAT) && tokens.length == 3) {
      val dataType = AlarmDataType.map(AlarmDataType.withName(tokens(1)))
      val alarmLevel = AlarmLevel.map(AlarmLevel.withName(tokens(2))).desc

      s"超出${dataType}${alarmLevel}"
    } else
      MonitorStatus.map(ar.code).desp
  }

  def getMonitorTypeValue(ar: Alarm) = {
    val tokens = ar.mItem.split("-")
    if ((ar.code == MonitorStatus.OVER_STAT || ar.code == MonitorStatus.WARN_STAT) && tokens.length == 3) {
      val mt = MonitorType.withName(tokens(0))
      val mtCase = MonitorType.map(mt)
      s"${MonitorType.format(mt, Some(ar.mVal))}${mtCase.unit}"
    } else {
      if (ar.mVal == 0)
        "解除"
      else
        "觸發"
    }
  }

  def getAlarmOverStdList(monitors: Seq[Monitor.Value], start: DateTime, end: DateTime)
                         (implicit session: DBSession = AutoSession): List[Alarm] = {
    val mStr = SQLSyntax.createUnsafely(monitors.mkString("('", "','", "')"))
    val tab = getTabName(start.getYear)
    val startT: Timestamp = start
    val endT: Timestamp = end
    sql"""
        Select *
        From ${tab}
        Where M_DateTime>=${startT} and M_DateTime<${endT} and M_ITEM like '%-%-%' and DP_NO in ${mStr}
        ORDER BY M_DateTime Desc
        """.map {
      rs =>
        Alarm(Monitor.withName(rs.string(1)), rs.string(2), rs.timestamp(3), rs.float(4),
          MonitorStatus.getTagInfo(rs.string(5).trim()).toString, rs.stringOpt(6))
    }.list.apply
  }

  private def arList: List[AlarmItem] =
    DB readOnly { implicit session =>
      sql"""
        SELECT *
        FROM [alarmCode]
      """.map { r => AlarmItem(r.string(1), r.string(2))
      }.list.apply
    }

  private def map(key: String) = {
    _map.getOrElse(key, "未知的警告代碼:" + key)
  }

  case class Alarm(monitor: Monitor.Value, mItem: String, time: DateTime, mVal: Float, code: String, ticket: Option[String] = None)

  case class AlarmItem(id: String, desp: String)
}