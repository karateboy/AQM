package models
import java.sql.Date
import java.sql.Timestamp
import scalikejdbc._
import play.api._
import com.github.nscala_time.time.Imports._
import models.ModelHelper._
import models._

object Alarm {

  case class Alarm(monitor: Monitor.Value, mItem: String, time: DateTime, mVal: Float, code: String)
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
              MonitorStatus.getTagInfo(rs.string(5).trim()).toString)
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
              MonitorStatus.getTagInfo(rs.string(5).trim()).toString)
        }.list.apply
      }

    if (start.getYear == end.getYear)
      result
    else
      result ++ getAlarm(monitors, statusFilter, DateTime.parse(s"${start.getYear + 1}-1-1"), end)

  }

  def insertAlarm(ar: Alarm) = {
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
            MonitorStatus.getTagInfo(rs.string(5).trim()).toString)).single.apply
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
    SQLSyntax.createUnsafely(s"[AQMSDB].[dbo].[P1234567_Alm_${year}]")
  }

  case class AlarmItem(id: String, desp: String)
  private val arList: List[AlarmItem] =
    DB readOnly { implicit session =>
      sql"""
        SELECT *
        FROM [AQMSDB].[dbo].[alarmCode]
      """.map { r => AlarmItem(r.string(1), r.string(2))
      }.list.apply
    }
  private val _map: Map[String, String] = Map(arList.map { r => (r.id -> r.desp) }: _*)

  def map(key: String) = {
    _map.getOrElse(key, "未知的警告代碼:" + key)
  }

}