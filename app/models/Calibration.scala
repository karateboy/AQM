package models
import scalikejdbc._
import java.sql.Date
import java.sql.Timestamp
import scalikejdbc._
import play.api._
import com.github.nscala_time.time.Imports._
import com.github.nscala_time.time._
import models.ModelHelper._

object Calibration {
  
  case class CalibrationItem(monitor:Monitor.Value, monitorType:MonitorType.Value,
        startTime:DateTime, endTime:DateTime, span:Float, z_std:Float, z_val:Float, 
        zd_val:Float, zd_pnt:Float,
        s_std:Float, s_sval:Float, sd_val:Float, sd_pnt:Float
      )

  def getTabName(year: Int) = {
    SQLSyntax.createUnsafely(s"[P1234567_Cal_${year}]")
  }
  
  def calibrationQueryReport(monitor: Monitor.Value, start: Timestamp, end: Timestamp) = {
    val tab = getTabName(start.toDateTime.getYear)
    DB readOnly { implicit session =>
      sql"""
      SELECT *
      FROM ${tab}
      Where DP_NO=${monitor.toString} and S_DateTime >= ${start} and S_DateTime < ${end}
      Order by S_DateTime
      """.map { rs =>
        val monitor = Monitor.withName(rs.string(1))
        val monitorType = MonitorType.withName(rs.string(2).replace("A4", "A2"))
        val startTime = rs.timestamp(3)
        val endTime = rs.timestamp(4)
        val span = rs.float(5)
        val z_std = rs.float(6)
        val z_val = rs.float(7)
        val zd_val = rs.float(8)
        val zd_pnt = rs.float(9)
        val s_std = rs.float(10)
        val s_sval = rs.float(11)
        val sd_val = rs.float(12)
        val sd_pnt = rs.float(13)
        CalibrationItem(monitor, monitorType, startTime, endTime, span, z_std, z_val, zd_val, zd_pnt, s_std, s_sval, sd_val, sd_pnt)
      }.list.apply

    }
  }
  
  def calibrationSummary(monitor: Monitor.Value, start: Timestamp, end: Timestamp) ={
    val reports = calibrationQueryReport(monitor, start, end)
    val pairs =
    for(r <- reports)
    yield 
      r.monitorType -> r
    
    Map(pairs :_*).values.toList.sortBy { item => item.startTime }
  }

  def calibrationMonthly(monitor: Monitor.Value, monitorType: MonitorType.Value, start: DateTime) = {
    val end = start + 1.month
    val mtStr = monitorType.toString().replace("A2", "A4")
    val tab = getTabName(start.toDateTime.getYear)
    val report =
      DB readOnly { implicit session =>
        sql"""
      SELECT *
      FROM ${tab}
      Where DP_NO=${monitor.toString} and S_DateTime >= ${start} and S_DateTime < ${end} and M_ITEM = ${mtStr}
      Order by S_DateTime
      """.map { rs =>
          val monitor = Monitor.withName(rs.string(1))
          val monitorType = MonitorType.withName(rs.string(2).replace("A4", "A2"))
          val startTime = rs.timestamp(3)
          val endTime = rs.timestamp(4)
          val span = rs.float(5)
          val z_std = rs.float(6)
          val z_val = rs.float(7)
          val zd_val = rs.float(8)
          val zd_pnt = rs.float(9)
          val s_std = rs.float(10)
          val s_sval = rs.float(11)
          val sd_val = rs.float(12)
          val sd_pnt = rs.float(13)
          CalibrationItem(monitor, monitorType, startTime, endTime, span, z_std, z_val, zd_val, zd_pnt, s_std, s_sval, sd_val, sd_pnt)
        }.list.apply

      }
    val pairs =
      for{r<-report}
    yield{
      r.startTime.toString("d")->r
    }
    Map(pairs :_*)
  }
}