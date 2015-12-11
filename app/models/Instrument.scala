package models
import scalikejdbc._
import com.github.nscala_time.time.Imports._
import models.ModelHelper._
import models._
/**
 * @author user
 */

case class H370Record(monitor: Monitor.Value, time: DateTime, thc_signal: Float, ch4_signal: Float,
                      zero_compression: Float, purifierTemp: Float, NMHCCOutTemp: Float, v5: Float, v24: Float, sampleFlow: Float, overFlow: Float,
                      pumpPress: Float, purifierPress: Float, atmosPress: Float)

case class T100Record(monitor: Monitor.Value, time: DateTime, range: Float, stabil: Float, stabil2: Float, press: Float, samp_fl: Float,
                      pmt: Float, norm_pmt: Float, uv_lamp: Float, uv_stb: Float, lamp_ratio: Float, str_lgt: Float, drk_pmt: Float, drk_lmp: Float,
                      slope: Float, offset: Float, hvps: Float, rcell_temp: Float, box_temp: Float, pmt_temp: Float, test: Float)

case class T200Record(monitor: Monitor.Value, time: DateTime, no: Float, nox: Float, range: Float, nox_stb: Float, samp_flw: Float, ozone_fl: Float, pmt: Float,
                      norm_pmt: Float, azero: Float, hvps: Float, rcell_temp: Float, box_temp: Float, pmt_temp: Float, moly_temp: Float,
                      rcel: Float, samp: Float, nox_slope: Float, nox_offs: Float, no_slope: Float, no_offs: Float)

case class T300Record(monitor: Monitor.Value, time: DateTime, range: Float, stabil: Float, co_meas: Float, co_ref: Float, mr_ratio: Float, pres: Float,
                      samp_fl: Float, sample_temp: Float, bench_temp: Float, wheel_temp: Float, box_temp: Float, pht_drive: Float, slope: Float, offset: Float)

case class T400Record(monitor: Monitor.Value, time: DateTime, range: Float, stabil: Float, o3_meas: Float, o3_ref: Float, pres: Float, samp_fl: Float,
                      sample_temp: Float, photo_lamp: Float, box_temp: Float, slope: Float, offset: Float)

case class PM10Record(monitor: Monitor.Value, time: DateTime, Conc:Option[Float], Qtot:Option[Float], RH:Option[Float], AT:Option[Float])

case class TSPRecord(monitor: Monitor.Value, time: DateTime, Conc:Option[Float], Qtot:Option[Float], RH:Option[Float], AT:Option[Float])

object Instrument extends Enumeration {
  val H370 = Value
  val T100 = Value
  val T200 = Value
  val T300 = Value
  val T400 = Value
  val PM10 = Value
  val TSP = Value

  
  def getTabName(inst: Instrument.Value, year: Int) = {
    SQLSyntax.createUnsafely(s"[AQMSDB].[dbo].[P1234567_Diag${inst.toString}_${year}]")
  }

  def getH370Record(monitor: Monitor.Value, start: DateTime, end: DateTime)(implicit session: DBSession = AutoSession) = {
    val tab = getTabName(H370, start.getYear)
    sql"""
      Select *
      From ${tab}
      Where DP_NO=${monitor.toString} and M_DateTime>= ${start} and M_DateTime<${end}
      """.map { rs =>
      val m = Monitor.withName(rs.string(1))
      val time = rs.timestamp(2)
      H370Record(m, time, rs.floatOpt(4).getOrElse(0f), rs.floatOpt(6).getOrElse(0f),
        rs.floatOpt(8).getOrElse(0f), rs.floatOpt(10).getOrElse(0f), rs.floatOpt(12).getOrElse(0f),
        rs.floatOpt(14).getOrElse(0f), rs.floatOpt(16).getOrElse(0f), rs.floatOpt(18).getOrElse(0f),
        rs.floatOpt(20).getOrElse(0f), rs.floatOpt(22).getOrElse(0f), rs.floatOpt(24).getOrElse(0f),
        rs.floatOpt(26).getOrElse(0f))
    }.list().apply()
  }

  def getT100Record(monitor: Monitor.Value, start: DateTime, end: DateTime)(implicit session: DBSession = AutoSession) = {
    val tab = getTabName(T100, start.getYear)
    sql"""
      Select *
      From ${tab}
      Where DP_NO=${monitor.toString} and M_DateTime>= ${start} and M_DateTime<${end}
      """.map { rs =>
      val m = Monitor.withName(rs.string(1))
      val time = rs.timestamp(2)
      T100Record(m, time, rs.floatOpt(4).getOrElse(0f), rs.floatOpt(6).getOrElse(0f),
        rs.floatOpt(8).getOrElse(0f), rs.floatOpt(10).getOrElse(0f), rs.floatOpt(12).getOrElse(0f),
        rs.floatOpt(14).getOrElse(0f), rs.floatOpt(16).getOrElse(0f), rs.floatOpt(18).getOrElse(0f),
        rs.floatOpt(20).getOrElse(0f), rs.floatOpt(22).getOrElse(0f), rs.floatOpt(24).getOrElse(0f),
        rs.floatOpt(26).getOrElse(0f), rs.floatOpt(28).getOrElse(0f), rs.floatOpt(30).getOrElse(0f),
        rs.floatOpt(32).getOrElse(0f), rs.floatOpt(34).getOrElse(0f), rs.floatOpt(36).getOrElse(0f),
        rs.floatOpt(38).getOrElse(0f), rs.floatOpt(40).getOrElse(0f), rs.floatOpt(42).getOrElse(0f))
    }.list().apply()
  }

  def getT200Record(monitor: Monitor.Value, start: DateTime, end: DateTime)(implicit session: DBSession = AutoSession) = {
    val tab = getTabName(T200, start.getYear)
    sql"""
      Select *
      From ${tab}
      Where DP_NO=${monitor.toString} and M_DateTime>= ${start} and M_DateTime<${end}
      """.map { rs =>
      val m = Monitor.withName(rs.string(1))
      val time = rs.timestamp(2)
      T200Record(m, time, rs.floatOpt(4).getOrElse(0f), rs.floatOpt(6).getOrElse(0f),
        rs.floatOpt(8).getOrElse(0f), rs.floatOpt(10).getOrElse(0f), rs.floatOpt(12).getOrElse(0f),
        rs.floatOpt(14).getOrElse(0f), rs.floatOpt(16).getOrElse(0f), rs.floatOpt(18).getOrElse(0f),
        rs.floatOpt(20).getOrElse(0f), rs.floatOpt(22).getOrElse(0f), rs.floatOpt(24).getOrElse(0f),
        rs.floatOpt(26).getOrElse(0f), rs.floatOpt(28).getOrElse(0f), rs.floatOpt(30).getOrElse(0f),
        rs.floatOpt(32).getOrElse(0f), rs.floatOpt(34).getOrElse(0f), rs.floatOpt(36).getOrElse(0f),
        rs.floatOpt(38).getOrElse(0f), rs.floatOpt(40).getOrElse(0f), rs.floatOpt(42).getOrElse(0f))
    }.list().apply()
  }

  def getT300Record(monitor: Monitor.Value, start: DateTime, end: DateTime)(implicit session: DBSession = AutoSession) = {
    val tab = getTabName(T300, start.getYear)
    sql"""
      Select *
      From ${tab}
      Where DP_NO=${monitor.toString} and M_DateTime>= ${start} and M_DateTime<${end}
      """.map { rs =>
      val m = Monitor.withName(rs.string(1))
      val time = rs.timestamp(2)
      T300Record(m, time, rs.floatOpt(4).getOrElse(0f), rs.floatOpt(6).getOrElse(0f),
        rs.floatOpt(8).getOrElse(0f), rs.floatOpt(10).getOrElse(0f), rs.floatOpt(12).getOrElse(0f),
        rs.floatOpt(16).getOrElse(0f), rs.floatOpt(18).getOrElse(0f), rs.floatOpt(20).getOrElse(0f),
        rs.floatOpt(22).getOrElse(0f), rs.floatOpt(24).getOrElse(0f), rs.floatOpt(26).getOrElse(0f),
        rs.floatOpt(30).getOrElse(0f), rs.floatOpt(32).getOrElse(0f), rs.floatOpt(34).getOrElse(0f))
    }.list().apply()
  }

  def getT400Record(monitor: Monitor.Value, start: DateTime, end: DateTime)(implicit session: DBSession = AutoSession) = {
    val tab = getTabName(T400, start.getYear)
    sql"""
      Select *
      From ${tab}
      Where DP_NO=${monitor.toString} and M_DateTime>= ${start} and M_DateTime<${end}
      """.map { rs =>
      val m = Monitor.withName(rs.string(1))
      val time = rs.timestamp(2)
      T400Record(m, time, rs.floatOpt(4).getOrElse(0f), rs.floatOpt(6).getOrElse(0f),
        rs.floatOpt(8).getOrElse(0f), rs.floatOpt(10).getOrElse(0f), rs.floatOpt(12).getOrElse(0f),
        rs.floatOpt(14).getOrElse(0f), rs.floatOpt(16).getOrElse(0f), rs.floatOpt(18).getOrElse(0f),
        rs.floatOpt(20).getOrElse(0f), rs.floatOpt(22).getOrElse(0f), rs.floatOpt(24).getOrElse(0f))
    }.list().apply()
  }
  
  def getPM10Record(monitor: Monitor.Value, start: DateTime, end: DateTime)(implicit session: DBSession = AutoSession) = {
    val tab = getTabName(PM10, start.getYear)
    sql"""
      Select *
      From ${tab}
      Where DP_NO=${monitor.toString} and M_DateTime>= ${start} and M_DateTime<${end}
      """.map { rs =>
      val m = Monitor.withName(rs.string(1))
      val time = rs.timestamp(2)
      PM10Record(m, time, rs.floatOpt(4), rs.floatOpt(6),
        rs.floatOpt(8), rs.floatOpt(10))
    }.list().apply()
  }
  
  def getTSPRecord(monitor: Monitor.Value, start: DateTime, end: DateTime)(implicit session: DBSession = AutoSession) = {
    val tab = getTabName(TSP, start.getYear)
    sql"""
      Select *
      From ${tab}
      Where DP_NO=${monitor.toString} and M_DateTime>= ${start} and M_DateTime<${end}
      """.map { rs =>
      val m = Monitor.withName(rs.string(1))
      val time = rs.timestamp(2)
      TSPRecord(m, time, rs.floatOpt(4), rs.floatOpt(6), rs.floatOpt(8), rs.floatOpt(10))
    }.list().apply()
  }
}