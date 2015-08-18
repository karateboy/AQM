package models
import java.sql.Date
import java.sql.Timestamp
import scalikejdbc._
import play.api._
import com.github.nscala_time.time.Imports._
import models.ModelHelper._
import models._
import Record._
import MonitorType._

/**
 * @author user
 */
class AuditStat(hr: HourRecord) {
  val name = hr.name
  val date = hr.date
  var chk = hr.chk
  var tsp_stat = hr.tsp_stat
  var pm10_stat = hr.pm10_stat
  var pm25_stat = hr.pm25_stat
  var s_stat = hr.s_stat
  var so2_stat = hr.so2_stat
  var nox_stat = hr.nox_stat
  var co_stat = hr.co_stat
  var o3_stat = hr.o3_stat
  var thc_stat = hr.thc_stat
  var ammonia_stat = hr.ammonia_stat
  var noy_stat = hr.noy_stat
  var noy_no_stat = hr.noy_no_stat
  var nh3_stat = hr.nh3_stat
  var no_stat = hr.no_stat
  var ch4_stat = hr.ch4_stat
  var monitor_humid_stat = hr.monitor_humid_stat
  var monitor_temp_stat = hr.monitor_temp_stat
  var no2_stat = hr.no2_stat
  var nmhc_stat = hr.nmhc_stat
  var wind_speed_stat = hr.wind_speed_stat
  var wind_dir_stat = hr.wind_dir_stat
  var rain_stat = hr.rain_stat
  var temp_stat = hr.temp_stat
  var humid_stat = hr.temp_stat
  var air_pressure_stat = hr.air_pressure_stat

  def getStat(mt: MonitorType.Value) = {
    mt match {
      case A213 => tsp_stat
      case A214 => pm10_stat
      case A215 => pm25_stat
      case A221 => s_stat
      case A222 => so2_stat
      case A223 => nox_stat
      case A224 => co_stat
      case A225 => o3_stat
      case A226 => thc_stat
      case A229 => ammonia_stat
      case A232 => noy_stat
      case A233 => noy_no_stat
      case A235 => nh3_stat
      case A283 => no_stat
      case A286 => ch4_stat
      case A288 => monitor_humid_stat
      case A289 => monitor_temp_stat
      case A293 => no2_stat
      case A296 => nmhc_stat
      case C211 => wind_speed_stat
      case C212 => wind_dir_stat
      case C213 => rain_stat
      case C214 => temp_stat
      case C215 => humid_stat
      case C216 => air_pressure_stat
    }
  }

  def setStat(mt: MonitorType.Value, stat: String) = {
    mt match {
      case A213 => tsp_stat = Some(stat)
      case A214 => pm10_stat = Some(stat)
      case A215 => pm25_stat = Some(stat)
      case A221 => s_stat = Some(stat)
      case A222 => so2_stat = Some(stat)
      case A223 => nox_stat = Some(stat)
      case A224 => co_stat = Some(stat)
      case A225 => o3_stat = Some(stat)
      case A226 => thc_stat = Some(stat)
      case A229 => ammonia_stat = Some(stat)
      case A232 => noy_stat = Some(stat)
      case A233 => noy_no_stat = Some(stat)
      case A235 => nh3_stat = Some(stat)
      case A283 => no_stat = Some(stat)
      case A286 => ch4_stat = Some(stat)
      case A288 => monitor_humid_stat = Some(stat)
      case A289 => monitor_temp_stat = Some(stat)
      case A293 => no2_stat = Some(stat)
      case A296 => nmhc_stat = Some(stat)
      case C211 => wind_speed_stat = Some(stat)
      case C212 => wind_dir_stat = Some(stat)
      case C213 => rain_stat = Some(stat)
      case C214 => temp_stat = Some(stat)
      case C215 => humid_stat = Some(stat)
      case C216 => air_pressure_stat = Some(stat)
    }
  }

  def clear() = {
    chk = None
    for (mt <- MonitorType.mtvAllList) {
      val stat = getStat(mt)
      if (stat.isDefined && MonitorStatus.getTagInfo(stat.get).statusType == StatusType.Auto) {
        setStat(mt, MonitorStatus.NORMAL_STAT)
      }
    }
    updateDB
  }

  private def statT(stat: Option[String]) = {
    stat
    /*
    if (stat.isEmpty)
      "NULL"
    else
      stat.get
      * 
      */
  }

  def setAuditStat(mt: MonitorType.Value, mask: Int) {
    val old_stat = getStat(mt)
    if (old_stat.isEmpty)
      return

    val tagInfo = MonitorStatus.getTagInfo(old_stat.get)

    tagInfo.statusType match {
      case StatusType.Internal =>
        val tag = MonitorStatus.getAutoAuditTagStr(mask)
        setStat(mt, tag)
      case StatusType.Auto =>
        val old_mask = Integer.parseInt(tagInfo.id, 16)
        val new_mask = old_mask | mask
        val tag = MonitorStatus.getAutoAuditTagStr(new_mask)
        setStat(mt, tag)
      case StatusType.Manual =>
    }
  }

  def updateDB()(implicit session: DBSession = AutoSession) = {
    val tab_name = Record.getTabName(TableType.Hour, date.toDateTime().getYear)
    sql"""
    Update ${tab_name}
     Set  [CHK] = ${statT(chk)} 
          ,[A513S] = ${statT(tsp_stat)}
          ,[A514S] = ${statT(pm10_stat)}
          ,[A515S] = ${statT(pm25_stat)}
          ,[A521S] = ${statT(s_stat)}
          ,[A522S] = ${statT(so2_stat)}
          ,[A523S] = ${statT(nox_stat)}
          ,[A524S] = ${statT(co_stat)}
          ,[A525S] = ${statT(o3_stat)}
          ,[A526S] = ${statT(thc_stat)}
          ,[A529S] = ${statT(ammonia_stat)}
          ,[A532S] = ${statT(noy_stat)}
          ,[A533S] = ${statT(noy_no_stat)}
          ,[A535S] = ${statT(nh3_stat)}
          ,[A583S] = ${statT(no_stat)}
          ,[A586S] = ${statT(ch4_stat)}
          ,[A588S] = ${statT(monitor_humid_stat)}
          ,[A589S] = ${statT(monitor_temp_stat)}
          ,[A593S] = ${statT(no2_stat)}
          ,[A596S] = ${statT(nmhc_stat)}
          ,[C511S] = ${statT(wind_speed_stat)}
          ,[C512S] = ${statT(wind_dir_stat)}
          ,[C513S] = ${statT(rain_stat)}
          ,[C514S] = ${statT(temp_stat)}
          ,[C515S] = ${statT(humid_stat)}
          ,[C516S] = ${statT(air_pressure_stat)}
        Where DP_NO=${name} and M_DateTime = ${date}        
      """.update.apply
  }
}

object Auditor {
  def getUnauditedRecords(monitor: Monitor.Value, startTime: DateTime, endTime: DateTime)(implicit session: DBSession = AutoSession) = {
    val start: Timestamp = startTime
    val end: Timestamp = endTime
    val monitorName = monitor.toString()
    val tab_name = Record.getTabName(TableType.Hour, startTime.getYear)
    sql"""
        Select * 
        From ${tab_name}
        Where DP_NO=${monitorName} and M_DateTime >= ${start} and M_DateTime < ${end} and CHK is Null
        ORDER BY M_DateTime ASC
      """.map { Record.mapper }.list().apply()
  }

  def auditHourData(monitor: Monitor.Value, auditConfig: AutoAudit, start: DateTime, end: DateTime)(implicit session: DBSession = AutoSession) = {
    val prestart = auditConfig.persistenceRule.same.hours
    //val records = getUnauditedRecords(monitor, start - prestart, end + 1.hour).toArray
    def clearAuditData()={
      val records = getHourRecords(monitor, start - prestart, end + 1.hour).toArray
      val auditStatList = records.map { new AuditStat(_) }
      auditStatList.foreach { _.clear}
    }
    
    clearAuditData()
    val records = getHourRecords(monitor, start - prestart, end + 1.hour).toArray
    def isOk(r: (Option[Float], Option[String])) = {
      r._1.isDefined && r._2.isDefined && 
      (MonitorStatus.isNormalStat(r._2.get)|| MonitorStatus.getTagInfo(r._2.get).statusType == StatusType.Auto )
    }

    val mtAvgStdPairs =
      for {
        mt <- auditConfig.differenceRule.monitorTypes
        mt_records = records.map { Record.monitorTypeProject2(mt) }.filter(isOk).map { r => r._1.get } if (mt_records.length != 0)
      } yield {
        val count = mt_records.length
        val avg = mt_records.sum / count
        val std = Math.sqrt(mt_records.map { r => (r - avg) * (r - avg) }.sum / count)
        mt -> (avg, std)
      }

    val avgStdMap = Map(mtAvgStdPairs: _*)

    for {
      hr <- records.zipWithIndex if (hr._2 >= auditConfig.persistenceRule.same && hr._2 < records.length - 1)
      record = hr._1
      idx = hr._2
      targetStat = new AuditStat(record)
    } {
      var invalid = false      
      if (auditConfig.minMaxRule.enabled) {
        Logger.debug("minMax checking")
        for (cfg <- auditConfig.minMaxRule.monitorTypes) {
          val mt = cfg.id
          val mtRecord = Record.monitorTypeProject2(mt)(record)
          //Only check for normal record
          if (isOk(mtRecord)) {
              val mt_value = mtRecord._1.get
                            
              if (mt_value > cfg.max || mt_value <= cfg.min) {
                invalid = true
                targetStat.setAuditStat(mt, auditConfig.minMaxRule.mask)
              }
          }
        }
      }

      if (auditConfig.compareRule.enabled) {
        Logger.debug("compareRule checking")
        val thc_rec = Record.monitorTypeProject2(A226)(record)
        val ch4_rec = Record.monitorTypeProject2(A286)(record)
        if (isOk(thc_rec) && isOk(ch4_rec)) {
          val thc = thc_rec._1.get
          val ch4 = ch4_rec._1.get
          if (ch4 < thc) {
            invalid = true
            targetStat.setAuditStat(A226, auditConfig.compareRule.mask)
            targetStat.setAuditStat(A286, auditConfig.compareRule.mask)
          }
        }

        val nox_rec = Record.monitorTypeProject2(A223)(record)
        val no2_rec = Record.monitorTypeProject2(A293)(record)
        if (isOk(nox_rec) && isOk(no2_rec)) {
          val nox = nox_rec._1.get
          val no2 = no2_rec._1.get
          if (nox < no2) {
            invalid = true
            targetStat.setAuditStat(A223, auditConfig.compareRule.mask)
            targetStat.setAuditStat(A293, auditConfig.compareRule.mask)
          }
        }
      }

      if (auditConfig.differenceRule.enabled) {
        Logger.debug("differenceRule checking")
        for {
          mt <- auditConfig.differenceRule.monitorTypes
          mr_record = Record.monitorTypeProject2(mt)(record) if (isOk(mr_record))
        } {
          val v = mr_record._1.get
          val (avg, std) = avgStdMap(mt)
          if (Math.abs(v - avg) > auditConfig.differenceRule.multiplier * std) {
            invalid = true
            targetStat.setAuditStat(mt, auditConfig.differenceRule.mask)
          }
        }
      }

      if (auditConfig.persistenceRule.enabled) {
        Logger.debug("persistenceRule checking")
        if (idx > auditConfig.persistenceRule.same) {
          for (mt <- MonitorType.mtvAllList) {
            val mt_rec = Record.monitorTypeProject2(mt)(record)
            if (isOk(mt_rec)) {
              val end_idx = idx - 1
              def testSame(i: Int): Boolean = {
                if (i >= end_idx)
                  return false

                val test_rec = Record.monitorTypeProject2(mt)(records(i))
                if (!isOk(test_rec))
                  return false

                val v = test_rec._1.get
                if (v != mt_rec._1.get)
                  return false

                return testSame(i + 1)
              }

              if (testSame(idx - auditConfig.persistenceRule.same + 1)) {
                invalid = true
                targetStat.setAuditStat(mt, auditConfig.persistenceRule.mask)
              }
            }
          }
        }
      }

      if (auditConfig.spikeRule.enabled) {
        Logger.debug("spikeRule checking")
        for (mtcfg <- auditConfig.spikeRule.monitorTypes) {
          val mt_rec = Record.monitorTypeProject2(mtcfg.id)(record)
          if (isOk(mt_rec)) {
            val pre_idx = idx - 1
            val post_idx = idx + 1
            if (pre_idx >= 0 && post_idx < records.length) {
              val pre = Record.monitorTypeProject2(mtcfg.id)(records(pre_idx))
              val post = Record.monitorTypeProject2(mtcfg.id)(records(post_idx))
              if (isOk(pre) && isOk(post)) {
                val avg = (pre._1.get + post._1.get) / 2
                val v = mt_rec._1.get
                if (Math.abs(v - avg) > mtcfg.abs) {
                  invalid = true
                  targetStat.setAuditStat(mtcfg.id, auditConfig.spikeRule.mask)
                }
              }
            }
          }
        }
      }

      //Save
      if(invalid)
        targetStat.chk = Some("BAD")
      else
        targetStat.chk = Some("OK")
       
      Logger.debug("chk="+targetStat.chk.toString())
      targetStat.updateDB
    }
  }

}