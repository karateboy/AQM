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
  
  var h2s_stat = hr.h2s_stat
  var noy_dif_stat = hr.noy_dif_stat
  var nh3_nt_stat = hr.nh3_nt_stat
  var nh3_nox_stat = hr.nh3_nox_stat
  var nh3_no_stat = hr.nh3_no_stat
  var nh3_no2_stat = hr.nh3_no2_stat
  var h2s_cs_stat = hr.h2s_cs_stat
  var h2s_so2_stat = hr.h2s_so2_stat
  
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
      
      //New MT
      case A234 => h2s_stat
      case A242 => noy_dif_stat
      case A236 => nh3_nt_stat
      case A237 => nh3_nox_stat
      case A238 => nh3_no_stat
      case A239 => nh3_no2_stat
      case A244 => h2s_cs_stat
      case A245 => h2s_so2_stat
      
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
            
      //New MT
      case A234 => h2s_stat = Some(stat)
      case A242 => noy_dif_stat = Some(stat)
      case A236 => nh3_nt_stat = Some(stat)
      case A237 => nh3_nox_stat = Some(stat)
      case A238 => nh3_no_stat = Some(stat)
      case A239 => nh3_no2_stat = Some(stat)
      case A244 => h2s_cs_stat = Some(stat)
      case A245 => h2s_so2_stat = Some(stat)
      
    }
  }

  def clear() = {
    chk = None
    for (mt <- MonitorType.mtvAllList) {
      val stat = getStat(mt)
      if (stat.isDefined && MonitorStatus.getTagInfo(stat.get).statusType == StatusType.Auto) {
        val internalStatus = MonitorStatus.switchTagToInternal(stat.get)
        setStat(mt, internalStatus)
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

  def setAuditStat(mt: MonitorType.Value, lead: Char) {
    val old_stat = getStat(mt)
    if (old_stat.isEmpty)
      return

    val tagInfo = MonitorStatus.getTagInfo(old_stat.get)

    if (tagInfo.statusType == StatusType.Internal) {
      val autoAsNormal = SystemConfig.getConfig(SystemConfig.AutoAuditAsNormal, "True").toBoolean
      val l =
        if (autoAsNormal)
          lead.toLower
        else
          lead.toUpper

      setStat(mt, l + tagInfo.id)

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
  def clearAuditData(records: List[HourRecord]) = {
    val auditStatList = records.map { new AuditStat(_) }
    auditStatList.foreach { _.clear }
  }

  def isOk(r: (Option[Float], Option[String])) = {
    r._1.isDefined && r._2.isDefined &&
      (MonitorStatus.isNormalStat(r._2.get) || MonitorStatus.getTagInfo(r._2.get).statusType == StatusType.Auto)
  }
      
  def auditHourData(monitor: Monitor.Value, auditConfig: AutoAudit, start: DateTime, end: DateTime, reaudit:Boolean = false)(implicit session: DBSession = AutoSession) = {
    val records =
      if(reaudit)
        getHourRecords(monitor, start, end).toArray
      else
        getUncheckedHourRecords(monitor, start, end).toArray
      

    for {
      hr <- records.zipWithIndex
      record = hr._1
      idx = hr._2
      targetStat = {
        if(reaudit){
          val as = new AuditStat(record)
          as.clear()
          as
        }else
          new AuditStat(record)}
    } {
      var invalid = false
      if(auditConfig.minMaxRule.checkInvalid(record, targetStat))
        invalid = true

      if(auditConfig.compareRule.checkInvalid(record, targetStat))
        invalid = true

      if(auditConfig.differenceRule.checkInvalid(record, targetStat, monitor, start))
        invalid = true

      if(auditConfig.persistenceRule.checkInvalid(record, targetStat, monitor, start))
        invalid = true
      

      if(auditConfig.spikeRule.checkInvalid(record, targetStat, monitor, start))
        invalid = true

      if(auditConfig.twoHourRule.checkInvalid(record, targetStat, monitor, start))
        invalid = true
        
      if(auditConfig.threeHourRule.checkInvalid(record, targetStat, monitor, start))
        invalid = true
        
      if(auditConfig.fourHourRule.checkInvalid(record, targetStat, monitor, start))
        invalid = true
        
      if(auditConfig.monoRule.checkInvalid(record, targetStat, monitor, start))
        invalid = true
        
      //Save
      if (invalid)
        targetStat.chk = Some("BAD")
      else
        targetStat.chk = Some("OK")

      targetStat.updateDB
    }
  }

}