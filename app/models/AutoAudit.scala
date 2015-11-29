package models
import play.api.libs.json._
import play.api.libs.functional.syntax._
import scalikejdbc._
import scalikejdbc.config._
import Record._
import Auditor._
import MonitorType._
import com.github.nscala_time.time.Imports._

abstract class Rule(val lead:Char)

case class MinMaxCfg(
  id:MonitorType.Value,
  min:Float,
  max:Float
)

case class MinMaxRule (
  enabled:Boolean,
  monitorTypes:Seq[MinMaxCfg]    
)extends Rule('a'){
  def checkInvalid(record: HourRecord, targetStat: AuditStat):Boolean = {
    if(!enabled)
      return false
      
    var invalid = false
    for (cfg <- monitorTypes) {
      val mt = cfg.id
      val mtRecord = Record.monitorTypeProject2(mt)(record)

      if (Auditor.isOk(mtRecord)) {
        val mt_value = mtRecord._1.get

        if (mt_value > cfg.max || mt_value <= cfg.min) {
          targetStat.setAuditStat(mt, lead)
          invalid = true
        }
      }
    }
    invalid
  }
}


object MinMaxRule{
  implicit val minMaxCfgRead = Json.reads[MinMaxCfg]
  implicit val minMaxCfgWrite = Json.writes[MinMaxCfg]
  implicit val minMaxRuleWrite = Json.writes[MinMaxRule]
  implicit val minMaxRuleRead = Json.reads[MinMaxRule]

  val default = MinMaxRule(false, Seq())
}

case class CompareRule(
  enabled:Boolean    
)extends Rule('b'){
  def checkInvalid(record: HourRecord, targetStat: AuditStat): Boolean = {
    if (!enabled)
      return false

    var invalid = false
    val thc_rec = Record.monitorTypeProject2(A226)(record)
    val ch4_rec = Record.monitorTypeProject2(A286)(record)
    if (isOk(thc_rec) && isOk(ch4_rec)) {
      val thc = thc_rec._1.get
      val ch4 = ch4_rec._1.get
      if (ch4 < thc) {
        invalid = true
        targetStat.setAuditStat(A226, lead)
        targetStat.setAuditStat(A286, lead)
      }
    }

    val nox_rec = Record.monitorTypeProject2(A223)(record)
    val no2_rec = Record.monitorTypeProject2(A293)(record)
    if (isOk(nox_rec) && isOk(no2_rec)) {
      val nox = nox_rec._1.get
      val no2 = no2_rec._1.get
      if (nox < no2) {
        invalid = true
        targetStat.setAuditStat(A223, lead)
        targetStat.setAuditStat(A293, lead)
      }
    }
    val pm25 = Record.monitorTypeProject2(A215)(record)
    val pm10 = Record.monitorTypeProject2(A214)(record)
    val tsp = Record.monitorTypeProject2(A213)(record)
    if(isOk(tsp) && isOk(pm10)){
      if(pm10._1.get > tsp._1.get){
        invalid = true
        targetStat.setAuditStat(A214, lead)
        targetStat.setAuditStat(A213, lead)
      }
    }
    if(isOk(pm25) && isOk(pm10)){
      if(pm25._1.get > pm10._1.get){
        invalid = true
        targetStat.setAuditStat(A214, lead)
        targetStat.setAuditStat(A215, lead)
      }
    }
    invalid
  }
}

object CompareRule{
  implicit val compareRuleRead = Json.reads[CompareRule]
  implicit val compareRuleWrite = Json.writes[CompareRule]
  
  val default = CompareRule(false)
}

case class DifferenceRule(
  enabled:Boolean,
  multiplier:Float,
  monitorTypes:Seq[MonitorType.Value]  
)extends Rule('c'){
  def checkInvalid(record: HourRecord, targetStat: AuditStat, monitor:Monitor.Value, start: DateTime): Boolean = {
    if (!enabled)
      return false

    var invalid = false
    val records = getHourRecords(monitor, start - 24.hour, start).toArray
    val mtAvgStdPairs =
      for {
        mt <- monitorTypes
        mt_records = records.map { Record.monitorTypeProject2(mt) }.filter(isOk).map { r => r._1.get } if (mt_records.length != 0)
      } yield {
        val count = mt_records.length
        val avg = mt_records.sum / count
        val std = Math.sqrt(mt_records.map { r => (r - avg) * (r - avg) }.sum / count)
        mt -> (avg, std)
      }

    val avgStdMap = Map(mtAvgStdPairs: _*)

    for {
      mt <- monitorTypes
      mr_record = Record.monitorTypeProject2(mt)(record) if (isOk(mr_record))
    } {
      val v = mr_record._1.get
      val (avg, std) = avgStdMap(mt)
      if (Math.abs(v - avg) > multiplier * std) {
        invalid = true
        targetStat.setAuditStat(mt, lead)
      }
    }

    invalid
  }
}

object DifferenceRule{
  implicit val differenceRuleRead = Json.reads[DifferenceRule]
  implicit val differenceRuleWrite = Json.writes[DifferenceRule]
  
  val default = DifferenceRule(false, 3, Seq())
}

case class SpikeCfg(
  id:MonitorType.Value,
  abs:Float
)
case class SpikeRule(
  enabled:Boolean,
  monitorTypes:Seq[SpikeCfg]
)extends Rule('d'){
  def checkInvalid(record: HourRecord, targetStat: AuditStat, monitor: Monitor.Value, start: DateTime): Boolean = {
    if (!enabled)
      return false

    var invalid = false
    val pre_records = getHourRecords(monitor, start - 2.hour, start).toList
    for (mtcfg <- monitorTypes) {
      val mt_rec = Record.monitorTypeProject2(mtcfg.id)(record)
      val pre_mt_rec = pre_records.map(Record.monitorTypeProject2(mtcfg.id)).filter(isOk)
      if (isOk(mt_rec) && pre_mt_rec.length == 2) {
        val avg = (pre_mt_rec(0)._1.get + mt_rec._1.get) / 2
        val v = pre_mt_rec(1)._1.get
        if (Math.abs(v - avg) > mtcfg.abs) {
          invalid = true
          targetStat.setAuditStat(mtcfg.id, lead)
        }
      }
    }

    invalid
  }
}

object SpikeRule{
  implicit val spikeCfgRead = Json.reads[SpikeCfg]
  implicit val spikeRuleRead = Json.reads[SpikeRule]
  implicit val spikeCfgWrite = Json.writes[SpikeCfg]
  implicit val spikeRuleWrite = Json.writes[SpikeRule]
  
  val default = SpikeRule(false, Seq())
}

case class PersistenceRule(
  enabled:Boolean,
  same:Int
)extends Rule('e'){
  def checkInvalid(record: HourRecord, targetStat: AuditStat, monitor:Monitor.Value, start: DateTime): Boolean = {
    if (!enabled)
      return false

    var invalid = false
    val pre_records = getHourRecords(monitor, start - (same-1).hour, start).toArray
    
    for (mt <- MonitorType.mtvAllList) {
      val mt_rec = Record.monitorTypeProject2(mt)(record)
      if (isOk(mt_rec)) {
        val pre_mt_rec = pre_records.map(Record.monitorTypeProject2(mt)).filter(isOk).filter(r=>r._1.get == mt_rec._1.get)
        if (pre_mt_rec.length == same-1) {
          invalid = true
          targetStat.setAuditStat(mt, lead)
        }
      }
    }

    invalid
  }
}

object PersistenceRule{
  implicit val persistenceRuleRead = Json.reads[PersistenceRule]
  implicit val persistenceRuleWrite = Json.writes[PersistenceRule]
  
  val default = PersistenceRule(false, 3)
}

case class MonoCfg(
    id:MonitorType.Value,
    abs:Float
)

case class MonoRule(enabled:Boolean, count:Int, 
    monitorTypes:Seq[MonoCfg])extends Rule('f'){
  def checkInvalid(record: HourRecord, targetStat: AuditStat, monitor: Monitor.Value, start: DateTime): Boolean = {
    if (!enabled)
      return false

    var invalid = false
    val pre_records = getHourRecords(monitor, start - (count-1).hour, start).toList

    for (mtcfg <- monitorTypes) {
      val mt_rec = Record.monitorTypeProject2(mtcfg.id)(record)
      val pre_rec = pre_records.map(Record.monitorTypeProject2(mtcfg.id)).filter(isOk)
      if (isOk(mt_rec) && pre_rec.length == count-1) {
        val values = pre_rec.map(_._1.get) ::: List(mt_rec._1.get)
        val max = values.max
        val min = values.min
        if ((max - min) < mtcfg.abs) {
          invalid = true
          targetStat.setAuditStat(mtcfg.id, lead)
        }
      }
    }

    invalid
  }
}

object MonoRule{
  implicit val monoCfgRead = Json.reads[MonoCfg]
  implicit val monoCfgWrite = Json.writes[MonoCfg]
  implicit val monoRuleRead = Json.reads[MonoRule]
  implicit val monoRuleWrite = Json.writes[MonoRule]
  
  val default = MonoRule(false, 3, Seq.empty[MonoCfg])
  
}

case class TwoHourRule(enabled: Boolean, monitorTypes: Seq[MonoCfg]) extends Rule('g') {
  def checkInvalid(record: HourRecord, targetStat: AuditStat, monitor: Monitor.Value, start: DateTime): Boolean = {
    if (!enabled)
      return false

    var invalid = false
    val pre_records = getHourRecords(monitor, start - 1.hour, start).toList

    for (mtcfg <- monitorTypes) {
      val mt_rec = Record.monitorTypeProject2(mtcfg.id)(record)
      val pre_rec = pre_records.map(Record.monitorTypeProject2(mtcfg.id)).filter(isOk)
      if (isOk(mt_rec) && pre_rec.length == 1) {
        if (Math.abs(pre_rec(0)._1.get - mt_rec._1.get) > mtcfg.abs) {
          invalid = true
          targetStat.setAuditStat(mtcfg.id, lead)
        }
      }
    }

    invalid
  }
}

object TwoHourRule{
  import MonoRule._
  implicit val read = Json.reads[TwoHourRule]
  implicit val write = Json.writes[TwoHourRule]
  val default = TwoHourRule(false, Seq.empty[MonoCfg])
}

case class ThreeHourCfg(
    id:MonitorType.Value,
    abs:Float,
    percent:Float
)
case class ThreeHourRule(enabled:Boolean, monitorTypes:Seq[ThreeHourCfg])extends Rule('h') {
  def checkInvalid(record: HourRecord, targetStat: AuditStat, monitor: Monitor.Value, start: DateTime): Boolean = {
    if (!enabled)
      return false

    var invalid = false
    val pre_records = getHourRecords(monitor, start - 2.hour, start).toList

    for (mtcfg <- monitorTypes) {
      val mt_rec = Record.monitorTypeProject2(mtcfg.id)(record)
      val pre_rec = pre_records.map(Record.monitorTypeProject2(mtcfg.id)).filter(isOk)
      if (isOk(mt_rec) && pre_rec.length == 2) {
        val values = pre_rec.map(_._1.get) ::: List(mt_rec._1.get)
        val abs_percent =
          for(v1 <- values.zipWithIndex.dropRight(1))
            yield{
            val v2 = values(v1._2)
            (Math.abs(v1._1 - v2), Math.abs((1 - v1._1/v2)*100))
          }
        val overs = abs_percent.filter(v => v._1 > mtcfg.abs && v._2> mtcfg.percent)
        if (overs.length == 2) {
          invalid = true
          targetStat.setAuditStat(mtcfg.id, lead)
        }
      }
    }

    invalid
  }
}
object ThreeHourRule{
  implicit val thcfgRead = Json.reads[ThreeHourCfg]
  implicit val thcfgWrite = Json.writes[ThreeHourCfg]
  implicit val reads = Json.reads[ThreeHourRule]
  implicit val writes = Json.writes[ThreeHourRule]
  
  val default = ThreeHourRule(false, Seq.empty[ThreeHourCfg])
}

case class FourHourCfg(
    id:MonitorType.Value,
    abs:Float)

case class FourHourRule(enabled:Boolean, monitorTypes:Seq[FourHourCfg])extends Rule('i') {
  def checkInvalid(record: HourRecord, targetStat: AuditStat, monitor: Monitor.Value, start: DateTime): Boolean = {
    if (!enabled)
      return false

    var invalid = false
    val pre_records = getHourRecords(monitor, start - 3.hour, start).toList

    for (mtcfg <- monitorTypes) {
      val mt_rec = Record.monitorTypeProject2(mtcfg.id)(record)
      val pre_rec = pre_records.map(Record.monitorTypeProject2(mtcfg.id)).filter(isOk)
      if (isOk(mt_rec) && pre_rec.length == 3) {
        val values = pre_rec.map(_._1.get) ::: List(mt_rec._1.get)
        val avg = values.sum/4
        if (avg > mtcfg.abs) {
          invalid = true
          targetStat.setAuditStat(mtcfg.id, lead)
        }
      }
    }

    invalid
  }
}
object FourHourRule{
  implicit val thcfgRead = Json.reads[FourHourCfg]
  implicit val thcfgWrite = Json.writes[FourHourCfg]
  implicit val reads = Json.reads[FourHourRule]
  implicit val writes = Json.writes[FourHourRule]
  
  val default = FourHourRule(false, Seq.empty[FourHourCfg])
}
case class AutoAudit(
    minMaxRule:MinMaxRule,
    compareRule:CompareRule,
    differenceRule:DifferenceRule,
    spikeRule:SpikeRule,
    persistenceRule:PersistenceRule,
    monoRule:MonoRule,
    twoHourRule:TwoHourRule,
    threeHourRule:ThreeHourRule,
    fourHourRule:FourHourRule
)

/**
 * @author user
 */
object AutoAudit {
  implicit val autoAuditRead = Json.reads[AutoAudit]
  implicit val autoAuditWrite = Json.writes[AutoAudit]
  
  val default = AutoAudit(
      MinMaxRule.default, 
      CompareRule.default, 
      DifferenceRule.default,
      SpikeRule.default,
      PersistenceRule.default,
      MonoRule.default,
      TwoHourRule.default,
      ThreeHourRule.default,
      FourHourRule.default) 
      
  val map= Map(
    'a' -> "極大極小值",
    'b' -> "合理性",
    'c' -> "單調性",
    'd' -> "突波高值",
    'e' -> "持續性",
    'f' -> "一致性",
    'g' -> "小時測值變化驗證",
    'h' -> "三小時變化測值驗證",
    'i' -> "四小時變化測值驗證"
  )
}