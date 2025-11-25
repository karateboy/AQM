package models

import com.github.nscala_time.time.Imports._
import models.Auditor._
import models.ModelHelper._
import models.MonitorType._
import models.Record._
import play.api.libs.json._

abstract class Rule(val lead: Char)

case class MinMaxCfg(
                      id: MonitorType.Value,
                      min: Float,
                      max: Float)

case class MinMaxRule(
                       enabled: Boolean = false,
                       autoTicket: Option[Boolean] = None,
                       monitorTypes: Seq[MinMaxCfg] = Seq.empty[MinMaxCfg]) extends Rule('a') {
  def checkInvalid(record: HourRecord, targetStat: AuditStat): Boolean = {
    if (!enabled)
      return false

    var invalid = false
    for (cfg <- monitorTypes) {
      val mt = cfg.id
      val mtRecord = Record.monitorTypeProject2(mt)(record)

      if (Auditor.isOk(mtRecord)) {
        val mt_value = mtRecord._1.get

        if (mt_value > cfg.max || mt_value <= cfg.min) {
          targetStat.setAuditStat(mt, lead)
          val ar = Alarm.Alarm(Monitor.withName(record.name), MonitorType.map(mt).id, record.date, 1.0f, lead + "10")
          try {
            Alarm.insertAlarm(ar)
            val m = Monitor.withName(record.name)
            for (autoTicket <- Monitor.map(m).autoAudit.minMaxRule.autoTicket if autoTicket)
              Alarm.newTicketFromAlarm(ar, DateTime.now().plusDays(2))
          } catch {
            case ex: Exception =>
            // Skip duplicate alarm
          }
          invalid = true
        }
      }
    }
    invalid
  }
}

object MinMaxRule {
  implicit val minMaxCfgRead = Json.reads[MinMaxCfg]
  implicit val minMaxCfgWrite = Json.writes[MinMaxCfg]
  implicit val minMaxRuleWrite = Json.writes[MinMaxRule]
  implicit val minMaxRuleRead = Json.reads[MinMaxRule]

}

case class CompareRule(
                        enabled: Boolean = false,
                        autoTicket: Option[Boolean] = None,
                        thc: Option[Boolean] = Some(true),
                        nox: Option[Boolean] = Some(true),
                        pm10: Option[Boolean] = Some(true),
                        humid: Option[Boolean] = Some(true),
                        thc2: Option[Boolean] = Some(true),
                        nox2: Option[Boolean] = Some(true),
                        pm102: Option[Boolean] = Some(true),
                        humid2: Option[Boolean] = Some(true)) extends Rule('b') {
  def checkInvalid(record: HourRecord, targetStat: AuditStat): Boolean = {
    if (!enabled)
      return false

    var invalid = false
    val thc_rec = Record.monitorTypeProject2(A226)(record)
    val ch4_rec = Record.monitorTypeProject2(A286)(record)
    if ((thc.getOrElse(true) || thc2.getOrElse(true)) && isOk(thc_rec) && isOk(ch4_rec)) {
      val thcValue = thc_rec._1.get
      val ch4 = ch4_rec._1.get
      if (ch4 > thcValue) {
        invalid = true
        val ar = Alarm.Alarm(Monitor.withName(record.name), MonitorType.map(A226).id, record.date, 1.0f, lead + "10")
        try {
          if (thc.getOrElse(true))
            Alarm.insertAlarm(ar)

          val m = Monitor.withName(record.name)
          for (autoTicket <- Monitor.map(m).autoAudit.compareRule.autoTicket if autoTicket && thc2.getOrElse(true))
            Alarm.newTicketFromAlarm(ar, DateTime.now().plusDays(2))
        } catch {
          case _: Exception =>
        }
        targetStat.setAuditStat(A226, lead)
        targetStat.setAuditStat(A286, lead)
      }
    }

    val nox_rec = Record.monitorTypeProject2(A223)(record)
    val no2_rec = Record.monitorTypeProject2(A293)(record)
    if ((nox.getOrElse(true) || nox2.getOrElse(true)) && isOk(nox_rec) && isOk(no2_rec)) {
      val noxValue = nox_rec._1.get
      val no2Value = no2_rec._1.get
      if (noxValue < no2Value) {
        invalid = true
        val ar = Alarm.Alarm(Monitor.withName(record.name), MonitorType.map(A223).id, record.date, 1.0f, lead + "10")
        try {
          if (nox.getOrElse(true))
            Alarm.insertAlarm(ar)
          val m = Monitor.withName(record.name)
          for (autoTicket <- Monitor.map(m).autoAudit.compareRule.autoTicket if autoTicket && nox2.getOrElse(true))
            Alarm.newTicketFromAlarm(ar, DateTime.now().plusDays(2))
        } catch {
          case _: Exception =>
        }
        targetStat.setAuditStat(A223, lead)
        targetStat.setAuditStat(A293, lead)
      }
    }

    val pm25Record = Record.monitorTypeProject2(A215)(record)
    val pm10Record = Record.monitorTypeProject2(A214)(record)
    val tspRecord = Record.monitorTypeProject2(A213)(record)
    val humidRecord = Record.monitorTypeProject2(C215)(record)
    if ((pm10.getOrElse(true) || pm102.getOrElse(true)) && isOk(tspRecord) && isOk(pm10Record)) {
      if (pm10Record._1.get > tspRecord._1.get) {
        invalid = true
        targetStat.setAuditStat(A214, lead)
        targetStat.setAuditStat(A213, lead)
        val ar = Alarm.Alarm(Monitor.withName(record.name), "Z206", record.date, 1.0f, "056")
        try {
          if (pm10.getOrElse(true))
            Alarm.insertAlarm(ar)

          val m = Monitor.withName(record.name)
          for (autoTicket <- Monitor.map(m).autoAudit.compareRule.autoTicket if autoTicket && pm102.getOrElse(true))
            Alarm.newTicketFromAlarm(ar, DateTime.now().plusDays(2))
        } catch {
          case _: Exception =>
        }
      }
    }
    if ((pm10.getOrElse(true) || pm102.getOrElse(true)) && isOk(pm25Record) && isOk(tspRecord)) {
      if (pm25Record._1.get > tspRecord._1.get) {
        invalid = true
        targetStat.setAuditStat(A215, lead)
        targetStat.setAuditStat(A213, lead)
        val ar = Alarm.Alarm(Monitor.withName(record.name), "Z216", record.date, 1.0f, "058")
        try {
          if(pm10.getOrElse(true))
            Alarm.insertAlarm(ar)

          val m = Monitor.withName(record.name)
          for (autoTicket <- Monitor.map(m).autoAudit.compareRule.autoTicket if autoTicket && pm102.getOrElse(true))
            Alarm.newTicketFromAlarm(ar, DateTime.now().plusDays(2))
        } catch {
          case _: Exception =>
        }
      }
    }

    if ((pm10.getOrElse(true) || pm102.getOrElse(true)) && isOk(pm25Record) && isOk(pm10Record)) {
      if (pm25Record._1.get > pm10Record._1.get) {
        invalid = true
        targetStat.setAuditStat(A214, lead)
        targetStat.setAuditStat(A215, lead)
        val ar = Alarm.Alarm(Monitor.withName(record.name), "Z226", record.date, 1.0f, "059")
        try {
          if(pm10.getOrElse(true))
            Alarm.insertAlarm(ar)

          val m = Monitor.withName(record.name)
          for (autoTicket <- Monitor.map(m).autoAudit.compareRule.autoTicket if autoTicket && pm102.getOrElse(true))
            Alarm.newTicketFromAlarm(ar, DateTime.now().plusDays(2))
        } catch {
          case _: Exception =>
        }
      }
    }

    if ((humid.getOrElse(true)||humid2.getOrElse(true)) && isOk(humidRecord)) {
      for (v <- humidRecord._1 if v > 100) {
        invalid = true
        targetStat.setAuditStat(C215, lead)
        val ar = Alarm.Alarm(Monitor.withName(record.name), "Z215", record.date, 1.0f, "059")
        try {
          if(humid.getOrElse(true))
            Alarm.insertAlarm(ar)

          val m = Monitor.withName(record.name)
          for (autoTicket <- Monitor.map(m).autoAudit.compareRule.autoTicket if autoTicket && humid2.getOrElse(true))
            Alarm.newTicketFromAlarm(ar, DateTime.now().plusDays(2))
        } catch {
          case ex: Exception =>
        }
      }
    }
    invalid
  }
}

object CompareRule {
  implicit val compareRuleRead = Json.reads[CompareRule]
  implicit val compareRuleWrite = Json.writes[CompareRule]

}

case class DifferenceRule(
                           enabled: Boolean = false,
                           autoTicket: Option[Boolean] = None,
                           multiplier: Float = 3.0f,
                           monitorTypes: Seq[MonitorType.Value] = Seq.empty[MonitorType.Value]) extends Rule('c') {
  def checkInvalid(record: HourRecord, targetStat: AuditStat, monitor: Monitor.Value, start: DateTime): Boolean = {
    if (!enabled)
      return false

    var invalid = false
    val records = getHourRecords(monitor, start - 24.hour, start).toArray
    val mtAvgStdPairs =
      for {
        mt <- monitorTypes
        mt_records = records.map {
          Record.monitorTypeProject2(mt)
        }.filter(isOk).map { r => r._1.get } if (mt_records.length != 0)
      } yield {
        val count = mt_records.length
        val avg = mt_records.sum / count
        val std = Math.sqrt(mt_records.map { r => (r - avg) * (r - avg) }.sum / count)
        mt -> (avg, std)
      }

    val avgStdMap = Map(mtAvgStdPairs: _*)

    for {
      mt <- monitorTypes
      mr_record = Record.monitorTypeProject2(mt)(record) if isOk(mr_record)
      v <- mr_record._1
      (avg, std) <- avgStdMap.get(mt)
    } {
      if (Math.abs(v - avg) > multiplier * std) {
        invalid = true
        val ar = Alarm.Alarm(Monitor.withName(record.name), MonitorType.map(mt).id, record.date, 1.0f, lead + "10")
        try {
          Alarm.insertAlarm(ar)
          val m = Monitor.withName(record.name)
          for (autoTicket <- Monitor.map(m).autoAudit.differenceRule.autoTicket if autoTicket)
            Alarm.newTicketFromAlarm(ar, DateTime.now().plusDays(2))
        } catch {
          case ex: Exception =>
          // Skip duplicate alarm
        }
        targetStat.setAuditStat(mt, lead)
      }
    }

    invalid
  }
}

object DifferenceRule {
  implicit val differenceRuleRead = Json.reads[DifferenceRule]
  implicit val differenceRuleWrite = Json.writes[DifferenceRule]

}

case class SpikeCfg(
                     id: MonitorType.Value,
                     abs: Float)

case class SpikeRule(
                      enabled: Boolean = false,
                      autoTicket: Option[Boolean] = None,
                      monitorTypes: Seq[SpikeCfg] = Seq.empty[SpikeCfg]) extends Rule('d') {
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
          val ar = Alarm.Alarm(Monitor.withName(record.name), MonitorType.map(mtcfg.id).id, record.date, 1.0f, lead + "10")
          try {
            Alarm.insertAlarm(ar)
            val m = Monitor.withName(record.name)
            for (autoTicket <- Monitor.map(m).autoAudit.spikeRule.autoTicket if autoTicket)
              Alarm.newTicketFromAlarm(ar, DateTime.now().plusDays(2))
          } catch {
            case ex: Exception =>
            // Skip duplicate alarm
          }

          targetStat.setAuditStat(mtcfg.id, lead)
        }
      }
    }

    invalid
  }
}

object SpikeRule {
  implicit val spikeCfgRead = Json.reads[SpikeCfg]
  implicit val spikeRuleRead = Json.reads[SpikeRule]
  implicit val spikeCfgWrite = Json.writes[SpikeCfg]
  implicit val spikeRuleWrite = Json.writes[SpikeRule]
}

case class PersistenceRule(
                            enabled: Boolean = false,
                            autoTicket: Option[Boolean] = None,
                            same: Int = 3,
                            monitorTypes: Option[Seq[MonitorType.Value]] = Some(Seq.empty[MonitorType.Value])) extends Rule('e') {
  def checkInvalid(record: HourRecord, targetStat: AuditStat, monitor: Monitor.Value, start: DateTime): Boolean = {
    if (!enabled)
      return false

    var invalid = false
    val pre_records = getHourRecords(monitor, start - (same - 1).hour, start).toArray

    val mtList = monitorTypes.getOrElse(Seq.empty[MonitorType.Value])

    for (mt <- mtList) {
      val mt_rec = Record.monitorTypeProject2(mt)(record)
      if (isOk(mt_rec)) {
        val pre_mt_rec = pre_records.map(Record.monitorTypeProject2(mt)).filter(isOk).filter(r => r._1.get == mt_rec._1.get)
        if (pre_mt_rec.length == same - 1) {
          invalid = true
          val ar = Alarm.Alarm(Monitor.withName(record.name), MonitorType.map(mt).id, record.date, 1.0f, lead + "10")
          try {
            Alarm.insertAlarm(ar)
            val m = Monitor.withName(record.name)
            for (autoTicket <- Monitor.map(m).autoAudit.persistenceRule.autoTicket if autoTicket)
              Alarm.newTicketFromAlarm(ar, DateTime.now().plusDays(2))
          } catch {
            case ex: Exception =>
            // Skip duplicate alarm
          }

          targetStat.setAuditStat(mt, lead)
        }
      }
    }

    invalid
  }
}

object PersistenceRule {
  implicit val persistenceRuleRead = Json.reads[PersistenceRule]
  implicit val persistenceRuleWrite = Json.writes[PersistenceRule]

}

case class MonoCfg(
                    id: MonitorType.Value,
                    abs: Float)

case class MonoRule(enabled: Boolean = false,
                    autoTicket: Option[Boolean] = None,
                    count: Int = 3,
                    monitorTypes: Seq[MonoCfg] = Seq.empty[MonoCfg]) extends Rule('f') {
  def checkInvalid(record: HourRecord, targetStat: AuditStat, monitor: Monitor.Value, start: DateTime): Boolean = {
    if (!enabled)
      return false

    var invalid = false
    val pre_records = getHourRecords(monitor, start - (count - 1).hour, start).toList

    for (mtcfg <- monitorTypes) {
      val mt_rec = Record.monitorTypeProject2(mtcfg.id)(record)
      val pre_rec = pre_records.map(Record.monitorTypeProject2(mtcfg.id)).filter(isOk)
      if (isOk(mt_rec) && pre_rec.length == count - 1) {
        val values = pre_rec.map(_._1.get) ::: List(mt_rec._1.get)
        val max = values.max
        val min = values.min
        if ((max - min) < mtcfg.abs) {
          invalid = true
          val ar = Alarm.Alarm(Monitor.withName(record.name), MonitorType.map(mtcfg.id).id, record.date, 1.0f, lead + "10")
          try {
            Alarm.insertAlarm(ar)
            val m = Monitor.withName(record.name)
            for (autoTicket <- Monitor.map(m).autoAudit.monoRule.autoTicket if autoTicket)
              Alarm.newTicketFromAlarm(ar, DateTime.now().plusDays(2))
          } catch {
            case ex: Exception =>
            // Skip duplicate alarm
          }

          targetStat.setAuditStat(mtcfg.id, lead)
        }
      }
    }

    invalid
  }
}

object MonoRule {
  implicit val monoCfgRead = Json.reads[MonoCfg]
  implicit val monoCfgWrite = Json.writes[MonoCfg]
  implicit val monoRuleRead = Json.reads[MonoRule]
  implicit val monoRuleWrite = Json.writes[MonoRule]

}

case class TwoHourRule(enabled: Boolean = false,
                       autoTicket: Option[Boolean] = None,
                       monitorTypes: Seq[MonoCfg] = Seq.empty[MonoCfg]) extends Rule('g') {
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
          val ar = Alarm.Alarm(Monitor.withName(record.name), MonitorType.map(mtcfg.id).id, record.date, 1.0f, lead + "10")
          try {
            Alarm.insertAlarm(ar)
            val m = Monitor.withName(record.name)
            for (autoTicket <- Monitor.map(m).autoAudit.twoHourRule.autoTicket if autoTicket)
              Alarm.newTicketFromAlarm(ar, DateTime.now().plusDays(2))
          } catch {
            case ex: Exception =>
            // Skip duplicate alarm
          }

          targetStat.setAuditStat(mtcfg.id, lead)
        }
      }
    }

    invalid
  }
}

object TwoHourRule {
  implicit val monoCfgRead = Json.reads[MonoCfg]
  implicit val monoCfgWrite = Json.writes[MonoCfg]
  implicit val read = Json.reads[TwoHourRule]
  implicit val write = Json.writes[TwoHourRule]
}

case class ThreeHourCfg(
                         id: MonitorType.Value,
                         abs: Float,
                         percent: Float)

case class ThreeHourRule(enabled: Boolean = false,
                         autoTicket: Option[Boolean] = None,
                         monitorTypes: Seq[ThreeHourCfg] = Seq.empty[ThreeHourCfg]) extends Rule('h') {
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
          for (v1 <- values.zipWithIndex.dropRight(1)) yield {
            val v2 = values(v1._2)
            (Math.abs(v1._1 - v2), Math.abs((1 - v1._1 / v2) * 100))
          }
        val overs = abs_percent.filter(v => v._1 > mtcfg.abs && v._2 > mtcfg.percent)
        if (overs.length == 2) {
          invalid = true
          val ar = Alarm.Alarm(Monitor.withName(record.name), MonitorType.map(mtcfg.id).id, record.date, 1.0f, lead + "10")
          try {
            Alarm.insertAlarm(ar)
            val m = Monitor.withName(record.name)
            for (autoTicket <- Monitor.map(m).autoAudit.threeHourRule.autoTicket if autoTicket)
              Alarm.newTicketFromAlarm(ar, DateTime.now().plusDays(2))
          } catch {
            case ex: Exception =>
            // Skip duplicate alarm
          }

          targetStat.setAuditStat(mtcfg.id, lead)
        }
      }
    }

    invalid
  }
}

object ThreeHourRule {
  implicit val thcfgRead = Json.reads[ThreeHourCfg]
  implicit val thcfgWrite = Json.writes[ThreeHourCfg]
  implicit val reads = Json.reads[ThreeHourRule]
  implicit val writes = Json.writes[ThreeHourRule]
}

case class FourHourCfg(
                        id: MonitorType.Value,
                        abs: Float)

case class FourHourRule(enabled: Boolean = false,
                        autoTicket: Option[Boolean] = None,
                        monitorTypes: Seq[FourHourCfg] = Seq.empty[FourHourCfg]) extends Rule('i') {
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
        val avg = values.sum / 4
        if (avg > mtcfg.abs) {
          invalid = true
          val ar = Alarm.Alarm(Monitor.withName(record.name), MonitorType.map(mtcfg.id).id, record.date, 1.0f, lead + "10")
          try {
            Alarm.insertAlarm(ar)
            val m = Monitor.withName(record.name)
            for (autoTicket <- Monitor.map(m).autoAudit.fourHourRule.autoTicket if autoTicket)
              Alarm.newTicketFromAlarm(ar, DateTime.now().plusDays(2))
          } catch {
            case ex: Exception =>
            // Skip duplicate alarm
          }

          targetStat.setAuditStat(mtcfg.id, lead)
        }
      }
    }

    invalid
  }
}

object FourHourRule {
  implicit val thcfgRead = Json.reads[FourHourCfg]
  implicit val thcfgWrite = Json.writes[FourHourCfg]
  implicit val reads = Json.reads[FourHourRule]
  implicit val writes = Json.writes[FourHourRule]

}

case class OverInternalStdMinRule(enabled: Boolean = false,
                                  autoTicket: Option[Boolean] = None,
                                  threshold: Int = 20) extends Rule('j') {
  def checkInvalid(m: Monitor.Value): Boolean = {
    if (!enabled)
      return false

    var invalid = false
    val mCase = Monitor.map(m)

    val records = Record.getMinRecords(m, DateTime.now - 1.hour, DateTime.now)
    for {
      mt <- mCase.monitorTypes
      std_internal <- MonitorTypeAlert.map(m)(mt).internal
      mtRecords = records.map {
        Record.monitorTypeProject2(mt)
      }
    } {
      val over = mtRecords.count(r => r._1.isDefined && r._2.isDefined
        && MonitorStatus.isNormalStat(r._2.get)
        && r._1.get > std_internal)

      if (over > threshold) {
        invalid = true
        val ar = Alarm.Alarm(m, mt.toString, DateTime.now, 1.0f, lead + "10")
        try {
          Alarm.insertAlarm(ar)
          for {overInternal <- Monitor.map(m).autoAudit.overInternalStdMinRule
               autoTicket <- overInternal.autoTicket if autoTicket}
            Alarm.newTicketFromAlarm(ar, DateTime.now().plusDays(2))
        } catch {
          case ex: Exception =>
          // Skip duplicate alarm
        }
      }

    }

    invalid
  }
}

object OverInternalStdMinRule {
  implicit val reads = Json.reads[OverInternalStdMinRule]
  implicit val writes = Json.writes[OverInternalStdMinRule]
}

case class DataReadyMinRule(enabled: Boolean = false,
                            autoTicket: Option[Boolean] = None,
                            overdue: Int = 20) extends Rule('k') {
  def checkInvalid(m: Monitor.Value): Boolean = {
    if (!enabled)
      return false

    val currentMinOpt = Realtime.getLatestMonitorRecordTime(TableType.Min, m)
    if (currentMinOpt.isDefined) {
      val duetime = DateTime.now() - overdue.minutes
      if (currentMinOpt.get.toDateTime < duetime) {
        for (mt <- Monitor.map(m).monitorTypes) {
          val ar = Alarm.Alarm(m, mt.toString, DateTime.now, 1.0f, lead + "10")
          try {
            Alarm.insertAlarm(ar)
            for {dataReadyRule <- Monitor.map(m).autoAudit.dataReadyMinRule
                 autoTicket <- dataReadyRule.autoTicket if autoTicket}
              Alarm.newTicketFromAlarm(ar, DateTime.now().plusDays(2))
          } catch {
            case ex: Exception =>
            // Skip duplicate alarm
          }
        }
        return true
      }
    }

    false
  }
}

object DataReadyMinRule {
  implicit val reads = Json.reads[DataReadyMinRule]
  implicit val writes = Json.writes[DataReadyMinRule]
}

case class OtherRule(enabled: Boolean = false,
                     autoTicket: Option[Boolean] = Some(false),
                     calibrate: Boolean = true,
                     invalidData: Boolean = true,
                     instAbnormal: Boolean = true,
                     overStd: Boolean = true,
                     calibrate2: Option[Boolean] = Some(true),
                     invalidData2: Option[Boolean] = Some(true),
                     instAbnormal2: Option[Boolean] = Some(true),
                     overStd2: Option[Boolean] = Some(true)
                    )

object OtherRule {
  implicit val reads = Json.reads[OtherRule]
  implicit val writes = Json.writes[OtherRule]
}

case class AutoAudit(
                      minMaxRule: MinMaxRule,
                      compareRule: CompareRule,
                      differenceRule: DifferenceRule,
                      spikeRule: SpikeRule,
                      persistenceRule: PersistenceRule,
                      monoRule: MonoRule,
                      twoHourRule: TwoHourRule,
                      threeHourRule: ThreeHourRule,
                      fourHourRule: FourHourRule,
                      overInternalStdMinRule: Option[OverInternalStdMinRule] = Some(OverInternalStdMinRule()),
                      dataReadyMinRule: Option[DataReadyMinRule] = Some(DataReadyMinRule()),
                      var otherRule: Option[OtherRule] = Some(OtherRule()))

/**
 * @author user
 */
object AutoAudit {
  implicit val autoAuditRead = Json.reads[AutoAudit]
  implicit val autoAuditWrite = Json.writes[AutoAudit]

  val default = AutoAudit(
    MinMaxRule(),
    CompareRule(),
    DifferenceRule(),
    SpikeRule(),
    PersistenceRule(),
    MonoRule(),
    TwoHourRule(),
    ThreeHourRule(),
    FourHourRule())

  val map = Map(
    'a' -> "極大極小值",
    'b' -> "合理性",
    'c' -> "單調性",
    'd' -> "突波高值",
    'e' -> "持續性",
    'f' -> "一致性",
    'g' -> "小時測值變化驗證",
    'h' -> "三小時變化測值驗證",
    'i' -> "四小時變化測值驗證",
    'j' -> "分鐘值超過內控",
    'k' -> "分鐘值回傳超時",
    'l' -> "其他")
}