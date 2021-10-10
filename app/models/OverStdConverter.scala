package models

import akka.actor.{Actor, ActorRef, Cancellable, Props}
import com.github.nscala_time.time.Imports._
import models.Record.getTabName
import play.api.Logger
import play.api.Play.current
import play.api.libs.concurrent.Akka
import scalikejdbc._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.FiniteDuration
import scala.concurrent.{Future, blocking}

object OverStdConverter {
  var worker: ActorRef = _

  def start = {
    val year = SystemConfig.getConvertOverStdYear
    worker = Akka.system.actorOf(Props[OverStdConverter], name = "OverStdConverter")
    worker ! ConvertStatus(year)
  }

  def convertOverStdToNewCode(tabType: TableType.Value, year: Int, monitor: Monitor.Value, monitorType: MonitorType.Value)(implicit session: DBSession = AutoSession) = {
    val monitorName = monitor.toString()
    val tab_name = getTabName(tabType, year)
    val field_name = getFieldName(tabType, monitorType)
    sql"""
          Update ${tab_name}
          Set ${field_name}='016'
          Where DP_NO=${monitorName} and ${field_name} = '011'
    """.update.apply
  }

  def getFieldName(tabType: TableType.Value, mt: MonitorType.Value) = {
    val name = mt.toString
    val head = name.charAt(0)
    val tail = name.substring(2)

    tabType match {
      case TableType.Hour =>
        SQLSyntax.createUnsafely(s"${head}5${tail}s")
      case TableType.Min =>
        SQLSyntax.createUnsafely(s"${head}2${tail}s")
    }
  }

  def convertAlarmOverStdToNewCode(year: Int)(implicit session: DBSession = AutoSession) = {
    val tab_name = Alarm.getTabName(year)
    sql"""
          Update ${tab_name}
          Set CODE2='016'
          Where CODE2 = '011'
    """.update.apply
  }

  def checkIfHasField(tabType: TableType.Value, year: Int, monitorType: MonitorType.Value)(implicit session: DBSession = AutoSession) = {
    val tab_name = getTabName(tabType, year)
    val field_name = getFieldName(tabType, monitorType)
    sql"""
          IF COL_LENGTH('$tab_name', '$field_name') IS NULL
            BEGIN
	            SELECT 0
            END
          ELSE
            Begin
	            Select 1
            end
         """.map(rs => rs.int(1) == 1).first().apply()
  }

  def changeOverStdCode()(implicit session: DBSession = AutoSession) = {
    sql"""
         UPDATE [dbo].[Infor_Status]
          SET [statusNo] = '016'
          WHERE [statusNo] = '011'
         """
  }

  def hasAlarmTable(year: Int)(implicit session: DBSession = AutoSession): Boolean = {
    val list = {
      sql"""
          SELECT TABLE_NAME
          FROM INFORMATION_SCHEMA.TABLES
         """.map { rs => rs.string(1) }.list().apply()
    }
    //P1234567_Hr_2012
    list.contains(s"P1234567_Alm_${year}")
  }


  def convertStatus(m: Monitor.Value, tableType: TableType.Value): Unit = {
    val now = DateTime.now()
    val recordList = tableType match {
      case TableType.Min =>
        Record.getMinRecords(m, now - 3.hour, now)
      case TableType.Hour =>
        Record.getHourRecords(m, now - 3.day, now)
    }

    val auditStatList: Seq[AuditStat] = recordList map {
      AuditStat
    }
    for (auditStat <- auditStatList) {
      /*
      for {
        mt <- Monitor.map(m).monitorTypes
        (valueOpt, statusOpt) = auditStat.getValueStat(mt)
        value <- valueOpt
        status <- statusOpt if status.startsWith("01")
        mta = MonitorTypeAlert.map(m)(mt)
      } {
        for (warn <- mta.warn) {
          if (value >= warn)
            auditStat.setStat(mt, MonitorStatus.WARN_STAT)
        }

        for (law <- mta.std_law) {
          if (value >= law)
            auditStat.setStat(mt, MonitorStatus.OVER_STAT)
        }
      }
       */

      // FIXME workaround during final version
      for {
        mt <- Monitor.map(m).monitorTypes
        (_, statusOpt) = auditStat.getValueStat(mt)
        status <- statusOpt if status.startsWith("01")
      }{
        if(status == "011")
          auditStat.setStat(mt, MonitorStatus.OVER_STAT)
      }

      if (auditStat.changed)
        auditStat.updateDB()
    }
  }

  case class ConvertStatus(year: Int)

  case object ConvertStatus
}

case class OverStdConverter() extends Actor {

  import OverStdConverter._

  var timer: Cancellable =
    context.system.scheduler.scheduleOnce(FiniteDuration.apply(10, scala.concurrent.duration.SECONDS),
      self, ConvertStatus)

  override def receive: Receive = {
    case ConvertStatus(year) =>
      if (Ozone8Hr.hasHourTab(year)) {
        Future {
          blocking {
            convertStatusOfYear(year)
            SystemConfig.setConvertOverStdYear(year - 1)
            self ! ConvertStatus(year - 1)
          }
        }
      }
    case ConvertStatus =>
      for{tab<-Seq(TableType.Hour, TableType.Min)
          m <- Monitor.mvList
          }{
        convertStatus(m, tab)
      }
      timer = context.system.scheduler.scheduleOnce(FiniteDuration.apply(10, scala.concurrent.duration.MINUTES),
        self, ConvertStatus)
  }

  def convertStatusOfYear(year: Int) = {
    val tabList = Seq(TableType.Hour, TableType.Min)
    for (tabType <- tabList) {
      Logger.info(s"convert $year $tabType data")
      for {m <- Monitor.mvList
           mt <- Monitor.map(m).monitorTypes
           hasField <- checkIfHasField(tabType, year, mt) if hasField == true
           } {
        convertOverStdToNewCode(tabType, year, m, mt)
      }
    }
  }

  override def postStop(): Unit = {
    timer.cancel()
    super.postStop()
  }
}
