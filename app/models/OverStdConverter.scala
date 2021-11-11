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


  def convertStatus(m: Monitor.Value, tableType: TableType.Value, start:DateTime, end:DateTime): Unit = {
    val recordList = tableType match {
      case TableType.Min =>
        Record.getMinRecords(m, start, end)
      case TableType.Hour =>
        Record.getHourRecords(m, start, end)
    }

    val auditStatList: Seq[AuditStat] = recordList map AuditStat
    for (auditStat <- auditStatList) {
      for {
        mt <- Monitor.map(m).monitorTypes if MonitorTypeAlert.map(m).contains(mt)
        (valueOpt, statusOpt) = auditStat.getValueStat(mt)
        value <- valueOpt
        status <- statusOpt if status.startsWith("01")
        mta = MonitorTypeAlert.map(m)(mt)
      } {
        def checkWarn(): Option[Boolean] =
          for (warn <- mta.warn if value >= warn) yield {
            auditStat.setStat(mt, MonitorStatus.WARN_STAT)
            true
          }

        def checkStdLaw(): Option[Boolean] =
          for (law <- mta.std_law if value >= law) yield {
            auditStat.setStat(mt, MonitorStatus.OVER_STAT)
            true
          }

        val checkSeq: Seq[(Option[Float], () => Option[Boolean])] = Seq((mta.std_law, checkStdLaw),
          (MonitorTypeAlert.map(m)(mt).warn, checkWarn))
        val sorted: Seq[(Option[Float], () => Option[Boolean])] = checkSeq.sortBy(_._1).reverse
        sorted.find(t => t._2() == Some(true))
      }

      if (auditStat.changed)
        auditStat.updateDB()
    }
  }

  case class ConvertStatus(year: Int)

  case object ConvertCurrentStatus
}

case class OverStdConverter() extends Actor {

  import OverStdConverter._

  var timer: Cancellable =
    context.system.scheduler.scheduleOnce(FiniteDuration.apply(10, scala.concurrent.duration.SECONDS),
      self, ConvertCurrentStatus)

  override def receive: Receive = {
    case ConvertStatus(year) =>
      if (SystemConfig.getGenerateAggregate2)
        Future {
          blocking {
            AggregateReport2.generatePast()
            SystemConfig.setGenerateAggregate2(false)
          }
        }
      if (SystemConfig.getUpdatePastAggregate2) {
        Future {
          blocking {
            AggregateReport2.updatePastState()
            SystemConfig.setUpdatePastAggregate2(false)
          }
        }
      }

      if (Ozone8Hr.hasHourTab(year))
        Future {
          blocking {
            Logger.info(s"Convert $year year OverStd code")
            convertStatusOfYear(year)
            SystemConfig.setConvertOverStdYear(year - 1)
            self ! ConvertStatus(year - 1)
          }
        }
    case ConvertCurrentStatus =>
      for {tab <- Seq(TableType.Hour, TableType.Min)
           m <- Monitor.mvList
           } {
        val now = DateTime.now
        val (start, end) = if(tab == TableType.Hour)
          (now - 3.day, now)
        else
          (now - 3.hour, now)

        convertStatus(m, tab, start, end)
      }
      timer = context.system.scheduler.scheduleOnce(FiniteDuration.apply(10, scala.concurrent.duration.MINUTES),
        self, ConvertStatus)
  }

  def convertStatusOfYear(year: Int) = {
    val tabList = Seq(TableType.Hour)
    for (tabType <- tabList) {
      Logger.info(s"convert $year $tabType data")
      for {m <- Monitor.mvList
           } {
        val startOfYear = new DateTime(year, 1, 1, 0, 0)
        val endOfYear = startOfYear + 1.year
        convertStatus(m, tabType, startOfYear, endOfYear)
      }
      /*
      for {m <- Monitor.mvList
           mt <- Monitor.map(m).monitorTypes
           hasField <- checkIfHasField(tabType, year, mt) if hasField == true
           } {
        val startOfYear = new DateTime(year, 1, 1)
        val endOfYear = startOfYear + 1.year
        convertStatus(m, tabType, startOfYear, endOfYear)
        // convertOverStdToNewCode(tabType, year, m, mt)
      }*/
    }
  }

  override def postStop(): Unit = {
    timer.cancel()
    super.postStop()
  }
}
