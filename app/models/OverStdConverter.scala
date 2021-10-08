package models

import akka.actor.{Actor, ActorRef, PoisonPill, Props}
import play.api.libs.concurrent.Akka
import play.api.Play.current
import com.github.nscala_time.time.Imports._
import models.Record.{getFieldName, getTabName}
import play.api.Logger
import scalikejdbc._
import models.ModelHelper._

import java.sql.Timestamp
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Future, blocking}

object OverStdConverter {
  var worker: ActorRef = _

  def start = {
    val year = SystemConfig.getConvertOverStdYear
    worker = Akka.system.actorOf(Props[OverStdConverter], name = "OverStdConverter")
    worker ! ConvertStatus(year)
  }
  case class ConvertStatus(year:Int)

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

  def convertOverStdToNewCode(tabType: TableType.Value, year:Int, monitor: Monitor.Value, monitorType: MonitorType.Value)(implicit session: DBSession = AutoSession) = {
    val monitorName = monitor.toString()
    val tab_name = getTabName(tabType, year)
    val field_name = getFieldName(tabType, monitorType)
    sql"""
          Update ${tab_name}
          Set ${field_name}='016'
          Where DP_NO=${monitorName} and ${field_name} = '011'
    """.update.apply
  }
  def checkIfHasField(tabType: TableType.Value, year:Int, monitorType: MonitorType.Value)(implicit session: DBSession = AutoSession) ={
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
         """.map(rs=>rs.int(1)==1).first().apply()
  }

  def changeOverStdCode()(implicit session: DBSession = AutoSession)={
    sql"""
         UPDATE [dbo].[Infor_Status]
          SET [statusNo] = '016'
          WHERE [statusNo] = '011'
         """
  }
}

case class OverStdConverter() extends Actor {
  import OverStdConverter._

  def convertStatusOfYear(year: Int)={
    val tabList = Seq(TableType.Hour, TableType.Min)
    for(tabType<-tabList){
      Logger.info(s"convert $year $tabType data")
      for{m<-Monitor.mvList
          mt<-Monitor.map(m).monitorTypes
          hasField<- checkIfHasField(tabType, year, mt) if hasField == true
          }{
        convertOverStdToNewCode(tabType, year, m, mt)
      }
    }
  }

  override def receive: Receive = {
    case ConvertStatus(year)=>
      if (!Ozone8Hr.hasHourTab(year)) {
        Logger.info(s"Reach end of beginning $year")
        self ! PoisonPill
      } else {
        Future {
          blocking {
            convertStatusOfYear(year)
            SystemConfig.setConvertOverStdYear(year -1)
            self ! ConvertStatus(year - 1 )
          }
        }
      }
  }
}
