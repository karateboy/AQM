package models

import akka.actor.{Actor, ActorRef, Props}
import play.api.libs.concurrent.Akka
import scalikejdbc._

import scala.collection.immutable

object Ozone8Hr {
  def createTab(year: Int)(implicit session: DBSession = AutoSession) = {
    val tabName = SQLSyntax.createUnsafely(s"O3_8Hr_${year}")
    val pkName = SQLSyntax.createUnsafely(s"PK_O3_8Hr_${year}")
    sql"""CREATE TABLE [dbo].[${tabName}](
            [DP_NO] [varchar](6) NOT NULL,
            [M_DateTime] [datetime2](7) NOT NULL,
            [Value] [float] NOT NULL,
            [Status] [varchar](3) NOT NULL,
            CONSTRAINT [${pkName}] PRIMARY KEY CLUSTERED
            (
              [DP_NO] ASC,
              [M_DateTime] ASC
            )WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON) ON [PRIMARY]
       ) ON [PRIMARY]""".execute().apply()
  }

  def hasOzone8hrTab(year: Int)(implicit session: DBSession = AutoSession): Boolean = {
    val list = {
      sql"""
          SELECT TABLE_NAME
          FROM INFORMATION_SCHEMA.TABLES
         """.map { rs => rs.string(1) }.list().apply()
    }
    list.contains(s"O3_8Hr_${year}")
  }
}

import com.github.nscala_time.time.Imports._
import scala.concurrent.ExecutionContext.Implicits.global
import play.api.Play.current
case class OzoneRecord(time:DateTime, value:Option[Float], status:Option[String])
object Ozone8HrCalculator {
  case class CalculateDay(date:DateTime)

  var worker: ActorRef = _

  def start = {
    val date = SystemConfig.getOzone8HrCalculateDate
    worker = Akka.system.actorOf(Props[Ozone8HrCalculator], name = "Ozone8HrCalculator")
    worker ! CalculateDay(date)
  }

  def calculateOzoneOfTheSameMonth(start:DateTime, end:DateTime): Unit ={
    DB readOnly {
      for(m <- Monitor.mvList){
        if(Monitor.map(m).monitorTypes.contains(MonitorType.A225)){
          val hrList: List[OzoneRecord] = Record.getHourRecords(m, start, end)
            .map(hr=>OzoneRecord(hr.date.toJodaDateTime, hr.o3, hr.o3_stat)).toList

          val avgList: Seq[List[OzoneRecord]] =
            for(i <- 0 to hrList.length-8 if hrList(i).time.getDayOfMonth == hrList(i+7).time.getDayOfMonth) yield
              hrList.slice(i, i+7)

          val updateCount =
            for(avgData<- avgList){
              val filtered = avgData
            }
        }
      }

    }

  }
}

class Ozone8HrCalculator extends Actor {
  import Ozone8HrCalculator._
  override def receive: Receive = {
    case CalculateDay(date)=>
      if(!Ozone8Hr.hasOzone8hrTab(date.getYear))
        Ozone8Hr.createTab(date.getYear)

      val start: DateTime = date.withDayOfMonth(1).withMillisOfDay(0)
      val end: DateTime = if(date.withMillisOfDay(0).withHourOfDay(23).isAfterNow())
        DateTime.now().minusHours(1).withMinuteOfHour(0)
      else
        date.withMillisOfDay(0).withHourOfDay(23)

  }
}
