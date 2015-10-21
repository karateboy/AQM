package models
import scalikejdbc._
import play.api._
import com.github.nscala_time.time.Imports._
import ModelHelper._
import models._
import play.api.libs.json._
import play.api.libs.functional.syntax._
import play.api.libs.json.Json
import java.sql.Time
import scala.language.implicitConversions

case class MonitorJournalJson(date:String, monitor:Monitor.Value,  
    routine_desc:String, abnormal_desc:String, event_desc:String, operator_id:Option[Int], enter_time:String, out_time:String)
    
case class MonitorJournal(date:DateTime, monitor:Monitor.Value,  
    routine_desc:String, abnormal_desc:String, event_desc:String, operator_id:Option[Int], enter_time:Time, out_time:Time)
 
case class MonitorInvalidHour(invalidHour:String, status:String)

object MonitorJournal {
  implicit val mjRead = Json.reads[MonitorJournalJson]
  implicit def jsonConvert(j:MonitorJournalJson)={
    MonitorJournal(DateTime.parse(j.date), j.monitor, j.routine_desc, j.abnormal_desc, j.event_desc, j.operator_id, 
        DateTime.parse(j.enter_time, DateTimeFormat.forPattern("HH:mm")), 
        DateTime.parse(j.out_time, DateTimeFormat.forPattern("HH:mm")))
  }
  
  def newReport(monitor:Monitor.Value, date:DateTime)(implicit session: DBSession = AutoSession)={
    val enter_time:java.sql.Time = DateTime.parse("9:00", DateTimeFormat.forPattern("HH:mm"))
    val out_time:java.sql.Time = DateTime.parse("17:00", DateTimeFormat.forPattern("HH:mm"))

    DB localTx{ implicit session=>
      sql"""
        Insert into MonitorJournal([date],[DP_NO],[routine_desc],[abnormal_desc],[event_desc],[operator_id],[enter_time],[out_time])
        values(${date.toString("YYYY-MM-dd")}, ${monitor.toString}, '', '', '', ${None}, ${enter_time}, ${out_time})
        """.update.apply
    }
  }
  
  def getReportFromDb(monitor:Monitor.Value, date:DateTime)(implicit session: DBSession = AutoSession)={
    DB readOnly { implicit session =>
      sql"""
        Select * 
        From MonitorJournal
        Where date = ${date} and DP_NO = ${monitor.toString}
        """.map{
          r=>
            MonitorJournal(r.date(1), Monitor.withName(r.string(2)), r.string(3), r.string(4), 
                r.string(5), r.intOpt(6), r.time(7), r.time(8))
        }.single.apply
    }
  }

  def updateReport(report : MonitorJournal)(implicit session: DBSession = AutoSession)={
    DB localTx{ implicit session=>
      sql"""
        Update MonitorJournal
        Set [routine_desc]=${report.routine_desc}
        ,[abnormal_desc]=${report.abnormal_desc}
        ,[event_desc]=${report.event_desc}
        ,[operator_id]=${report.operator_id}
        ,[enter_time]=${report.enter_time}
        ,[out_time]=${report.out_time}
        Where [date] = ${report.date} and [DP_NO]=${report.monitor.toString}
        """.update.apply
    }
  }

  def getInvalidHourList(monitor: Monitor.Value, date: DateTime) = {
    import Record._
    val records = Record.getHourRecords(monitor, date, date + 1.day)
    def getInvalidHourDesc(mt: MonitorType.Value) = {
      val mtRecord = records.map { monitorTypeProject2(mt) }.zipWithIndex
      val invalidRecord =
        for {
          r <- mtRecord
          hr = r._2
          data = r._1 if (data._1.isDefined && data._2.isDefined && !MonitorStatus.isNormalStat(data._2.get))
        } yield 
          (hr, MonitorStatus.map(data._2.get).desp)

      def genDesc(start: Int, end: Int, status: String, list: List[(Int, String)]): List[MonitorInvalidHour] = {
        def output = {
          if (start != end)
            MonitorInvalidHour(s"${start}-${end}", s"${status}")
          else
            MonitorInvalidHour(s"${start}", s"${status}")
        }
        
        list match {
          case Nil =>
            output::Nil
          case head :: tail =>
            if (status != head._2)
              output::genDesc(head._1, head._1, head._2, tail)
            else
              genDesc(start, head._1, head._2, tail)
        }
      }

      if (invalidRecord.isEmpty)
        List.empty[MonitorInvalidHour]
      else {
        val head = invalidRecord.head
        genDesc(head._1, head._1, head._2, invalidRecord.tail)
      }
    }

    val l =
      for {
        mt <- Monitor.map(monitor).monitorTypes
        desc = getInvalidHourDesc(mt) if !desc.isEmpty
      } yield (mt, desc)

    l.toList
  }
  

}