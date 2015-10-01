package models
import scalikejdbc._
import play.api._
import com.github.nscala_time.time.Imports._
import ModelHelper._
import models._
import play.api.libs.json._
import play.api.libs.functional.syntax._
import play.api.libs.json.Json

case class AbnormalEntry(monitor:Monitor.Value, monitorType:MonitorType.Value, invalidHours:String, explain:String)
case class AbnormalReport(date:DateTime, report:Seq[AbnormalEntry])
object AbnormalReport {
  implicit val abEntriesReads = Json.reads[AbnormalEntry]
  
  
  def getReportFromDb(date:DateTime)(implicit session: DBSession = AutoSession)={
    DB localTx { implicit session =>
      sql"""
        Select * 
        From AbnormalReport
        Where date = ${date}
        """.map{
          r=>
            val report = Json.parse(r.string(2)).validate[Seq[AbnormalEntry]].get
            AbnormalReport(r.date(1), report)
        }.single.apply
    }
  }

  def generate(date: DateTime)(implicit session: DBSession = AutoSession) = {
    import Record._
    val pairs =
      for (m <- Monitor.mvList) yield {
        val records = Record.getHourRecords(m, date, date + 1.day)
        def getInvalidHourDesc(mt: MonitorType.Value) = {
          val mtRecord = records.map { monitorTypeProject2(mt) }.zipWithIndex
          val invalidRecord =
            for {
              r <- mtRecord
              hr = r._2
              data = r._1 if !(data._1.isDefined && data._2.isDefined && MonitorStatus.isNormalStat(data._2.get))
            } yield if (data._1.isEmpty || data._2.isEmpty)
              (hr, "資料遺失")
            else
              (hr, MonitorStatus.map(data._2.get).desp)

          def genDesc(start: Int, end: Int, status: String, list: List[(Int, String)]): String = {
            list match {
              case Nil =>
                s"${status}(${start}-${end})"
              case head :: tail =>
                if (status != head._2)
                  s"${status}(${start}-${end}), " + genDesc(head._1, head._1, head._2, tail)
                else
                  genDesc(start, head._1, head._2, tail)
            }
          }

          if (invalidRecord.isEmpty)
            ""
          else {
            val head = invalidRecord.head
            genDesc(head._1, head._1, head._2, invalidRecord.tail)
          }
        }

        val l =
          for {
            mt <- Monitor.map(m).monitorTypes
            desc = getInvalidHourDesc(mt) if !desc.isEmpty()
          } yield (m, mt, desc)

        l.toList
      }

    pairs.flatMap(f =>
      f.map(r =>
        AbnormalEntry(r._1, r._2, r._3, "")))
  }
}