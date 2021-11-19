package models
import scalikejdbc._
import play.api._
import com.github.nscala_time.time.Imports._
import ModelHelper._
import models._
import play.api.libs.json._
import play.api.libs.functional.syntax._
import play.api.libs.json.Json

case class AbnormalEntry(monitor: Monitor.Value, monitorType: MonitorType.Value, invalidHours: String, explain: String)
case class AbnormalReport(date: DateTime, report: Seq[AbnormalEntry])
object AbnormalReport {
  implicit val abEntriesReads = Json.reads[AbnormalEntry]
  implicit val abEntriesWrites = Json.writes[AbnormalEntry]

  val defaultExplain = Seq.empty[AbnormalEntry]

  def newReport(date: DateTime)(implicit session: DBSession = AutoSession) = {
    DB localTx { implicit session =>
      val explain = Json.toJson(defaultExplain).toString
      sql"""
        Insert into AbnormalReport([date],[explain])
        values(${date.toString("YYYY-MM-dd")}, ${explain})
        """.update.apply
    }
  }

  def getReportFromDb(date: DateTime)(implicit session: DBSession = AutoSession) = {
    val reportOpt = DB readOnly { implicit session =>
      sql"""
        Select * 
        From AbnormalReport
        Where date = ${date}
        """.map {
        r =>
          val report = Json.parse(r.string(2)).validate[Seq[AbnormalEntry]].get
          AbnormalReport(r.date(1), report)
      }.single.apply
    }

    if (reportOpt.isEmpty) {
      AbnormalReport.newReport(date)
      AbnormalReport(date, Seq.empty[AbnormalEntry])
    } else
      reportOpt.get
  }

  def getExplainMap(report: AbnormalReport) = {
    import scala.collection.mutable.Map
    val explainMap = Map.empty[Monitor.Value, Map[MonitorType.Value, String]]
    for (ex <- report.report) {
      val mtMap = explainMap.getOrElseUpdate(ex.monitor, Map.empty[MonitorType.Value, String])
      mtMap.put(ex.monitorType, ex.explain)
    }
    explainMap
  }

  def getLatestExplain(date:DateTime) = {
    val explainMap = getExplainMap(getReportFromDb(date)) 
    val latestReport = AbnormalReport(date, AbnormalReport.generate(date))
    latestReport.report.map {
      ex =>
        val mtMap = explainMap.getOrElse(ex.monitor, Map.empty[MonitorType.Value, String])
        val explain = mtMap.getOrElse(ex.monitorType, "")
        AbnormalEntry(ex.monitor, ex.monitorType, ex.invalidHours, explain)
    }
  }
  def updateReport(date: DateTime, explain: Seq[AbnormalEntry])(implicit session: DBSession = AutoSession) = {
    DB localTx { implicit session =>
      val explainJson = Json.toJson(explain).toString
      sql"""
        Update AbnormalReport
        Set [explain] = ${explainJson}
        Where [date] = ${date}
        """.update.apply
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
              (data, hr) <- mtRecord if data._1.isEmpty || data._2.isEmpty || (!MonitorStatus.isNormalStat(data._2.get))
            } yield {
              if (data._1.isEmpty || data._2.isEmpty)
                (hr, "資料遺失")
              else
                (hr, MonitorStatus.map(data._2.get).desp)
            }

          def genDesc(start: Int, end: Int, status: String, list: List[(Int, String)]): String = {
            def output = {
              if (start != end)
                s"${status}(${start}-${end})"
              else
                s"${status}(${start})"
            }
            list match {
              case Nil =>
                output
              case head :: tail =>
                if (status != head._2 || ((end + 1) != head._1))
                  output + " ," + genDesc(head._1, head._1, head._2, tail)
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