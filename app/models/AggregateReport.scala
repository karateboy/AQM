package models

import com.github.nscala_time.time.Imports._
import models.ModelHelper._
import play.api.libs.json.Json
import scalikejdbc._

case class MonitorSummary(monitor: Monitor.Value, desc: String, var explain: String)

case class AggregateReport(date: DateTime, report: Seq[MonitorSummary])

object AggregateReport {
  implicit val mSummaryReads = Json.reads[MonitorSummary]
  implicit val mSummaryWrites = Json.writes[MonitorSummary]

  val defaultExplain = Seq.empty[MonitorSummary]

  def getReportListFromDb(start: DateTime, end: DateTime)(implicit session: DBSession = AutoSession) = {
    DB readOnly { implicit session =>
      sql"""
        Select *
        From AggregateReport
        Where date between $start and $end
        """.map {
        rs =>
          val report = Json.parse(rs.string(2)).validate[Seq[MonitorSummary]].get
          AggregateReport(rs.date(1), report)
      }.list().apply()
    }
  }

  def updateReport(date: DateTime, explain: Seq[MonitorSummary])(implicit session: DBSession = AutoSession) = {
    DB localTx { implicit session =>
      val explainJson = Json.toJson(explain).toString
      sql"""
        Update AggregateReport
        Set [explain] = ${explainJson}
        Where [date] = ${date}
        """.update.apply
    }
  }

  def getLatestMonitorSummary(date: DateTime): Seq[MonitorSummary] = {
    val report = AggregateReport.getReportFromDb(date).getOrElse({
      AggregateReport.newReport(date)
      AggregateReport(date, Seq.empty[MonitorSummary])
    })

    val explainMap = Map(report.report.map { r => (r.monitor -> r.explain) }: _*)
    val latestReport = AggregateReport(date, AggregateReport.generate(date))
    latestReport.report.map {
      summary =>
        val explain = explainMap.getOrElse(summary.monitor, "")
        MonitorSummary(summary.monitor, summary.desc, explain)
    }
  }

  def newReport(date: DateTime)(implicit session: DBSession = AutoSession) = {
    val explain = Json.toJson(defaultExplain).toString
    sql"""
        Insert into AggregateReport([date],[explain])
        values(${date.toString("YYYY-MM-dd")}, ${explain})
        """.update.apply
  }

  def getReportFromDb(date: DateTime)(implicit session: DBSession = AutoSession) = {
    DB readOnly { implicit session =>
      sql"""
        Select *
        From AggregateReport
        Where date = ${date}
        """.map {
        r =>
          val report = Json.parse(r.string(2)).validate[Seq[MonitorSummary]].get
          AggregateReport(r.date(1), report)
      }.single.apply
    }
  }

  def generate(date: DateTime)(implicit session: DBSession = AutoSession): List[MonitorSummary] = {
    for {
      m <- Monitor.mvList
    } yield {
      val dailyReport = Record.getDailyReport(m, date)
      val instrumentAbnormalMonitorTypes = AggregateReport2.query(Seq(m), Monitor.map(m).monitorTypes, date, date + 1.day)
        .filter(_.state == "儀器異常").map(_.monitorType).distinct

      def getDesc = {
        val windSpeed = dailyReport.typeList.find(t => t.monitorType == MonitorType.C211).get
        val windDirOpt = dailyReport.typeList.find(t => t.monitorType == MonitorType.C212)
        val dirMap =
          Map(
            (0 -> "北"), (1 -> "北北東"), (2 -> "東北"), (3 -> "東北東"), (4 -> "東"),
            (5 -> "東南東"), (6 -> "東南"), (7 -> "南南東"), (8 -> "南"),
            (9 -> "南南西"), (10 -> "西南"), (11 -> "西西南"), (12 -> "西"),
            (13 -> "西北西"), (14 -> "西北"), (15 -> "北北西"))

        val desc =
          for {
            t <- dailyReport.typeList if MonitorTypeAlert.map(m).contains(t.monitorType) &&
              MonitorTypeAlert.map(m)(t.monitorType).internal.isDefined
            mCase = MonitorType.map(t.monitorType)
            mtInternal <- MonitorTypeAlert.map(m)(t.monitorType).internal
            over_hrs = t.dataList.filter(r => r._2.isDefined && r._3.isDefined && MonitorStatus.isNormalStat(r._3.get)
              && r._2.get >= mtInternal) if over_hrs.nonEmpty
          } yield {
            import java.util.Calendar
            val calendar = Calendar.getInstance()

            def genDesc(start: Int, expected: Int, list: List[Int]): String = {
              def output = {
                if (start != (expected - 1))
                  s"${start}-${expected - 1}"
                else
                  s"${start}"
              }

              list match {
                case Nil =>
                  output
                case head :: tail =>
                  if (expected != head)
                    output + "," + genDesc(head, head + 1, tail)
                  else
                    genDesc(start, expected + 1, tail)
              }
            }

            val hours = over_hrs.map { hr =>
              calendar.setTime(hr._1)
              calendar.get(Calendar.HOUR_OF_DAY)
            }
            val header = s"${mCase.desp}於${genDesc(hours.head, hours.head + 1, hours.drop(1))}時超過內控($mtInternal${mCase.unit})"

            val overLaw =
              if (MonitorTypeAlert.map(m)(t.monitorType).std_law.isDefined) {
                if (t.monitorType == MonitorType.A214 || t.monitorType == MonitorType.A213) {
                  if (t.stat.avg.isDefined && t.stat.avg.get >= MonitorTypeAlert.map(m)(t.monitorType).std_law.get)
                    s",日均值${t.stat.avg.get}超過法規值(${MonitorTypeAlert.map(m)(t.monitorType).std_law.get}${mCase.unit})"
                  else {
                    if (t.stat.avg.isDefined)
                      s",日均值${t.stat.avg.get}未超過法規值"
                    else
                      s",日均值無效, 未超過法規值"
                  }
                } else {
                  val overLawHr = over_hrs.filter(_._2.get >= MonitorTypeAlert.map(m)(t.monitorType).std_law.get).map {
                    hr =>
                      calendar.setTime(hr._1)
                      calendar.get(Calendar.HOUR_OF_DAY)
                  }

                  if (overLawHr.nonEmpty)
                    s",${genDesc(overLawHr.head, overLawHr.head + 1, overLawHr.drop(1))}超過法規值(${MonitorTypeAlert.map(m)(t.monitorType).std_law.get}${mCase.unit})"
                  else
                    s",未超過法規值(${MonitorTypeAlert.map(m)(t.monitorType).std_law.get}${mCase.unit})"
                }
              } else
                ",未超過法規值"


            val dir = windDirOpt.map { windDir =>
              if (windDir.stat.avg.isDefined)
                dirMap(Math.ceil((windDir.stat.avg.get - 22.5 / 2) / 22.5).toInt % 16)
              else
                "無資料"
            }.getOrElse("無資料")

            val summary = s"(最大風速${windSpeed.stat.max.getOrElse("")}m/s, 平均風向${dir}, 濃度${t.stat.min.getOrElse("")}~${t.stat.max.getOrElse("")} ${mCase.unit})"

            if (instrumentAbnormalMonitorTypes.contains(t.monitorType))
              header + " (儀器異常)"
            else
              header + overLaw + summary
          }

        if (desc.length == 0)
          "本日測值未超出內控值"
        else
          desc.mkString(".")
      }

      MonitorSummary(m, getDesc, "")
    }
  }
}