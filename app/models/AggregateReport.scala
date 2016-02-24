package models
import scalikejdbc._
import play.api._
import com.github.nscala_time.time.Imports._
import ModelHelper._
import models._
import play.api.libs.json._
import play.api.libs.functional.syntax._
import play.api.libs.json.Json

case class MonitorSummary(monitor: Monitor.Value, desc: String, explain: String)
case class AggregateReport(date: DateTime, report: Seq[MonitorSummary])
object AggregateReport {
  implicit val mSummaryReads = Json.reads[MonitorSummary]
  implicit val mSummaryWrites = Json.writes[MonitorSummary]

  val defaultExplain = Seq.empty[MonitorSummary]

  def newReport(date: DateTime)(implicit session: DBSession = AutoSession) = {
    DB localTx { implicit session =>
      val explain = Json.toJson(defaultExplain).toString
      sql"""
        Insert into AggregateReport([date],[explain])
        values(${date.toString("YYYY-MM-dd")}, ${explain})
        """.update.apply
    }
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

  def generate(date: DateTime)(implicit session: DBSession = AutoSession) = {
    for {
      m <- Monitor.mvList
      dailyReport = Record.getDailyReport(m, date)
    } yield {
      val dailyReport = Record.getDailyReport(m, date)
      def getDesc = {
        val windSpeed = dailyReport.typeList.find(t => t.monitorType == MonitorType.C211).get
        val windDir = dailyReport.typeList.find(t => t.monitorType == MonitorType.C212).get
        val dirMap =
          Map(
            (0 -> "北"), (1 -> "北北東"), (2 -> "東北"), (3 -> "東北東"), (4 -> "東"),
            (5 -> "東南東"), (6 -> "東南"), (7 -> "南南東"), (8 -> "南"),
            (9 -> "南南西"), (10 -> "西南"), (11 -> "西西南"), (12 -> "西"),
            (13 -> "西北西"), (14 -> "西北"), (15 -> "北北西"))

        val descs =
          for {
            t <- dailyReport.typeList if Monitor.map(m).getStdInternal(t.monitorType).isDefined
            mCase = MonitorType.map(t.monitorType)
            mtInternal = Monitor.map(m).getStdInternal(t.monitorType).get
            over_hrs = t.dataList.filter(r => r._2.isDefined && r._3.isDefined && MonitorStatus.isNormalStat(r._3.get)
              && r._2.get > mtInternal) if !over_hrs.isEmpty
          } yield {
            import java.util.Calendar
            val calendar = Calendar.getInstance()
            def genDesc(start: Int, expected: Int, list: List[Int]): String = {
              def output = {
                if (start != (expected-1))
                  s"${start}-${expected-1}"
                else
                  s"${start}"
              }
              list match {
                case Nil =>
                  output
                case head :: tail =>
                  if (expected != head)
                    output + "," + genDesc(head, head+1, tail)
                  else
                    genDesc(start, expected+1, tail)
              }
            }
            val hours = over_hrs.map { hr =>
              calendar.setTime(hr._1)
              calendar.get(Calendar.HOUR_OF_DAY)
            }
            val header = s"${mCase.desp}於${genDesc(hours(0),hours(0)+1, hours.drop(1))}時超過內控(${mtInternal}${mCase.unit})"
            val overLaw =
              if (mCase.std_law.isDefined) {
                if (t.monitorType == MonitorType.A214 || t.monitorType == MonitorType.A213) {
                  if (t.stat.avg.isDefined && t.stat.avg.get > mCase.std_law.get)
                    s",日均值${t.stat.avg}超過法規值(${mCase.std_law.get}${mCase.unit})"
                  else
                    s",日均值${t.stat.avg}未超過法規值"
                } else {
                  val overLawHr = over_hrs.filter(_._2.get >= mCase.std_law.get).map{
                    hr=>
                      calendar.setTime(hr._1)
                      calendar.get(Calendar.HOUR_OF_DAY)
                  }
                  
                  if (!overLawHr.isEmpty)
                    s",${genDesc(overLawHr(0), overLawHr(0)+1, overLawHr.drop(1))}超過法規值(${mCase.std_law.get}${mCase.unit})"
                  else
                    ",未超過法規值(${mCase.std_law.get}${mCase.unit})"
                }
              } else
                ",未超過法規值"

            val dir = dirMap(Math.ceil((windDir.stat.avg.get - 22.5 / 2) / 22.5).toInt % 16)
            val summary = s"(最大風速${windSpeed.stat.max}m/s, 平均風向${dir}, 濃度${t.stat.min}~${t.stat.max} ${mCase.unit})"

            header + overLaw + summary
          }
        if (descs.length == 0)
          "本日測值未超出內控值"
        else
          descs.mkString(".")
      }

      MonitorSummary(m, getDesc, "")
    }
  }

}