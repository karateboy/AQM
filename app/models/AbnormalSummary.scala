package models

import com.github.nscala_time.time.Imports
import com.github.nscala_time.time.Imports._

import scala.collection.immutable

case class AbnormalSummary(monitor: Monitor.Value, monitorType: MonitorType.Value,
                           abnormalType: String, duration: String, count: Int)

object AbnormalSummary {
  val abnormalStatusList = List("038", "055", "032", "M00")

  def query(monitors: Seq[Monitor.Value], start: DateTime, end: DateTime): Seq[AbnormalSummary] = {
    for {m <- monitors
         hourList = Record.getHourRecords(m, start, end)
         mt <- Monitor.map(m).monitorTypes
         mtStatusOptList = hourList.map(Record.timeStatusProjection(mt))
         abnormalStatus <- abnormalStatusList
         mtStatusList = mtStatusOptList.flatMap(p => {
           for (status <- p._2) yield
             (p._1, status)
         })
         targetTagInfo = MonitorStatus.getTagInfo(abnormalStatus)
         filtered = mtStatusList.filter(p => {
           val tagInfo = MonitorStatus.getTagInfo(p._2)
           if (tagInfo.statusType == targetTagInfo.statusType && tagInfo.id == targetTagInfo.id)
             true
           else if (tagInfo.statusType == targetTagInfo.statusType && targetTagInfo.statusType == StatusType.Manual)
             true
           else
             false
         })
         count = filtered.size if count > 0
         } yield {

      val timeList: Seq[DateTime] = filtered.map(t => new DateTime(t._1.getTime))
      val timeListByDate: Map[DateTime, Seq[Imports.DateTime]] = timeList.groupBy(t => t.withMillisOfDay(0))
      val keys = timeListByDate.keys.toList.sorted
      def genDesc(start: Int, end: Int, list: Seq[Int]): String = {
        def output =
          if (start != end)
            s"(${start}-${end})"
          else
            s"(${start})"

        list match {
          case Nil =>
            output
          case head :: tail =>
            if ((end + 1) != head)
              output + " ," + genDesc(head, head, tail)
            else
              genDesc(start, head, tail)
        }
      }
      val dateSummaries: immutable.Iterable[String] =
        for(date<-keys) yield{
          val hours = timeListByDate(date).sorted.map(_.getHourOfDay)
          val dateStr = date.toString("M/d")
          if (hours.isEmpty)
            ""
          else {
            val head = hours.head
            s"${dateStr}${genDesc(head, head, hours.tail)}"
          }
        }
      val duration = dateSummaries.mkString(", ")
      val abnormalType = MonitorStatus.map(abnormalStatus).desp
      AbnormalSummary(m, mt, abnormalType, duration, count)
    }
  }
}