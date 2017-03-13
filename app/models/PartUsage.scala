package models
import scalikejdbc._
import play.api._
import com.github.nscala_time.time.Imports._
import models.ModelHelper._
import models._
import play.api.libs.json._
import play.api.libs.functional.syntax._
import play.api.libs.json.Json

case class FrequencyType(id: String, name: String)
object FrequencyType extends Enumeration {
  implicit val write = Json.writes[FrequencyType]

  val Weekly = Value
  val Biweekly = Value
  val Monthly = Value
  val Quarterly = Value
  val Half_year = Value
  val Yearly = Value
  val Days = Value

  def getPeriod(freq_type: FrequencyType.Value, numDays: Int) = {
    freq_type match {
      case FrequencyType.Weekly =>
        1.week
      case FrequencyType.Biweekly =>
        2.week
      case FrequencyType.Monthly =>
        1.month
      case FrequencyType.Quarterly =>
        3.month
      case FrequencyType.Half_year =>
        6.month
      case FrequencyType.Yearly =>
        1.year
      case FrequencyType.Days =>
        numDays.day
    }
  }

  val map = Map(Weekly -> "單周保養", Biweekly -> "雙周保養", Monthly -> "月保養", Quarterly -> "季保養",
    Half_year -> "半年保養", Yearly -> "年保養", Days -> "日保養(天數)")

  def getList = {
    FrequencyType.values.toList.sorted map {
      t => FrequencyType(t.toString(), map(t))
    }
  }
}

case class PartUsage(id: String, usage: Int, monitor: String, model: String, startDate: DateTime,
                     freqType: String, freqDays: Int, alarm: Boolean, nochange_reason: Option[String] = None, remark: Option[String] = None) {
  def getPartId = {
    val ids = id.split("_")
    ids(0)
  }

  def getFreqDesc = {
    val freq = FrequencyType.withName(freqType)
    if (freq == FrequencyType.Days)
      s"$freqDays 天"
    else {
      FrequencyType.map(freq)
    }
  }

  def getNextReplaceDate = {
    val freq = FrequencyType.withName(freqType)
    startDate + FrequencyType.getPeriod(freq, freqDays)
  }

  def getUsageBefore(date: DateTime) = {
    var totalUsage = 0
    var current = startDate
    val freq = FrequencyType.withName(freqType)
    while (current + FrequencyType.getPeriod(freq, freqDays) < date) {
      totalUsage += usage
      current += FrequencyType.getPeriod(freq, freqDays)
    }

    totalUsage
  }

  def getAlarmCode = {
    val ids = id.split("_")
    ids(0) + "_" + ids(1)
  }
  
}
/**
 * @author user
 */
object PartUsage {
  implicit val partUsageRead = Json.reads[PartUsage]
  private def mapper = (r: WrappedResultSet) => PartUsage(r.string("id"), r.int("usage"), r.string("monitor"), r.string("model"), r.date("startDate"),
    r.string("freqType"), r.int("freqDays"), r.boolean("alarm"), r.stringOpt("nochange_reason"), r.stringOpt("remark"))

  def getList() = {
    DB readOnly { implicit session =>
      sql"""
        Select * 
        From PartUsage
        """.map { mapper }.list.apply
    }
  }

  def getPartUsage(id: String) = {
    DB readOnly { implicit session =>
      sql"""
        Select * 
        From PartUsage
        Where id = ${id}
        """.map { mapper }.single.apply
    }
  }

  def create(newUsage: PartUsage) = {
    DB localTx { implicit session =>
      sql"""
        INSERT INTO [dbo].[PartUsage]
           ([id]
           ,[usage]
           ,[monitor]
           ,[model]
           ,[startDate]
           ,[freqType]
           ,[freqDays]
           ,[alarm])
         VALUES
           (${newUsage.id}
            ,${newUsage.usage}
           ,${newUsage.monitor}
           ,${newUsage.model}
           ,${newUsage.startDate.toDate()}
           ,${newUsage.freqType}
           ,${newUsage.freqDays}
           ,${newUsage.alarm})
        """.update.apply
    }
  }

  def update(id: String, colName: String, newValue: String) = {
    DB localTx { implicit session =>
      val col = SQLSyntax.createUnsafely(s"${colName}")
      if (colName != "usage" || colName != "freq_days")
        sql"""
        Update PartUsage
        Set ${col}=${newValue}
        Where id=${id}  
        """.update.apply
      else {
        val newQuantity = newValue.toInt
        sql"""
        Update Update PartUsage
        Set ${col}=${newQuantity}
        Where id=${id}  
        """.update.apply
      }
    }
  }

  def updateStartDate(id: String, date: DateTime) = {
    DB localTx { implicit session =>
      sql"""
        Update PartUsage
        Set startDate=${date.toDate}
        Where id=${id}  
        """.update.apply
    }
  }

  def delete(id: String) = {
    DB localTx { implicit session =>
      sql"""
        Delete from PartUsage
        Where id = ${id}
        """.update.apply
    }
  }

  def deletePart(partId: String) = {
    DB localTx { implicit session =>
      val pattern = s"${partId}_%"
      sql"""
        Delete from PartUsage
        Where id like ${pattern}
        """.update.apply
    }
  }
}