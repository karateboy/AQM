package models

import com.github.nscala_time.time.Imports._
import models.ModelHelper._
import scalikejdbc._

case class EventLog(evtTime: DateTime, evtType: Int, evtDesc: String)

object EventLog {
  val evtTypeManualAudit = 1
  val evtTypeInformAlarm = 2
  val evtTypeDueAlarm = 3
  val evtTypeGetEpaData = 4

  val map = Map(
    evtTypeManualAudit -> "人工註記",
    evtTypeInformAlarm -> "警報通報",
    evtTypeDueAlarm -> "逾期案件",
    evtTypeGetEpaData -> "擷取環保署資料"
  )

  def evtTypeToStr(evtType: Int) = {
    map.getOrElse(evtType, s"未知事件類別:${evtType}")
  }

  def getList(eventTypes: Seq[Int], start: DateTime, end: DateTime)(implicit session: DBSession = AutoSession) = {
    val eventTypeStr = SQLSyntax.createUnsafely(eventTypes.mkString("('", "','", "')"))
    sql"""
        Select * 
        From eventLog
        Where evtTime Between ${start} and ${end} and evtType in $eventTypeStr
        ORDER BY evtTime DESC
        """.map { r =>
      EventLog(r.timestamp(1), r.int(2), r.string(3))
    }.list.apply
  }

  def create(evt: EventLog)(implicit session: DBSession = AutoSession) = {
    try{
      val tm:java.sql.Timestamp = DateTime.now
      sql"""
        Insert into eventLog(evtTime, evtType, evtDesc)
        Values(${tm}, ${evt.evtType}, ${evt.evtDesc})
        """.update.apply
    }catch{
      case _:Throwable =>
        //ignore duplicate exception...
    }
  }
}