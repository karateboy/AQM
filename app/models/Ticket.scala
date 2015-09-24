package models
import scalikejdbc._
import play.api._
import com.github.nscala_time.time.Imports._
import models.ModelHelper._
import models._
import play.api.libs.json._
import play.api.libs.functional.syntax._
import play.api.libs.json.Json

object TicketType extends Enumeration{
  val maintance_week = Value
  val maintance_biweek = Value
  val maintance_month = Value
  val maintance_quarter = Value
  val maintance_year = Value
  val repair = Value

  val map = Map(repair->"維修", maintance_week->"單周定保", maintance_biweek->"雙周定保", 
      maintance_month->"月定保", maintance_quarter->"季定保", maintance_year->"年定保")
      
  def withId(id:Int)={
    if(id == repair.id)
      repair
    else if(id == maintance_week.id)
      maintance_week
    else if(id == maintance_biweek.id)
      maintance_biweek
    else if(id == maintance_month.id)
      maintance_month
    else if(id == maintance_quarter.id)
      maintance_quarter
    else if(id == maintance_year.id)
      maintance_year
    else
      throw new NoSuchElementException
  }
}

case class Ticket(id:Int, submit_date:DateTime, active:Boolean, ticketType:TicketType.Value, submiter_id:Int, owner_id:Int, monitor:Monitor.Value, 
    monitorType:MonitorType.Value)
/**
 * @author user
 */
object Ticket {
  def newTicket(ticket: Ticket)(implicit session: DBSession = AutoSession) = {
    DB localTx { implicit session =>
      sql"""
        Insert into Ticket(
        [submit_date]
        ,[active]
        ,[ticketType]
        ,[submiter_id]
        ,[owner_id]
        ,[monitor]
        ,[monitorType])
        Values(${ticket.submit_date}, ${ticket.active}, ${ticket.ticketType.toString}, 
          ${ticket.submiter_id}, ${ticket.owner_id}, ${ticket.monitor.toString}, ${ticket.monitorType.toString})
        """.update.apply
    }
  }
  
  def allTickets(implicit session: DBSession = AutoSession) = {
    sql"""
      Select *
      From Ticket
      Order by submit_date      
      """.map { r =>
      Ticket(r.int(1), r.timestamp(2), r.boolean(3), TicketType.withId(r.int(4)), 
          r.int(5), r.int(6), Monitor.withName(r.string(7)), MonitorType.withName(r.string(8)))
    }.list().apply()
  }
  
}