package controllers
import akka.actor._

object MinMonthlyReportWorker {
  def props(out: ActorRef) = Props(new MinMonthlyReportWorker(out))
}
 
class MinMonthlyReportWorker(out: ActorRef) extends Actor {
  object CmdType extends Enumeration{
    val start = Value
    val progress = Value
  }
  
  var progress: Int = _
  def getCmdType(msg:String)={
    val param = msg.split(":")
    CmdType.withName(param(0))
  }
  
  def receive = {
    case msg: String if getCmdType(msg) == CmdType.start=>      
      out ! ("Start preparing report: ")
  }
}