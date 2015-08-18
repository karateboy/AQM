package models
import play.api.libs.json._
import play.api.libs.functional.syntax._
import scalikejdbc._
import scalikejdbc.config._

abstract class Rule(val shift:Int) {
  lazy val mask=1<<shift
  def isTriggered(v:Int)={
    (v & mask) != 0
  }
  def setTriggered(v:Int)={
    v | mask
  }
}

case class MinMaxCfg(
  id:MonitorType.Value,
  min:Float,
  max:Float
)

case class MinMaxRule (
  enabled:Boolean,
  monitorTypes:Seq[MinMaxCfg]    
)extends Rule(0)

object MinMaxRule{
  implicit val minMaxCfgRead = Json.reads[MinMaxCfg]
  implicit val minMaxCfgWrite = Json.writes[MinMaxCfg]
  implicit val minMaxRuleWrite = Json.writes[MinMaxRule]
  implicit val minMaxRuleRead = Json.reads[MinMaxRule]

  val default = MinMaxRule(false, Seq())
}

case class CompareRule(
  enabled:Boolean    
)extends Rule(1)

object CompareRule{
  implicit val compareRuleRead = Json.reads[CompareRule]
  implicit val compareRuleWrite = Json.writes[CompareRule]
  
  val default = CompareRule(false)
}

case class DifferenceRule(
  enabled:Boolean,
  multiplier:Float,
  monitorTypes:Seq[MonitorType.Value]  
)extends Rule(2)

object DifferenceRule{
  implicit val differenceRuleRead = Json.reads[DifferenceRule]
  implicit val differenceRuleWrite = Json.writes[DifferenceRule]
  
  val default = DifferenceRule(false, 3, Seq())
}

case class SpikeCfg(
  id:MonitorType.Value,
  abs:Float
)
case class SpikeRule(
  enabled:Boolean,
  monitorTypes:Seq[SpikeCfg]
)extends Rule(3)

object SpikeRule{
  implicit val spikeCfgRead = Json.reads[SpikeCfg]
  implicit val spikeRuleRead = Json.reads[SpikeRule]
  implicit val spikeCfgWrite = Json.writes[SpikeCfg]
  implicit val spikeRuleWrite = Json.writes[SpikeRule]
  
  val default = SpikeRule(false, Seq())
}

case class PersistenceRule(
  enabled:Boolean,
  same:Int
)extends Rule(4)

object PersistenceRule{
  implicit val persistenceRuleRead = Json.reads[PersistenceRule]
  implicit val persistenceRuleWrite = Json.writes[PersistenceRule]
  
  val default = PersistenceRule(false, 3)
}

case class AutoAudit(
    minMaxRule:MinMaxRule,
    compareRule:CompareRule,
    differenceRule:DifferenceRule,
    spikeRule:SpikeRule,
    persistenceRule:PersistenceRule
)

/**
 * @author user
 */
object AutoAudit {
  implicit val autoAuditRead = Json.reads[AutoAudit]
  implicit val autoAuditWrite = Json.writes[AutoAudit]
  
  val default = AutoAudit(
      MinMaxRule.default, 
      CompareRule.default, 
      DifferenceRule.default,
      SpikeRule.default,
      PersistenceRule.default) 
}