package models
import play.api.libs.json._
import play.api.libs.functional.syntax._
import scalikejdbc._
import scalikejdbc.config._


case class MinMaxCfg(
  mt:MonitorType.Value,
  min:Float,
  max:Float
)

case class MinMaxRule(
  enabled:Boolean,
  monitorTypes:Seq[MinMaxCfg]    
)

object MinMaxRule {
  implicit val minMaxCfgRead = Json.reads[MinMaxCfg]
  implicit val minMaxCfgWrite = Json.writes[MinMaxCfg]
  implicit val minMaxRuleWrite = Json.writes[MinMaxRule]
  implicit val minMaxRuleRead = Json.reads[MinMaxRule]

  val default = MinMaxRule(false, Seq())
}

case class CompareRule(
  enabled:Boolean    
)

object CompareRule{
  implicit val compareRuleRead = Json.reads[CompareRule]
  implicit val compareRuleWrite = Json.writes[CompareRule]
  
  val default = CompareRule(false)
}

case class DifferenceRule(
  enabled:Boolean,
  multiplier:Float,
  monitorTypes:Seq[MonitorType.Value]  
)

object DifferenceRule{
  implicit val differenceRuleRead = Json.reads[DifferenceRule]
  implicit val differenceRuleWrite = Json.writes[DifferenceRule]
  
  val default = DifferenceRule(false, 3, Seq())
}

case class SpikeRule(
  enabled:Boolean,
  monitorTypes:Seq[MonitorType.Value]
)

object SpikeRule{
  implicit val spikeRuleRead = Json.reads[SpikeRule]
  implicit val spikeRuleWrite = Json.writes[SpikeRule]
  
  val default = SpikeRule(false, Seq())
}

case class PersistenceRule(
  enabled:Boolean,
  same:Int
)

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