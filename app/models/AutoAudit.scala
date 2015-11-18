package models
import play.api.libs.json._
import play.api.libs.functional.syntax._
import scalikejdbc._
import scalikejdbc.config._

abstract class Rule(val lead:Char)

case class MinMaxCfg(
  id:MonitorType.Value,
  min:Float,
  max:Float
)

case class MinMaxRule (
  enabled:Boolean,
  monitorTypes:Seq[MinMaxCfg]    
)extends Rule('a')

object MinMaxRule{
  implicit val minMaxCfgRead = Json.reads[MinMaxCfg]
  implicit val minMaxCfgWrite = Json.writes[MinMaxCfg]
  implicit val minMaxRuleWrite = Json.writes[MinMaxRule]
  implicit val minMaxRuleRead = Json.reads[MinMaxRule]

  val default = MinMaxRule(false, Seq())
}

case class CompareRule(
  enabled:Boolean    
)extends Rule('b')

object CompareRule{
  implicit val compareRuleRead = Json.reads[CompareRule]
  implicit val compareRuleWrite = Json.writes[CompareRule]
  
  val default = CompareRule(false)
}

case class DifferenceRule(
  enabled:Boolean,
  multiplier:Float,
  monitorTypes:Seq[MonitorType.Value]  
)extends Rule('c')

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
)extends Rule('d')

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
)extends Rule('e')

object PersistenceRule{
  implicit val persistenceRuleRead = Json.reads[PersistenceRule]
  implicit val persistenceRuleWrite = Json.writes[PersistenceRule]
  
  val default = PersistenceRule(false, 3)
}

case class MonoCfg(
    id:MonitorType.Value,
    abs:Float
)

case class MonoRule(enabled:Boolean, count:Int, 
    monitorTypes:Seq[MonoCfg])extends Rule('f')
object MonoRule{
  implicit val monoCfgRead = Json.reads[MonoCfg]
  implicit val monoCfgWrite = Json.writes[MonoCfg]
  implicit val monoRuleRead = Json.reads[MonoRule]
  implicit val monoRuleWrite = Json.writes[MonoRule]
  
  val default = MonoRule(false, 3, Seq.empty[MonoCfg])
  
}

case class TwoHourRule(enabled:Boolean, monitorTypes:Seq[MonoCfg])extends Rule('g')
object TwoHourRule{
  import MonoRule._
  implicit val read = Json.reads[TwoHourRule]
  implicit val write = Json.writes[TwoHourRule]
  val default = TwoHourRule(false, Seq.empty[MonoCfg])
}

case class ThreeHourCfg(
    id:MonitorType.Value,
    abs:Float,
    percent:Float
)
case class ThreeHourRule(enabled:Boolean, monitorTypes:Seq[ThreeHourCfg])extends Rule('h')
object ThreeHourRule{
  implicit val thcfgRead = Json.reads[ThreeHourCfg]
  implicit val thcfgWrite = Json.writes[ThreeHourCfg]
  implicit val reads = Json.reads[ThreeHourRule]
  implicit val writes = Json.writes[ThreeHourRule]
  
  val default = ThreeHourRule(false, Seq.empty[ThreeHourCfg])
}

case class FourHourCfg(
    id:MonitorType.Value,
    abs:Float)

case class FourHourRule(enabled:Boolean, monitorTypes:Seq[FourHourCfg])extends Rule('i')
object FourHourRule{
  implicit val thcfgRead = Json.reads[FourHourCfg]
  implicit val thcfgWrite = Json.writes[FourHourCfg]
  implicit val reads = Json.reads[FourHourRule]
  implicit val writes = Json.writes[FourHourRule]
  
  val default = FourHourRule(false, Seq.empty[FourHourCfg])
}
case class AutoAudit(
    minMaxRule:MinMaxRule,
    compareRule:CompareRule,
    differenceRule:DifferenceRule,
    spikeRule:SpikeRule,
    persistenceRule:PersistenceRule,
    monoRule:MonoRule,
    twoHourRule:TwoHourRule,
    threeHourRule:ThreeHourRule,
    fourHourRule:FourHourRule
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
      PersistenceRule.default,
      MonoRule.default,
      TwoHourRule.default,
      ThreeHourRule.default,
      FourHourRule.default) 
      
  val map= Map(
    'a' -> "極大極小值",
    'b' -> "合理性",
    'c' -> "單調性",
    'd' -> "突波高值",
    'e' -> "持續性",
    'f' -> "一致性",
    'g' -> "小時測值變化驗證",
    'h' -> "三小時變化測值驗證",
    'i' -> "四小時變化測值驗證"
  )
}