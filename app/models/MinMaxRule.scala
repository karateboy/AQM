package models
import play.api.libs.json._
import play.api.libs.functional.syntax._

/**
 * @author user
 */

case class MinMaxCfg(
  mt:MonitorType.Value,
  min:Float,
  max:Float
)

case class MinMaxRule(
  monitorTypes:Seq[MinMaxCfg]    
)

object MinMaxRule {
  implicit val minMaxCfgRead = Json.reads[MinMaxCfg]
  implicit val minMaxCfgWrite = Json.writes[MinMaxCfg]
  implicit val minMaxRuleWrite = Json.writes[MinMaxRule]
  implicit val minMaxRuleRead = Json.reads[MinMaxRule]
  
}