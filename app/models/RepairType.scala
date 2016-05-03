package models

object RepairType extends Enumeration{
  val Equipment = Value
  val Measurement = Value
  val Software = Value
  
  val map = Map(Equipment ->"儀器", Measurement->"測值", Software->"軟體")
  
  val subTypeList: List[String] = MonitorType.mtvAllList.map { MonitorType.map(_).desp } ++ 
    List("其他", "無數據", "計算", "報表", "註記")
}
