package models

/**
 * @author user
 */
case class EpaMonitor(name:String, idx:Int)
object EpaMonitor extends Enumeration{
  val Erlin = Value("Erlin")
  val Puzi = Value("Puzi")
  val Lunbei = Value("Lunbei")
  val Taixi = Value("Taixi")
  val Mailiao = Value("Mailiao")
  
  val map=Map(
    Erlin->EpaMonitor("二林", 35),
    Puzi->EpaMonitor("朴子", 40),
    Lunbei->EpaMonitor("崙背", 38),
    Taixi->EpaMonitor("臺西", 41),
    Mailiao->EpaMonitor("麥寮", 83)
  )
  
  val epaList = values.toList.sorted
}