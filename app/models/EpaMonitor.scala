package models

/**
 * @author user
 */
case class EpaMonitor(name:String, id:Int)
object EpaMonitor extends Enumeration{
  val Erlin = Value("Erlin")
  val Puzi = Value("Puzi")
  val Lunbei = Value("Lunbei")
  val Taixi = Value("Taixi")
  val Mailiao = Value("Mailiao")
  
  val map=Map(
    Erlin->EpaMonitor("二林站", 35),
    Puzi->EpaMonitor("朴子站", 40),
    Lunbei->EpaMonitor("崙背站", 38),
    Taixi->EpaMonitor("臺西站", 41),
    Mailiao->EpaMonitor("麥寮站", 83)
  )
  
  val idMap = map.map(r=>(r._2.id, r._1))
  
  val epaList = values.toList.sorted
}