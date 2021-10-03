package models

/**
 * @author user
 */
case class EpaMonitor(name:String, id:Int)
object EpaMonitor extends Enumeration{
  val Erlin = Value("二林")
  val Puzi = Value("朴子")
  val Lunbei = Value("崙背")
  val Taixi = Value("臺西")
  val Mailiao = Value("麥寮")
  val DaChen = Value("大城")
  
  val map=Map(
    DaChen -> EpaMonitor("環保署大城站", 85),
    Erlin->EpaMonitor("環保署二林站", 35),
    Puzi->EpaMonitor("環保署朴子站", 40),
    Lunbei->EpaMonitor("環保署崙背站", 38),
    Taixi->EpaMonitor("環保署臺西站", 41),
    Mailiao->EpaMonitor("環保署麥寮站", 83)
  )
  
  val idMap: Map[Int, EpaMonitor.Value] = map.map(r=>(r._2.id, r._1))
  val nameMap: Map[String, EpaMonitor.Value] = map.map(r=>(r._2.name, r._1))
  val epaList = values.toList.sorted
}