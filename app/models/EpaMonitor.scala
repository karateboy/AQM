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
  val Douliu = Value("斗六")
  
  val map=Map(
    DaChen -> EpaMonitor("環境部大城站", 85),
    Erlin->EpaMonitor("環境部二林站", 35),
    Puzi->EpaMonitor("環境部朴子站", 40),
    Lunbei->EpaMonitor("環境部崙背站", 38),
    Taixi->EpaMonitor("環境部臺西站", 41),
    Mailiao->EpaMonitor("環境部麥寮站", 83),
    Douliu ->EpaMonitor("環境部斗六站", 37)
  )
  
  val idMap: Map[Int, EpaMonitor.Value] = map.map(r=>(r._2.id, r._1))
  val nameMap: Map[String, EpaMonitor.Value] = map.map(r=>(r._2.name, r._1))
  val epaList = values.toList.sorted
}