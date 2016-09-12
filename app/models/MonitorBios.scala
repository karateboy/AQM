package models
case class MonitorBios(
  location:String,
  monitor_type:String,
  address:String,
  town:String,
  area:String,
  density:String,
  founded:String,
  height:String,
  angle:String,
  sample_height:String,
  distance:String
)
object MonitorBios {
  val A001 = MonitorBios(
    location="頂庄國小(彰化縣)",
    monitor_type="一般測站",
    address="彰化縣大城鄉東厝路63之1號",
    town="大城鄉",
    area="63.7(平方公里)",
    density="285(人/平方公里)",
    founded="101年12月",
    height="7公尺",
    angle="360度",
    sample_height="11.3公尺",
    distance="15公尺"
  )
  val A002 = MonitorBios(
    location="嘉新國中(嘉義縣)",
    monitor_type="工業測站",
    address="嘉義縣太保市北新里017鄰中興路一段250號",
    town="太保市",
    area="66.9(平方公里)",
    density="553(人/平方公里)",
    founded="102年12月",
    height="8公尺",
    angle="360度",
    sample_height="12.3公尺",
    distance="20公尺"
  )
  val A003 = MonitorBios(
    location="東榮國中(嘉義縣)",
    monitor_type="一般測站",
    address="嘉義縣東石鄉永屯村39-2號",
    town="東石鄉",
    area="81.6(平方公里)",
    density="367(人/平方公里)",
    founded="102年09月",
    height="11公尺",
    angle="360度",
    sample_height="15.5公尺",
    distance="10公尺"
  )
  val A004 = MonitorBios(
    location="I棟宿舍(雲林縣)",
    monitor_type="工業測站",
    address="雲林縣麥寮鄉三盛村台塑工業園區1-1號",
    town="麥寮鄉",
    area="106(平方公里)",
    density="519(人/平方公里)",
    founded="101年12月",
    height=" 23公尺",
    angle="360度",
    sample_height="25.5公尺",
    distance=" 90公尺"
  )
  val A005 = MonitorBios(
    location="龍巖國小(雲林縣)",
    monitor_type="一般測站",
    address="雲林縣褒忠鄉民生路28巷15號",
    town="褒忠鄉",
    area="37.1(平方公里)",
    density="370(人/平方公里)",
    founded="101年12月",
    height="7公尺",
    angle="360度",
    sample_height="11.3公尺",
    distance=" 205公尺"
  )
  
  val A006 = MonitorBios(
    location="豐榮宿舍(雲林縣)",
    monitor_type="一般測站",
    address="雲林縣崙背鄉豐榮村21號",
    town="崙背鄉",
    area="58.5(平方公里)",
    density=" 451(人/平方公里)",
    founded="101年12月",
    height="10.8公尺",
    angle="360度",
    sample_height="15.5公尺",
    distance="300公尺"
  )

  val A007 = MonitorBios(
    location="內湖國小(雲林縣)",
    monitor_type="一般測站",
    address="雲林縣四湖鄉三塊厝123號",
    town="四湖鄉",
    area="77.1(平方公里)",
    density="334(人/平方公里)",
    founded="101年12月",
    height="7公尺",
    angle="360度",
    sample_height=" 11.3公尺",
    distance="31公尺"
  )
  
  val A008 = MonitorBios(
    location="東勢鄉公所(雲林縣)",
    monitor_type="一般測站",
    address="雲林縣東勢鄉所前街3號",
    town="東勢鄉",
    area="48.4(平方公里)",
    density="330(人/平方公里)",
    founded="101年12月",
    height="11公尺",
    angle="360度",
    sample_height="15.5公尺",
    distance="36公尺"
  )
  
  val A009 = MonitorBios(
    location="麥寮高中(雲林縣)",
    monitor_type="一般測站",
    address="雲林縣麥寮鄉中興路310號",
    town="麥寮鄉",
    area="80.1668(平方公里)",
    density="427.5585 (人/平方公里)",
    founded="102年8月",
    height="13.6公尺",
    angle="360度",
    sample_height="17.8公尺",
    distance="25公尺"
  )
  
  val A010 = MonitorBios(
    location="台西國中(雲林縣)",
    monitor_type="一般測站",
    address="雲林縣台西鄉中山路408號",
    town="台西鄉",
    area="54.0983(平方公里)",
    density="495.2651(人/平方公里)",
    founded="102年10月",
    height="10.5公尺",
    angle="360度",
    sample_height="14.6公尺",
    distance="100公尺"
  )
  
  val A011 = MonitorBios(
    location="宏崙國小(雲林縣)",
    monitor_type="一般測站",
    address="雲林縣土庫鎮崙內里稞圍41號",
    town="土庫鎮",
    area="49.0212(平方公里)",
    density="629.7071(人/平方公里)",
    founded="102年8月",
    height="7.2公尺",
    angle="360度",
    sample_height="11.4公尺",
    distance="15公尺"
  )
  
  val A012 = MonitorBios(
    location="廣興國小(雲林縣)",
    monitor_type="一般測站",
    address="雲林縣西螺鎮648廣興里59號",
    town="西螺鎮",
    area="49.7985(平方公里)",
    density="629.7071(人/平方公里)",
    founded="104年10月",
    height="7.2公尺",
    angle="360度",
    sample_height="11.4公尺",
    distance="15公尺"
  )
  val A013 = MonitorBios(
    location="-",
    monitor_type="一般測站",
    address="-",
    town="-",
    area="-",
    density="-",
    founded="102年09月",
    height="10.5公尺",
    angle="360度",
    sample_height="11.3公尺",
    distance="-"
  )
 
  val map = Map(
      Monitor.withName("A001")->A001,
      Monitor.withName("A002")->A002,
      Monitor.withName("A003")->A003,
      Monitor.withName("A004")->A004,
      Monitor.withName("A005")->A005,
      Monitor.withName("A006")->A006,
      Monitor.withName("A007")->A007,
      Monitor.withName("A008")->A008,
      Monitor.withName("A009")->A009,
      Monitor.withName("A010")->A010,
      Monitor.withName("A011")->A011,
      Monitor.withName("A012")->A012,
      Monitor.withName("A013")->A013
      )
}