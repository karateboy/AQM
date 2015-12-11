package models

object InstrumentThreshold extends Enumeration {
  val H370_THC_Signal = Value
  val H370_CH4_Signal = Value
  val H370_Zero_Compension = Value
  val H370_PurifierTemp = Value
  val H370_NMHCOutTemp = Value
  val H370_5V = Value
  val H370_24V = Value
  val H370_SampleFlow = Value
  val H370_OverFlow = Value
  val H370_PumpPress = Value
  val H370_PurifierPress = Value
  val H370_AtmosPress = Value
  val T100_RANGE = Value
  val T100_STABIL = Value
  val T100_STABIL2 = Value
  val T100_PRES = Value
  val T100_SAMP_FL = Value
  val T100_PMT = Value
  val T100_NORM_PMT = Value
  val T100_UV_LAMP = Value
  val T100_UV_STB = Value
  val T100_LAMP_RATIO = Value
  val T100_STR_LGT = Value
  val T100_DRK_PMT = Value
  val T100_DRK_LMP = Value
  val T100_SLOPE = Value
  val T100_OFFSET = Value
  val T100_HVPS = Value
  val T100_RCELL_TEMP = Value
  val T100_BOX_TEMP = Value
  val T100_PMT_TEMP = Value
  val T100_TEST = Value
  val T200_NO = Value
  val T200_NOX = Value
  val T200_RANGE = Value
  val T200_NOX_STB = Value
  val T200_SAMP_FLW = Value
  val T200_OZONE_FL = Value
  val T200_PMT = Value
  val T200_NORM_PMT = Value
  val T200_AZERO = Value
  val T200_HVPS = Value
  val T200_RCELL_TEMP = Value
  val T200_BOX_TEMP = Value
  val T200_PMT_TEMP = Value
  val T200_MOLY_TEMP = Value
  val T200_RCEL = Value
  val T200_SAMP = Value
  val T200_NOX_SLOPE = Value
  val T200_NOX_OFFS = Value
  val T200_NO_SLOPE = Value
  val T200_NO_OFFS = Value
  val T300_RANGE = Value
  val T300_STABIL = Value
  val T300_CO_MEAS = Value
  val T300_CO_REF = Value
  val T300_MR_RATIO = Value
  val T300_PRES = Value
  val T300_SAMP_FL = Value
  val T300_SAMPLE_TEMP = Value
  val T300_BENCH_TEMP = Value
  val T300_WHEEL_TEMP = Value
  val T300_BOX_TEMP = Value
  val T300_PHT_DRIVE = Value
  val T300_SLOPE = Value
  val T300_OFFSET = Value
  val T400_RANGE = Value
  val T400_STABIL = Value
  val T400_O3_MEAS = Value
  val T400_O3_REF = Value
  val T400_PRES = Value
  val T400_SAMP_FL = Value
  val T400_SAMPLE_TEMP = Value
  val T400_PHOTO_LAMP = Value
  val T400_BOX_TEMP = Value
  val T400_SLOPE = Value
  val T400_OFFSET = Value
  val TSP_CONC = Value
  val TSP_QTOT = Value
  val TSP_RH = Value
  val TSP_AT = Value  
  val PM10_CONC = Value
  val PM10_QTOT = Value
  val PM10_RH = Value
  val PM10_AT = Value


  private def getValue(strKey:String)={
    val value = SystemConfig.getConfig(strKey, "")
    if(value.length == 0)
      None
    else
      Some(value.toFloat)
  }
  
  def setValue(strKey:String, value:Option[Float]) = {
    if(value.isDefined)
      SystemConfig.setConfig(strKey, value.get.toString)
    else
      SystemConfig.setConfig(strKey, "")
  }
  
  def getMax(key: InstrumentThreshold.Value) = getValue(s"${key.toString}_Max")
  def getMin(key: InstrumentThreshold.Value) = getValue(s"${key.toString}_Min")
  
  def setMax(key: InstrumentThreshold.Value, value:Some[Float]) = setValue(s"${key.toString}_Max", value)
  def setMin(key: InstrumentThreshold.Value, value:Some[Float]) = setValue(s"${key.toString}_Min", value)
  
  def getStyle(key: InstrumentThreshold.Value, value:Float)={
    val maxOpt = getMax(key)
    val minOpt = getMin(key)
    if((maxOpt.isDefined && value > maxOpt.get) ||
        (minOpt.isDefined && value < minOpt.get))
      s"Color:Red"
    else 
      s"Color:Black"
  }
  
  def getStyle(key: InstrumentThreshold.Value, valueOpt:Option[Float]):String={
    if(valueOpt.isEmpty)
      return s"Color:Red"
      
    val value = valueOpt.get
    val maxOpt = getMax(key)
    val minOpt = getMin(key)
    if((maxOpt.isDefined && value > maxOpt.get) ||
        (minOpt.isDefined && value < minOpt.get))
      s"Color:Red"
    else 
      s"Color:Black"
  }
  
  def formatV(fmt:String, valueOpt:Option[Float])={
    if(valueOpt.isEmpty)
      "-"
    else{
      val value = valueOpt.get
      fmt.format(value)
    }

  }
  
}