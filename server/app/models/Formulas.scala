package models

object Formulas {

  def weighted_average(v1: Double, w1: Double, v2: Double, w2: Double) = {
     ((v1*w1)+(v2*w2))/(w1+w2)
  }

  def weighted_average(v1: Double, w1: Double, v2: Double, w2: Double, v3: Double, w3: Double) = {
    ((v1*w1)+(v2*w2)+(v3*w3))/(w1+w2+w3)
  }

  def max(x: Double, y: Double): Double = if (x>y) x else y

  def min(x: Double, y: Double): Double = if (x<y) x else y

  def temperatureAdjustedValue(value: Double, temperature: Double) = Math.pow(value,((100.0-temperature)/30.0)+0.5)


  def temperature_adjusted_probability(value: Double, temperature: Double): Double ={
    // if the temperature is 0, no adjustment is made
    // otherwise, values above .5 are lowered and values below .5 are raised
    //System.out.println("the log of 100,10"+Math.pow(100.0,0.1));
    var rval = 0.0
    //var t = 0.0
    rval= value
    if (value==0.0) return 0.0;
    if (value==0.5) return 0.0;
    else if (value<0.5){
      //gets lowered the lower the temperature
      // t = max(Math.abs(Math.log(val,10.0))
      return 1.0-temperature_adjusted_probability(1.0-value, temperature)
    }
    else if (value>0.5){
      //gets raised the lower the temperature
      rval = max(1.0-((1.0-value)+((10-Math.sqrt(100.0-temperature))/
        100.0)*(1.0 - (1.0 - value))),0.5);

    }

    /* else if (value<0.5){
       //gets lowered the lower the temperature
       t = (Math.sqrt(100-Temperature.value))/10;
       rval=0.5-val;
       rval=rval*t;
       rval=0.5-rval;

     }
     else if (val>0.5){
       //gets raised the lower the temperature
       t = (Math.sqrt(100-Temperature.value))/10;
       rval=val-0.5;
       rval=rval*t;
       rval=0.5+rval;
     }*/
    //System.out.println("Temperature adjusted value.  Temp="+Temperature.value+" t:"+t+" inital val="+val+" adjusted val="+rval);
    return rval;
  }

}
