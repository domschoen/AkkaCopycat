package models

import akka.event.LoggingAdapter

import java.io.{PrintWriter, StringWriter}

object Random {
  val numbers: Seq[Integer] = Seq(
    10,3,335,33,355,217,536,195,700,949,
    274,444,108,698,564,41,165,815,685,764,
    827,959,219,426,952,839,923,810,451,604,
    661,599,549,720,113,406,121,671,474,491,
    564,344,868,264,179,423,694,163,538,645,
    623,3,787,268,461,386,376,581,603,279,
    170,805,294,333,408,240,413,54,494,983,
    1,409,69,73,254,974,355,404,197,197,
    211,249,758,889,905,735,461,531,35,130,
    458,483,596,253,114,701,649,886,875,42,
    553,510,620,771,253,217,235,335,566,132,
    614,31,948,212,808,629,715,732,551,571,
    167,332,869,824,614,953,991,982,737,509,
    295,718,285,639,492,175,559,383,774,388,
    650,127,144,336,918,654,648,709,655,661,
    720,997,421,562,376,983,14,942,452,237,
    546,494,626,707,583,789,432,3,835,715,
    986,457,352,939,644,564,698,223,309,898,
    72,957,612,81,847,727,674,925,493,285,
    680,309,354,755,977,920,244,410,913,419,
    338,885,145,145,322,151,624,949,269,100,
    979,245,216,85,527,877,905,265,614,893,
    655,83,516,822,847,738,614,547,974,982,
    64,366,729,97,681,338,115,379,252,504,
    20,996,1,82,247,542,422,220,459,146,
    204,415,8,464,455,181,117,420,958,59,
    383,19,669,570,77,85,178,203,195,266,
    352,26,745,154,874,20,227,857,910,724,
    781,870,607,298,970,589,649,573,661,310,
    587,209,990,64,170,537,856,116,380,188,
    931,489,404,91,646,349,255,952,128,589,
    261,789,549,470,144,630,861,650,906,964,
    383,448,235,30,217,692,664,556,946,988,
    184,22,597,757,545,58,329,885,956,53,
    827,327,399,591,846,509,291,781,910,871,
    691,103,594,405,793,838,395,510,904,694,
    635,29,644,716,375,808,568,65,809,600,
    945,780,105,695,907,91,162,80,680,403,
    251,507,126,521,9,283,307,229,759,153,
    302,803,615,100,972,561,353,412,921,34,
    504,862,348,868,653,345,23,30,394,811,
    101,427,926,843,986,529,753,715,981,82,
    452,293,922,515,986,593,534,130,524,371,
    709,666,241,569,444,336,66,961,641,929,
    843,858,498,67,881,196,706,219,354,867,
    172,554,523,627,488,241,254,428,853,278,
    789,518,695,445,252,152,672,485,845,259,
    311,93,219,909,972,819,970,915,374,465,
    747,434,398,86,926,888,377,177,329,61,
    610,140,954,717,295,635,175,573,625,894,
    399,986,241,607,637,183,782,458,952,379,
    738,922,273,644,328,400,522,839,625,297,
    532,477,639,133,732,862,585,281,55,324,
    53,426,42,920,293,335,617,432,341,42,
    493,0,198,27,682,256,792,498,79,271,
    923,358,585,192,932,954,741,881,293,211,
    343,430,861,223,994,871,987,925,88,937,
    219,908,165,219,814,996,126,558,773,664,
    270,12,619,898,619,253,639,584,508,731,
    807,32,896,447,113,712,382,678,498,807,
    383,511,142,679,446,511,299,830,166,373,
    412,464,675,916,716,235,172,240,62,282,
    462,402,170,135,977,460,25,597,239,214,
    657,189,242,500,993,253,721,378,26,307,
    769,52,5,743,781,872,873,472,186,262,
    664,591,942,913,79,689,257,88,974,865,
    503,381,954,62,811,778,811,814,280,358,
    20,989,307,479,5,630,979,797,408,488,
    11,961,60,333,192,316,891,647,810,480,
    491,785,22,115,533,199,643,754,791,825,
    321,18,965,218,559,68,22,533,568,643,
    292,946,402,509,24,612,362,694,617,742,
    120,316,261,906,545,380,193,153,697,364,
    285,32,946,955,201,119,711,767,820,143,
    753,389,155,292,522,611,486,815,508,161,
    925,488,75,809,130,60,732,174,391,259,
    337,79,799,460,166,800,724,596,354,52,
    322,430,645,50,280,750,938,502,530,607,
    593,862,138,180,969,700,97,357,147,524,
    242,633,334,899,997,416,896,213,185,336,
    487,931,604,649,261,667,610,326,624,947,
    400,937,486,973,237,897,503,265,317,193,
    259,490,513,172,868,356,701,490,958,887,
    893,806,512,945,903,332,526,317,742,810,
    962,244,494,35,962,703,858,495,785,308,
    103,240,445,162,631,554,520,513,885,551,
    81,545,813,255,241,164,607,295,268,105,
    929,769,739,223,365,409,702,290,295,615,
    327,166,924,455,990,663,677,240,281,966,
    683,750,898,942,566,713,890,191,263,634,
    224,169,81,554,146,243,724,854,294,5,
    495,677,50,708,404,75,81,145,389,470,
    547,839,383,520,951,863,428,992,81,159,
    574,727,37,678,765,296,784,196,439,440,
    232,149,65,882,280,332,324,618,814,793,
    162,266,121,789,430,574,686,126,333,683,
    845,734,400,233,254,831,202,909,659,504,
    532,523,917,55,265,215,749,968,622,884,
    191,822,46,432,212,6,427,907,458,578,
    871,160,615,110,756,92,331,860,272,87,
    182,121,183,738,653,87,14,439,594,79,
    391,954,120,479,748,553,434,208,130,795,
    525,101,287,579,235,16,813,525,628,27,
    509,305,171,533,728,2,655,640,886,100,
    362,658,880,190,416,42,631,994,808,646,
    452,797,54,604,484,877,950,711,899,36,
    414,627,105,819,850,755,659,512,40,141,
    110,525,2,228,497,234,797,821,714,903,
    763,102,465,749,115,29,19,769,417,983,
    499,408,861,983,907,667,851,336,624,749,
    835,47,573,822,872,844,802,12,892,156,
    674,208,132,889,395,773,688,351,108,756,
    694,153,252,706,50,452,419,669,210,353,
    426,472,970,848,818,880,735,95,515,729,
    234,419,840,619,154,852,624,268,658,573,
    622,116,248,75,956,277,966,421,800,425,
    370,418,28,724,284,63,44,371,761,97,
    510,873,260,12,202,627,477,367,690,919,
    814,732,86,913,953,132,563,748,502,310,
    867,316,181,672,546,827,519,245,105,746,
    526,499,819,211,318,371,404,505,153,340,
    290,687,854,895,686,987,86,238,182,29,
    456,568,92,505,899,786,253,385,327,976,
    515,238,13,483,628,794,658,961,747,305,
    639,210,274,819,381,160,259,963,335,840,
    80,309,534,676,604,937,184,966,578,284,
    263,870,440,862,609,759,282,952,439,278,
    187,862,803,946,649,840,904,656,303,246,
    789,914,676,60,346,147,187,336,105,946,
    108,691,791,572,703,413,109,571,279,297,
    725,851,337,792,913,49,581,558,841,546,
    970,22,948,970,550,35,410,524,284,210,
    137,993,583,54,279,759,787,469,714,693,
    276,723,485,239,467,493,649,550,388,61,
    511,20,67,259,714,47,436,267,150,384,
    990,846,160,903,904,102,2,138,826,386,
    31,431,85,403,598,33,501,143,94,14,
    547,719,145,302,59,705,399,716,223,72,
    894,330,61,312,462,7,123,846,141,450,
    614,306,75,620,674,795,846,855,23,702,
    738,535,691,183,875,647,67,891,718,80,
    959,47,880,2,905,461,734,163,451,576,
    785,463,599,148,263,629,169,634,917,357,
    269,910,476,738,609,386,744,341,869,127,
    795,772,846,68,249,68,590,922,77,764,
    256,868,928,979,888,614,423,357,851,940,
    408,223,135,291,612,780,245,542,697,700,
    897,507,159,956,244,295,50,227,837,109,
    996,732,61,573,468,831,241,188,787,235,
    156,11,821,171,170,146,855,350,700,144,
    589,74,241,903,744,330,667,168,238,211,
    25,217,403,521,544,829,214,884,75,134,
    993,224,678,429,563,945,582,562,319,667,
    234,969,179,842,648,972,894,119,788,409,
    675,783,189,220,617,116,543,51,877,32,
    466,497,85,816,167,55,948,325,234,403,
    525,482,258,76,132,415,518,149,354,84,
    434,20,126,736,20,874,360,34,48,938,
    76,202,143,104,514,37,529,34,197,120,
    644,318,401,76,344,801,21,234,911,652,
    280,513,590,285,267,895,866,719,765,321,
    209,469,337,352,493,565,139,442,826,789,
    437,170,989,735,282,264,217,765,324,774,
    541,479,231,227,395,769,677,116,730,73,
    368,557,328,905,90,470,558,267,222,117,
    277,26,700,347,100,863,905,389,761,870,
    425,538,778,411,175,580,147,319,841,287,
    272,35,112,852,868,787,674,578,38,382,
    432,680,256,733,926,554,465,693,419,327,
    13,298,616,64,651,9,484,453,299,62,
    153,327,809,489,346,848,444,938,333,725,
    287,861,846,281,294,178,960,725,228,998,
    376,485,170,511,92,917,842,616,937,263,
    459,408,910,184,312,96,827,716,831,463,
    216,996,318,438,947,599,80,909,555,827,
    191,332,733,802,582,256,958,697,692,355,
    719,956,393,46,318,842,411,301,715,161,
    958,746,380,505,797,628,621,847,455,759,
    367,856,809,27,857,747,757,558,223,33,
    194,131,422,7,22,948,488,311,390,330,
    580,419,910,456,788,396,49,725,165,767,
    420,839,961,839,196,202,518,320,358,724,
    803,585,3,631,44,427,425,366,437,636,
    190,188,395,254,929,539,308,846,54,807,
    279,634,132,543,808,837,122,753,894,54,
    207,344,520,47,426,322,989,755,835,836,
    88,892,409,663,541,965,414,225,716,62,
    449,731,70,253,901,544,284,940,11,358,
    569,12,279,287,102,141,428,452,67,62,
    643,171,272,185,301,325,804,381,918,25,
    692,285,526,138,51,961,197,799,673,269,
    945,477,533,788,706,704,183,622,630,53,
    494,950,396,468,725,652,775,524,379,861,
    153,730,826,783,234,448,143,625,645,545,
    162,979,663,351,181,509,778,27,305,276,
    308,935,894,552,796,105,88,462,398,392,
    864,298,986,413,610,860,178,862,671,645,
    25,301,849,252,559,3,317,830,940,971,
    914,920,397,690,295,535,703,741,911,815,
    392,841,266,114,440,428,445,846,621,365)
  var  rndseed: Integer = 1
  def rnd(log: LoggingAdapter): Double = {
    Thread.sleep(1)
    printSeed(log)
    val value = numbers(rndseed) / 1000.0
    rndseed += 1
    if (rndseed>1999) rndseed = 0
    value
  }

  private def printSeed(log: LoggingAdapter) = {
    if (rndseed == -1) {
      try {
        throw new Exception("toto")
      } catch {
        case e: Exception =>
          val sw = new StringWriter()
          e.printStackTrace(new PrintWriter(sw))
          println("H" + sw.toString)
      }
    }
    if (log == null) {
      println(s"Random rndseed=$rndseed")
    } else {
      log.debug(s"Random rndseed=$rndseed")
    }
  }

  def setseed(value: Integer) = {  rndseed = value }
}