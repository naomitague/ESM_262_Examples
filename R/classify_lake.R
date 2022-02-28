#' classify_lake
#'
#' compute net present value
#' @param nutrient concentraion (mg/L) (at least 5 days)
#' @param temperature (C) (at least 5 days)
#' @param nutrient_threshold_med (mg/L) (default 5)
#' @param nutrient_threshold_high (mg/L) (default 8)
#' @param temperature_threshold (mg/L) (default 10)
#' @return risk (high,med, low), mean nutrient concentration (mg/L)


classify_lake = function(nutrient, temperature, nutrient_threshold_med=5,
                         nutrient_threshold_high=8, temperature_threshold=10) {

  # start with some error checking
  if (length(nutrient) < 5)
    return("Not enough nutrient measurements, at least 5 are needed")

  if (length(temperature) < 5)
      return("Not enought temperature measurements, at least 5 are needed")


  # a while loop is useful here
  numb10 = 0
  i=1
  # we use while here because we want to exit our loop any time we get more than 5 days with
  # air temperature greater than the threshold

  while ( (numb10 < 5) && (i <= length(temperature))) {
      if (temperature[i] > temperature_threshold)
        # we have another day with greater than 10 so accumulate
        numb10 = numb10+1
      else
        # we have to start over
        numb10 = 0
      # remember with while loops we need to increment our counter
      i = i+1

  }

  # compute the mean nutrient
  mean_nutrient = mean(nutrient)

  # only high or med if temperature has been 10 degrees for more than 5 days
  if (numb10 >= 5) {
   risk= case_when ( mean_nutrient < nutrient_threshold_med ~ "low",
                     mean_nutrient >= nutrient_threshold_med &
                       mean_nutrient < nutrient_threshold_high ~ "med",
                mean_nutrient >= nutrient_threshold_high ~ "high" )
  } else
    risk = "low"


  return(list(risk=risk, mean_nutrient=mean_nutrient))
}

