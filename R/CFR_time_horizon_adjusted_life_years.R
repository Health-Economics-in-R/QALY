
#' Case-Fatality Rate Determined Time Horizon
#'
#' Using this function then don't require a separate CFR
#' function to calculate the total QALYs.
#' DRY priniciple.
#'
#' @param adjusted_life_years An object of class adjusted_life_years
#' @param cfr_modelframe Data frame with CFR and ages
#'
#' @return An object of class adjusted_life_years
#' @export
#'
#' @examples
#'
#' AdjLifeYears <- adjusted_life_years(
#'                     start_year = 2016,
#'                     end_year = 2020,
#'                     age = NA,
#'                     time_horizon = NA,
#'                     utility = 0.9,
#'                     discount_rate = 0.035)
#'
#' cfr_modelframe <- model.frame(cfr ~ age, data = cfr_age_lookup)
#'
#' CFR_time_horizon.adjusted_life_years(AdjLifeYears, cfr_modelframe)
#'
CFR_time_horizon.adjusted_life_years <- function(adjusted_life_years, cfr_modelframe){

  if(!is.data.frame(cfr_modelframe)) stop("Case fatality rate data must be a data frame.")

  cfr_name <- attr(x, "names")[1]
  age_name <- attr(x, "names")[2]

  agei <- adjusted_life_years$age

  while(ltime%%1==0){

    cfr_subset <- subset(x = cfr_modelframe,
                         subset = age_name==agei,
                         select = cfr_name)

    ltime <- ltime + ifelse(cfr_subset < runif(1), 1, 0.5)

    agei <- agei + 1
  }


  adjusted_life_years$time_horizon <- min(adjusted_life_years$time_horizon, death_date)

  return(adjusted_life_years)
}
