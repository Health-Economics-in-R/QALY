
#' Case-Fatality Rate Determined Time Horizon
#'
#' Using this function then don't require a separate CFR
#' function to calculate the total QALYs.
#' DRY priniciple.
#'
#' @param adj_lyears An object of class adjusted_life_years
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
CFR_time_horizon.adjusted_life_years <- function(adj_lyears,
                                                 cfr_modelframe){

  if (!is.data.frame(cfr_modelframe))
    stop("Case fatality rate data must be a data frame.")

  cfr_name <- attr(x, "names")[1]
  age_name <- attr(x, "names")[2]

  agei <- adj_lyears$age
  death_date <- 0

  while (death_date %% 1 == 0) {

    cfr_subset <- subset(x = cfr_modelframe,
                         subset = age_name == agei,
                         select = cfr_name)

    death_date <- death_date + ifelse(cfr_subset < runif(1), 1, 0.5)

    agei <- agei + 1
  }

  adj_lyears$time_horizon <- min(adj_lyears$time_horizon,
                                          death_date)

  adj_lyears$utility <-
    fillin_missing_utilities(adj_lyears$utility,
                             adj_lyears$time_horizon)

  adj_lyears$death <- TRUE

  return(adj_lyears)
}
