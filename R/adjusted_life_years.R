
#' Adjusted Life Years Object Constructor
#'
#' For use as input to the QALY and DALY functions.
#'
#' @param start_year Calendar year to begin calculation
#' @param end_year Calendar year to end calculation
#' @param age Age at start of period
#' @param time_horizon Number of time periods from start to end date
#' @param utility Proportion health detriment
#' @param discount_rate Fixed proportion reduction over time
#'
#' @return Object of class adjusted_life_years
#' @export
#'
#' @seealso \code{\link{QALY}}, \code{\link{DALY}}
#'
#' @examples
#'
#' AdjLifeYears <- adjusted_life_years(
#'                     start_year = 2016,
#'                     end_year = 2020,l
#'                     age = NA,
#'                     time_horizon = NA,
#'                     utility = 0.9,
#'                     discount_rate = 0.035)
#'
#' total_QALYs(AdjLifeYears)
#' ## 2.913622
#'
#' total_QALY(1)
#' ## "Error: Not an adjusted_life_years class input object."
#'
adjusted_life_years <- function(start_year = 0,
                                end_year = NA,
                                age = NA,
                                time_horizon = NA,
                                utility,
                                discount_rate = 0.035){

  if(start_year<0) stop("Start year must be non-negative.")
  if(!is.na(end_year) & end_year<0) stop("End year must be non-negative.")
  if(!is.na(time_horizon) & time_horizon<0) stop("Time horizon must be non-negative.")

  if(!is.na(age) & age<0) stop("Age must be non-negative.")

  # if(!is.na(start_year)) start_year <- floor(start_year)
  # if(!is.na(end_year)) end_year <- floor(end_year)

  if(any(utility>1) | any(utility<0)){
    stop("utility must be between 0 and 1.")
  }

  if(any(discount_rate>1) | any(discount_rate<0)){
    stop("Discount factors must be between 0 and 1.")
  }

  if(is.na(time_horizon) & is.na(end_year)){
    stop("Require a time horizon or end date.")
  }

  if(is.na(start_year) & !is.na(end_year)){
    warning("Are you sure you don't want to specify a start date?")
  }

  if(!is.na(start_year) & !is.na(time_horizon) & is.na(end_year)){
    end_year <- start_year + time_horizon
  }

  if(!is.na(start_year) & !is.na(end_year) & is.na(time_horizon)){
    time_horizon <- end_year - start_year
  }


  if(is.na(time_horizon) | length(utility) > time_horizon){
    time_horizon <- length(utility)
  }

  utility <- fillin_missing_utilities(utility, time_horizon)


  adjusted_life_years <- list(start_year = start_year,
                              end_year = end_year,
                              age = age,
                              time_horizon = time_horizon,
                              utility = utility,
                              discount_rate = discount_rate,
                              death = NA)

  class(adjusted_life_years) <- c("adjusted_life_years", class(adjusted_life_years))

  return(adjusted_life_years)
}

