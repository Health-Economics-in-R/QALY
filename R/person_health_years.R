
#' Person health years object constructor
#'
#' For use as input to the QALY (and DALY) functions.
#' Want to provide set-up info in different ways but which gives us all the necessary information to go calc QALYs later.
#' individual level data.
#' Don’t need to supply all of these otherwise there would be redundancy and possible conflicts. E.g. time horizon can be got from start and end year.
#' Need to calculate QoL which is discounting due to age from start to end year.
#'
#' @param start_year Calendar year to begin calculation
#' @param end_year Calendar year to end calculation
#' @param delay If the start_year is in the future then the
#'              first discounting is not 1 but smaller accordingly (positive integer)
#'              We could alternatively define year_now
#' @param age Age at start of period (could be an age group)
#' @param time_horizon Number of time periods from start to end date
#' @param utility Proportion health detriment. This may change over time. Could provide as a list of length end-start or as say \code{list(c(2,3), c(u1,u2))} like in WinBUGs data.
#' @param discount_rate Fixed proportion reduction over time or a (continuous) function e.g. negative exponential
#' @param utility_method Should the yearly QALYs be summed to a scalar? \code{add} or \code{prod}
#'
#' @return Object of class person_health_years
#' \itemize{
#'  \item start_year
#'  \item end_year
#'  \item delay
#'  \item age
#'  \item time_horizon
#'  \item utility
#'  \item QoL
#'  \item discount_rate
#'  \item death
#'  \item utility_method
#'  }
#' @export
#'
#' @seealso \code{\link{QALY}}, \code{\link{DALY}}
#'
#' @examples
#'
#' personHealthYears <- person_health_years(
#'                       start_year = 2016,
#'                       end_year = 2020,
#'                       age = 33,
#'                       time_horizon = NA,
#'                       utility = 0.9,
#'                       discount_rate = 0.035)
#'
#' total_QALYs(personHealthYears)
#' ## 2.913622
#'
#' \dontrun{
#'  total_QALY(1)
#'  ## "Error: Not an person_health_years class input object."
#' }
person_health_years <- function(start_year = 0,
                                end_year = NA,
                                delay = 0,
                                age = NA,
                                time_horizon = NA,
                                utility,
                                discount_rate = 0.035,
                                utility_method = "add"){

  if (start_year < 0) stop("Start year must be non-negative.")
  if (!is.na(end_year) & end_year < 0) stop("End year must be non-negative.")
  if (!is.na(time_horizon) & time_horizon < 0) stop("Time horizon must be non-negative.")

  if (!is.na(age) && age < 0) stop("Age must be non-negative.")

  # if(!is.na(start_year)) start_year <- floor(start_year)
  # if(!is.na(end_year)) end_year <- floor(end_year)

  if (any(utility > 1) | any(utility < 0)) {
    stop("utility must be between 0 and 1.")
  }

  if (any(discount_rate > 1) | any(discount_rate < 0)) {
    stop("Discount factors must be between 0 and 1.")
  }

  if (delay < 0) {
    stop("delay must be non-negative.")
  }

  if (is.na(time_horizon) & is.na(end_year)) {
    stop("Require a time horizon or end date.")
  }

  if (is.na(start_year) & !is.na(end_year)) {
    warning("Are you sure you don't want to specify a start date?")
  }

  if (!is.na(start_year) & !is.na(time_horizon) & is.na(end_year)) {
    end_year <- start_year + time_horizon
  }

  if (!is.na(start_year) & !is.na(end_year) & is.na(time_horizon)) {
    time_horizon <- end_year - start_year
  }

  if (is.na(time_horizon) | length(utility) > time_horizon) {
    time_horizon <- length(utility)
  }

  ##TODO: use intervals argument as well, see calc_QALY()
  utility <- fillin_utilities(utility, time_horizon)

  if (is.na(age)) {

    QoL <- rep(1, time_horizon)
  }else{

    QoL <- QoL_by_age(age, time_horizon)
  }

  person_health_years <- list(start_year = start_year,
                              end_year = end_year,
                              delay = delay,
                              age = age,
                              time_horizon = time_horizon,
                              utility = utility,
                              QoL = QoL,
                              discount_rate = discount_rate,
                              death = NA,
                              utility_method = utility_method)

  class(person_health_years) <- c("person_health_years", class(person_health_years))

  return(person_health_years)
}

