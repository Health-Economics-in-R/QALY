
total_QALYs <- function(adjusted_life_years) UseMethod("total_QALYs")


total_QALYs.default <- function(adjusted_life_years) print("Error: Not an adjusted_life_years class input object.")


#' Calculate QALYs
#'
#' @param adjusted_life_years Object of class adjusted_life_years
#'
#' @return QALYs
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
#' total_QALYs(AdjLifeYears)
#' ## 2.913622
#'
#' total_QALY(1)
#' ## "Error: Not an adjusted_life_years class input object."
#'
total_QALYs.adjusted_life_years <- function(adjusted_life_years){

  QALYs <- 0
  discountfactor <- make_discount(adjusted_life_years$discount_rate)

  # assume half final year
  period <- c(rep(1, adjusted_life_years$time_horizon - 1), 0.5)

  for(yeari in seq_along(adjusted_life_years$utility)){

    QALYs <- QALYs + (period[yeari] * adjusted_life_years$utility[yeari] * discountfactor())
  }

  return(QALYs)
}

