
#' Calculate Life-Time QALYs
#'
#' @param adjusted_life_years Object of class \code{adjusted_life_years}
#'
#' @return QALYs object
#' @export
#'
#' @examples
#'
#' adj_life_yrs <- adjusted_life_years(
#'                     start_year = 2016,
#'                     end_year = 2020,
#'                     delay = 0,
#'                     age = NA,
#'                     time_horizon = NA,
#'                     utility = 0.9,
#'                     discount_rate = 0.035,
#'                     utility_method = "add")
#'
#' total_QALYs(adj_life_yrs)
#' ## 2.913622
#'
#' \dontrun{
#'  total_QALYs(1)
#'  ## "Error: Not an adjusted_life_years class input object."
#' }
total_QALYs <- function(adjusted_life_years) UseMethod("total_QALYs")


#' @rdname total_QALYs
#' @export
#'
total_QALYs.default <- function(adjusted_life_years){
  stop("Error: Not an adjusted_life_years class input object.")
}


#' @rdname total_QALYs
#' @export
#'
total_QALYs.adjusted_life_years <- function(adj_life_yrs){

  max_year <- 100

  yearly_QALYs <- vector(mode = 'numeric',
                         length = adj_life_yrs$time_horizon)

  HSUV_method <- HSUV(method = adj_life_yrs$utility_method)

  discountfactor <-
    make_discount(adj_life_yrs$discount_rate)

  for (i in seq_len(adj_life_yrs$delay)) {
    discountfactor()
  }

  adj_life_yrs$period <-
    rep(1, adj_life_yrs$time_horizon)

  period_QALY <- function(x) x$period * HSUV_method(x$utility, x$QoL)

  for (i in seq_len(adj_life_yrs$time_horizon)) {

    yearly_QALYs[i] <-
      purrr::map(adj_life_yrs, i) %>%
      period_QALY() * discountfactor()
  }

  # fill later years so same length for all individuals
  yearly_QALYs <-  c(yearly_QALYs, rep(NA, max_year - adj_life_yrs$time_horizon))

  attr(yearly_QALYs, "adjusted_life_years") <- adj_life_yrs
  class(yearly_QALYs) <- c("QALY", class(yearly_QALYs))

  return(yearly_QALYs)
}

