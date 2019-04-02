
#' Discounted values over time
#'
#' @description Dscounted value, e.g. cost or health (QALY), for each time point, e.g. year.
#' E.g. a QALY in the future is worth less to us now because of 'interest'
#' or conversely we'd need more QALYs now to have a QALY further in the future.
#'
#' Formula used is
#'  \deqn{1/(1 + d)^{y}}
#'
#' @param discount_rate Discount factor, default at 3.5\%
#' @param t_limit Time period (positive integer) to discount over starting from 1
#'
#' @return Discounted value for each time point up to \code{t_limit}
#' @export
#' @seealso \code{\link{excess_lifetime}} is the sum of discounted terms.
#'
#' @references
#' Severens, Johan L and Milne, Richard J,
#' Value in Health, 4, Discounting Health Outcomes in Economic Evaluation : The Ongoing Debate,
#' volume 7, 2004
#' @bibliography QALY.bib ##TODO: import .bib file
#'
#' @examples
#'
#' D <- discount(t_limit = 10)
#' utility <- 0.97
#'
#' # Discounted QALYs upto 10 years
#' sum(utility * D)
#'
discount <- function(discount_rate = 0.035,
                     t_limit = 100) {

  if (!is.numeric(discount_rate) | !discount_rate >= 0 | !discount_rate <= 1) {
    stop("Discount rate must be number between 0 and 1.")
  }

  if (!is.numeric(t_limit) | !t_limit > 0) {
    stop("Time limit must be an integer great than 0.")
  }

  if (!t_limit %% 1 == 0) stop("Time limit must be whole number.")


  discount_years <- NULL

  discount_years <- 1

  for (year in seq_len(t_limit)) {

    discount_years[year] <- exp(-(year - 1) * log(1 + discount_rate))
  }

  return(discount_years)
}


#' @title Make an Encapsulated Discount Function
#'
#' @description This format doesn't need to keep track of time.
#'
#' @param discount_rate Discount factor, default at 3.5\%
#'
#' @return Function with global scoped index
#' @export
#'
#' @examples
#'
make_discount <- function(discount_rate = 0.035){

  i <- 0

  function(){
    i <<- i + 1
    return(min(discount(discount_rate, t_limit = i)))
  }
}

