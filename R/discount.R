#' Discounted Values Over Time
#'
#' Dscounted value, e.g. cost or health (QALY), for each time point, e.g. year.
#' E.g. a QALY in the future is worth less to us now because of 'interest'
#' or conversely we'd need more QALYs now to have a QALY further in the future.
#'
#' @param d discount factor, default at 3.5\%
#' @param t Time period (positive integer) to discount over starting from 1
#'
#' @return Dscounted value for each time point
#' @export
#'
#' @examples
#'
#' D <- discount(t = 10)
#' QALY <- 0.97
#' QALY * D
#'
discount <- function(d = 0.35, t = 100) {

  stopifnot(is.numeric(d), d>=0, d<=1)
  stopifnot(is.numeric(t), t%%1==0, t>0)

  r <- NULL

  for (i in seq_len(t)){

    r[i] <- exp(i * (log(1) + log(1-d)))
  }

  return(r)
}
