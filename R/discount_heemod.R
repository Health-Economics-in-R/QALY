#' Discount a Quantity Over Time
#'
#' @param x numeric. A quantity to discount.
#' @param r discount rate.
#' @param first logical. Should discouting start at the
#'   first value ?
#' @param period Number of cycle per unit of discount rate.
#'
#' @return A numeric vector of the same length as `x`.
#' @export
#'
#' @examples
#'
#' discount(rep(10, 5), .02)
#' discount(rep(10, 5), .02, first = FALSE)
#'
#' @keywords internal
discount_heemod <- function(x, r, first = FALSE, period = 1) {

  if (length(r) > 1) r <- r[1]
  stopifnot(
    r >= 0,
    r <= 1,
    period > 0
  )

  dr <- trunc((seq_along(x) - (1 - isTRUE(first))) / period)
  x / (1 + r) ^ dr
}
