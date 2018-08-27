
#' Health State Utility Value
#'
#' Utilities can be combined in different ways to give an overall utility.
#'
#' \url{http://nicedsu.org.uk/wp-content/uploads/2016/03/TSD12-Utilities-in-modelling-FINAL.pdf}
#'
#' TODO: Adjusted decrement estimator
#' TODO: Combination model
#'
#' @param method additive, multiplicative or minimum
#' @param ... One or more utilities to combine
#'
#' @return
#' @export
#'
#' @examples
#'
#' HSUV(1,1)
#' HSUV(1,1,2)
#'
HSUV <- function(..., method = c("prod", "add", "min")) {

  if ("add" %in% method) method <-
      function(...) max(0, sum(...) - length(list(...)) + 1)

  function(...) {

    do.call(what = method, args = list(...))
  }
}

