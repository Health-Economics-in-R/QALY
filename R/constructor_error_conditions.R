
#' forbidden_negative_error
#'
#' @param subclass
#' @param message
#' @param call
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
#' tryCatch(
#'   stop(forbidden_negative_error(message = "variable")),
#'   forbidden_negative_error = function(e) stop(e))

forbidden_negative_error <- function(subclass,
                                     message = "",
                                     call = sys.call(-1),
                                     ...) {

  c <- condition(c("forbidden_negative_error", "error"),
                 message = paste("Forbidden negative error", message),
                 call = call,
                 ...)
  stop(c)
}
