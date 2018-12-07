
#' condition
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
condition <- function(subclass,
                      message,
                      call = sys.call(-1),
                      ...) {
  structure(
    class = c(subclass, "condition"),
    list(message = message, call = call),
    ...
  )
}

#
is.condition <- function(x) inherits(x, "condition")
