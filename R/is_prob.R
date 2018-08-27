
is_prob <- function(x) {

  is.numeric(x) &&
    length(x) == 1 &&
    x <= 1 &&
    x >= 0
}
