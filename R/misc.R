
#
is_new_year <- function(cumul,
                        t) {

  cumul_new <- cumul + t
  floor(cumul_new) > floor(cumul)
}

#
get_remainder <- function(intervs) {

  last_value <-
    unlist(intervs) %>%
    tail(1)

  rem <- last_value %% 1

  if (rem == 0) {
    return(NULL)
  }

  return(rem)
}
