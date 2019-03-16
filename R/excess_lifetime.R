#' Excess lifetime
#'
#' The difference between two future time points
#' (the first of these can be trivially the present).
#' Allows discounting of future times so that they are valued less than sooner times.
#'
#' These are usually two years of death such that this (discounted) time
#' is years of life lost due to illness.
#' This is a component of the DALY calculation
#' by including as a product with the probability of early death (p) and population size (N),
#' i.e. DALY = YLD + YDD, such that
#' YLD = N p t
#'
#' The later time to death is all-cause expected lifetime at a given age
#' which can be obtained from life-tables e.g. from the ONS.
#'
#' The expanded sum is
#'
#' {\deqn{\frac{1}{d^{m+1}} + \cdots + \frac{1}{d^{n}}, n > m}}
#'
#' Using a finite series identity this is
#'
#' {\deqn{\frac{1 - (1/d)^{n+1}}{1 - 1/d} - \frac{1 - (1/d)^{m+1}}{1 - 1/d}}}
#'
#' {\deqn{= \frac{(1/d)^{m+1} - (1/d)^{n+1}}{1 - 1/d}}}
#'
#' @param discount default: 0.035
#' @param start time
#' @param end time
#'
#' @return
#' @export
#'
#' @examples
#'
excess_lifetime <- function(start,
                            end,
                            discount = 0.035){

}
