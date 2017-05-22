
##TODO: plot functions aren't finished

plot_lexis.adjusted_life_years <- function(adjusted_life_years){

  require(LexisPlotR)

  lg <- lexis.grid(year.start = 2015, year.end = 2030,
                   age.start = 0, age.end = 15)

  lexis.lifeline(lg = lg,
                 lineends = TRUE,
                 lwd = 2,
                 entry = as.Date(paste(adjusted_life_years$start_year, "01", "01", sep = "-")),
                 exit = as.Date(paste(adjusted_life_years$end_year, "01", "01", sep = "-")))
}


#' QALY Plot as a Non-Increasing Step Function Over Time
#'
#' @param QALYs QALY class object
#' @param add Overlay lines on current plot?
#'
#' @return
#' @export
#'
#' @seealso \code{\link{total_QALYs.adjusted_life_years}}
#'
#' @examples
#'
plot_QALY <- function(QALYs, add = FALSE){

  QoLs <- attr(QALYs, "QoL")

  # append final year
  QoLs <- c(QoLs, min(QoLs))

  par(new != add)

  plot(x = seq_along(QoLs) - 1, y = QoLs,
       type = "s",
       ylim = c(0, 1),
       ylab = "Health-related quality of life", xlab = "Time")
}
