
#' plot_lexis.adjusted_life_years
#'
#' ##TODO: plot functions aren't finished
#'
#' @param adjusted_life_years
#'
#' @return
#' @export
#'
#' @examples
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
