
#' plot_lexis.person_health_years
#'
#' ##TODO: plot functions aren't finished
#'
#' @param person_health_years
#'
#' @return
#' @export
#'
#' @examples
plot_lexis.person_health_years <- function(person_health_years){

  require(LexisPlotR)

  lg <- lexis.grid(year.start = 2015, year.end = 2030,
                   age.start = 0, age.end = 15)

  lexis.lifeline(lg = lg,
                 lineends = TRUE,
                 lwd = 2,
                 entry = as.Date(paste(person_health_years$start_year, "01", "01", sep = "-")),
                 exit = as.Date(paste(person_health_years$end_year, "01", "01", sep = "-")))
}
