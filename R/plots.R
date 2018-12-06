
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
#' @param overlay Overlay lines on current plot?
#' @param XLIM
#' @param COL
#' @param age_annotate
#'
#' @return
#' @export
#'
#' @seealso \code{\link{total_QALYs.adjusted_life_years}}
#'
#' @examples
#' QALYs <- total_QALYs(AdjLifeYears)
#' plot_QALY(QALYs)
#'
#' ## list of objects
#' plot_QALY(QALY_diseasefree[[1]], overlay = F)
#' map(QALY_diseasefree, plot_QALY, overlay = T, COL = rgb(0, 0, 0, 0.1))
#' map(QALYloss_diseasefree, plot_QALY, overlay = T, COL = "red")
#'
#' # cumulative total QALY loss
#' sapply(QALY_diseasefree,
#'        FUN = function(x) attr(x, "yearly_QALYs")) %>%
#'        rowSums(na.rm = TRUE) %>%
#'        cumsum %>%
#'        plot(type = 'l')
#'
plot_QALY <- function(QALYs,
                      overlay = FALSE,
                      XLIM = c(0, 80),#
                      COL = "light grey",
                      age_annotate = FALSE){

  # append final year
  yearly_QALYs <- c(attr(QALYs,"yearly_QALYs"),
                    min(attr(QALYs,"yearly_QALYs")))

  par(new = overlay)

  plot(x = seq_along(yearly_QALYs) + rnorm(length(yearly_QALYs), 0, 0.2),
       y = yearly_QALYs + rnorm(1, 0, 0.01),
       type = "s",
       ylim = c(0, 1),
       ylab = "Health-related quality of life",
       xlab = "Time",
       xlim = XLIM,
       col = COL
  )

  # lines(attr(QALYs,"adjusted_life_years")$QoL, lty = 2)
  # lines(attr(QALYs,"adjusted_life_years")$utility, lty = 3)

  # annotate start and end ages
  if (age_annotate) {

    text(1, yearly_QALYs[1], attr(QALYs,"adjusted_life_years")$age)
    text(x = length(yearly_QALYs),
         y = yearly_QALYs[length(yearly_QALYs)],
         labels = as.character(attr(QALYs,"adjusted_life_years")$age + length(yearly_QALYs) - 1))
  }
}


##TODO: finish S3 method

#' @rdname plot
#' @export
#'
plot.QALYs <- function(QALYs,
                       overlay = FALSE,
                       XLIM = c(0, 80),
                       COL = "light grey",
                       age_annotate = FALSE){

  # append final year
  yearly_QALYs <- c(attr(QALYs,"yearly_QALYs"),
                    min(attr(QALYs,"yearly_QALYs")))

  par(new = overlay)

  plot(x = seq_along(yearly_QALYs) + rnorm(length(yearly_QALYs), 0, 0.2),
       y = yearly_QALYs + rnorm(1, 0, 0.01),
       type = "s",
       ylim = c(0, 1),
       ylab = "Health-related quality of life",
       xlab = "Time",
       xlim = XLIM,
       col = COL
  )

  # lines(attr(QALYs,"adjusted_life_years")$QoL, lty = 2)
  # lines(attr(QALYs,"adjusted_life_years")$utility, lty = 3)

  # annotate start and end ages
  if (age_annotate) {

    text(1, yearly_QALYs[1], attr(QALYs,"adjusted_life_years")$age)
    text(x = length(yearly_QALYs),
         y = yearly_QALYs[length(yearly_QALYs)],
         labels = as.character(attr(QALYs,"adjusted_life_years")$age + length(yearly_QALYs) - 1))
  }
}


