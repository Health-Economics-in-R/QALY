#'
#' #' QALY Plot as a Non-Increasing Step Function Over Time
#' #'
#' #' @param QALYs QALY class object
#' #' @param overlay Overlay lines on current plot?
#' #' @param XLIM
#' #' @param COL
#' #' @param age_annotate
#' #'
#' #' @return
#' #' @export
#' #'
#' #' @seealso \code{\link{total_QALYs.adjusted_life_years}}
#' #'
#' #' @examples
#' #' QALYs <- total_QALYs(AdjLifeYears)
#' #' plot_QALY(QALYs)
#' #'
#' #' ## list of objects
#' #' plot_QALY(QALY_diseasefree[[1]], overlay = F)
#' #' map(QALY_diseasefree, plot_QALY, overlay = T, COL = rgb(0, 0, 0, 0.1))
#' #' map(QALYloss_diseasefree, plot_QALY, overlay = T, COL = "red")
#' #'
#' #' # cumulative total QALY loss
#' #' sapply(QALY_diseasefree,
#' #'        FUN = function(x) attr(x, "yearly_QALYs")) %>%
#' #'        rowSums(na.rm = TRUE) %>%
#' #'        cumsum %>%
#' #'        plot(type = 'l')
#' #'
#' plot_QALY <- function(QALYs,
#'                       overlay = FALSE,
#'                       XLIM = c(0, 80),
#'                       COL = "light grey",
#'                       age_annotate = FALSE){
#'
#'   # append final year
#'   yearly_QALYs <- c(attr(QALYs,"yearly_QALYs"),
#'                     min(attr(QALYs,"yearly_QALYs")))
#'
#'   par(new = overlay)
#'
#'   plot(x = seq_along(yearly_QALYs) + rnorm(length(yearly_QALYs), 0, 0.2),
#'        y = yearly_QALYs + rnorm(1, 0, 0.01),
#'        type = "s",
#'        ylim = c(0, 1),
#'        ylab = "Health-related quality of life",
#'        xlab = "Time",
#'        xlim = XLIM,
#'        col = COL
#'   )
#'
#'   # lines(attr(QALYs,"adjusted_life_years")$QoL, lty = 2)
#'   # lines(attr(QALYs,"adjusted_life_years")$utility, lty = 3)
#'
#'   # annotate start and end ages
#'   if (age_annotate) {
#'
#'     text(1, yearly_QALYs[1], attr(QALYs,"adjusted_life_years")$age)
#'     text(x = length(yearly_QALYs),
#'          y = yearly_QALYs[length(yearly_QALYs)],
#'          labels = as.character(attr(QALYs,"adjusted_life_years")$age + length(yearly_QALYs) - 1))
#'   }
#' }


#' @title plot HRQoL_year
#'
#' @param HRQoL_year
#' @param overlay
#' @param XLIM
#' @param COL
#' @param age_annotate
#'
#' @rdname plot
#' @export plot
#' @export plot.HRQoL
#' @export
#'
plot.HRQoL <- function(HRQoL_year,
                       overlay = FALSE,
                       XLIM = NA,
                       COL = "light grey",
                       age_annotate = FALSE){

  # append final year
  yearly_QALYs <- c(HRQoL_year,
                    min(HRQoL_year))


  if (is.na(XLIM)) XLIM <- c(0, sum(!is.na(HRQoL_year)))

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

    text(1, HRQoL_year[1], attr(HRQoL_year, "adjusted_life_years")$age)
    text(x = sum(!is.na(HRQoL_year)),
         y = HRQoL_year[length(HRQoL_year[!is.na(HRQoL_year)])],
         labels = as.character(attr(HRQoL_year, "adjusted_life_years")$age + length(HRQoL_year) - 1))
  }
}

