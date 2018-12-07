
#' QoL_by_age
#'
#' Quality of Life adjustment for age
#'
#' @param age
#' @param time_horizon
#'
#' @return
#' @export
#'
#' @examples
QoL_by_age <- function(age,
                       time_horizon) {

  if (is.na(age)) return(rep(1, ceiling(time_horizon)))
  if (is.na(time_horizon)) stop("Error: NA time_horizons.")

  if (age < 0) stop(forbidden_negative_error(message = "ages"))
  if (time_horizon < 0) stop(forbidden_negative_error(message = "time_horizons"))

  lookup <- Kind1998_agegroups_QoL

  age_groups <-
    cut(x = age + 0:time_horizon,
        breaks = c(-1, lookup$max_age)) %>%
    data.frame("cut_intervals" = .)

  lookup$cut_intervals <-
    cut(x = lookup$max_age,
        breaks = c(-1, lookup$max_age))

  QoL <-
    left_join(x = age_groups,
              y = lookup,
              by = "cut_intervals") %>%
    dplyr::select(QoL) %>%
    unlist() %>%
    unname()

  QoL
}
