
#' QoL_by_age
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

  return(QoL)
}
