
#' Calculate QALYs using Case-Fatality Rates
#'
#' Sum to time of death or a prespecified time horizon.
#' CFRs are dependent on age.
#' Utility is fixed over time.
#'
#' @param AGES Vector of ages at start
#' @param cfr_age_lookup Data frame of case-fatality ratios for ages
#' @param time_horizon Vector of end dates for each individual
#' @param utility Proportion health detriment
#'
#' @return
#' @export
#' @seealso \code{\link{calc_QALY}}
#'
#' @examples
#'
#'
#' # 12 month case fatality rate
#' # Crofts et al (2008)
#' cfr_age_breaks <- c(15, 45, 65, 200)
#' cfr_age_levels <- levels(cut(0, cfr_age_breaks, right = FALSE))
#'
#' cfr_age_lookup <- data.frame(age = cfr_age_levels,
#'                              cfr = c(0.0018, 0.0476, 0.1755),
#'                              a = c(1, 125, 413), #beta distn
#'                              b = c(564, 2500, 1940))
#'
#' rownames(cfr_age_lookup) <- cfr_age_lookup$age
#'
#' # status-quo
#' QALY.statusquo <- calc_QALY_CFR(AGES = IMPUTED_sample$cfr_age_groups[IMPUTED_sample$uk_tb==1],
#'                                 cfr_age_lookup)
#'
#' # screened imputed sample
#' QALY.screened <- calc_QALY_CFR(AGES = IMPUTED_sample$cfr_age_groups[IMPUTED_sample$uk_tb1==1],
#'                                cfr_age_lookup)
#'
#' # who changed LTBI status
#' uk_TB.completedTx <- (IMPUTED_sample$uk_tb1==0 & IMPUTED_sample$uk_tb==1)
#'
#' # fixed over time
#' QALY_uk_TB.completedTx <- years(death.isdtt[uk_TB.completedTx] - uk_tb.isdtt[uk_TB.completedTx])
#'
#' QALY.screened <- c(QALY.screened, QALY_uk_TB.completedTx)
#'
#' # (non-fixed) discounted all-cause (non-active TB)
#' notification_to_allcause_death <- (IMPUTED_sample$date_death1_issdt - IMPUTED_sample$fup_issdt)/365
#'
#' calc_QALY_CFR(time_horizon = notification_to_allcause_death[uk_TB.completedTx],
#'               utility = 1.0)
#'
calc_QALY_CFR <- function(AGES = NA,
                          cfr_age_lookup = NULL,
                          time_horizon = NA,
                          utility = 0.9){

  cfr_modelframe <- model.frame(cfr ~ age, data = cfr_age_lookup)


  QALY <- NULL
  pop <- max(length(AGES), length(time_horizon))

  for (popi in seq_len(pop)){

    # age at start
    agei <- AGES[popi]
    ltime <- 1
    discountfactor <- make_discount()
    count <- 1

    while(ltime==1){

      if(!is.null(cfr_age_lookup)){
        cfr_subset <- select(cfr_age_lookup, subset = age==agei, select = cfr)
        ltime <- ifelse(cfr_subset < runif(1), 1, 0.5)
      }
      if(!is.null(time_horizon)) ltime <- ifelse(count<time_horizon, 1, 0.5)

      QALY[popi] <- QALY[popi] + (ltime * utility * discountfactor())
      agei <- agei + 1
      count <- count + 1
    }
  }

  return(QALY)
}

