
#' Make an Encapsulated Discount Function
#'
#' This format don't need to keep track of i.
#'
#' @return
#' @export
#'
#' @examples
#'
make_discount <- function(){
  i <- 0
  function(){
    i <<- i + 1
    return(min(treeSimR::discount(t = i)))
  }
}


#' Calculate QALYs using Case-Fatality Rates
#'
#' Sum to time of death or a prespecified time horizon.
#' CFRs are dependent on age.
#' Utility is fixed over time.
#'
#' @param AGES
#' @param cfr_age_lookup
#' @param time_horizon
#' @param utility
#'
#' @return
#' @export
#' @seealso \code{\link{calc_QALY}}
#'
#' @examples
#'
# status-quo
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
#'
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

  QALY <- NULL
  pop <- max(length(AGES), length(time_horizon))

  for (i in seq_len(pop)){

    # age at start
    agei <- AGES[i]
    ltime <- 1
    discountfactor <- make_discount()
    count <- 1

    while(ltime==1){

      if(!is.null(cfr_age_lookup)){
        ltime <- ifelse(cfr_age_lookup$cfr[cfr_age_lookup$age==agei] < runif(1), 1, 0.5)
      }
      if(!is.null(time_horizon)) ltime <- ifelse(count<time_horizon, 1, 0.5)

      QALY[i] <- QALY[i] + (ltime * utility * discountfactor())
      age <- age + 1
      count <- count + 1
    }
  }

  return(QALY)
}

