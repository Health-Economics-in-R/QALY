
#' Calculate annual inflation adjusted costs
#'
#' Up to a present time inflated upwards.
#'
#' Option to use the following datasets:
#'
#' \itemize{
#'   \item \href{https://www.gov.uk/government/uploads/system/uploads/attachment_data/file/562750/GDP_Deflators_Qtrly_National_Accounts_September_2016_update_v2.csv}{ONS GDP_Deflators_Qtrly_National_Accounts}
#' (This document contains the latest gross domestic product (GDP) deflators.
#' The GDP deflator can be viewed as a measure of general inflation in the domestic economy)
#'   \item \href{https://www.pssru.ac.uk/pub/uc/uc2017/sources-of-information.pdf}{PSSRU annual inflation hospital and community health services (HCHS)}
#'   \item \href{https://www.ons.gov.uk/economy/inflationandpriceindices/datasets/consumerpriceinflation}{Consumer Price Inflation (CPI)}
#'   \item Fixed 3.5\%.
#' }
#'
#' \deqn{(1 + i_{1})(1 + i_{2}) \cdots (1 + i_{n}) \times C}
#'
#' Clearly, for the same \code{i} each year this is simply
#' \deqn{C(1 + i)^n}
#'
#' @param from_year Date of cost to convert from
#' @param to_year Date to convert cost to
#' @param from_cost Cost at \code{from_year}
#' @param inflation_data NA default is fixed 3.5\% rate of inflation; otherwise source of data (string) \code{\link{GDP_deflators}}, \code{\link{HCHS_pay}}, \code{\link{{HCHS_price}} or \code{\link{CPI}}.
#'
#' @return Inflated (to) cost (scalar), with attributes used to generate the return value:
#' \itemize{
#'   \item \code{from_year}
#'   \item \code{to_year}
#'   \item \code{from_cost}
#'   \item \code{reference} Which inflation adjustment dataset used.
#'  }
#' @export
#'
#' @examples
#' from_year <- 2012
#' to_year <- 2015
#' from_cost <- 96.140
#'
# inflation_adjust_cost(from_year,
#                       to_year,
#                       from_cost)
#' #100
#'
#' inflation_adjust_cost(from_year = 2010,
#'                       to_year = 2016,
#'                       from_cost = 1)
#' #1.229255
#' 1*(1+0.035)^6
#'
#' inflation_adjust_cost(from_year = 2014,
#'                       to_year = 2016,
#'                       from_cost = 1,
#'                       inflation_data = "CPI")
#' 1*1.004*1.01
#'
inflation_adjust_cost <- function(from_year,
                                  to_year,
                                  from_cost,
                                  inflation_data = 0.035){

  ##TODO##
  # the webpage deflator file is too messy- with comments etc- to use as raw data
  # # how to always use the latest?
  # coulud use readxl but file layout may change
  # temp <- tempfile()
  # download.file("https://www.gov.uk/government/uploads/system/uploads/attachment_data/file/562750/GDP_Deflators_Qtrly_National_Accounts_September_2016_update_v2.csv", temp)
  # https://www.gov.uk/government/statistics/gdp-deflators-at-market-prices-and-money-gdp-march-2018-quarterly-national-accounts
  # deflators <- read_csv(temp)
  # unlink(temp)

  if (from_year %% 1 != 0) stop("From date must be an integer valued whole year")
  if (to_year %% 1 != 0) stop("To date must be an integer valued whole year")
  if (from_cost < 0) stop("Cost must be non-negative")

  data_sources <- dir(system.file("extdata", package = "QALY"))

  if (!(inflation_data %in% data_sources || is_prob(inflation_data))) {

    stop("inflation_data not allowed")
  }

  num_years <- to_year - from_year

  if (inflation_data %in% data_sources) {

    data_files <- system.file("extdata", inflation_data, package = "QALY")
    datapkg_data <- datapkg::datapkg_read(data_files)
    data <- datapkg_data$data[[inflation_data]]
    data$adjustment <- data$prop_change_from_prev_year
    from_row <- which(data$year == from_year)

  } else {

    data <- NULL
    data$adjustment <- rep(inflation_data, num_years)
    from_row <- 0
  }

  to_cost <- from_cost

  for (i in seq_len(num_years)) {

    to_cost <- to_cost * (1 + data$adjustment[from_row + i])
  }

  attr(to_cost, "from_year") <- from_year
  attr(to_cost, "to_year")   <- to_year
  attr(to_cost, "from_cost") <- from_cost
  attr(to_cost, "reference") <- inflation_data

  return(to_cost)
}
