
#' Calculate annual inflation adjusted costs
#'
#' Up to the present time inflated upwards.
#'
#' Option to use the
#' \href{https://www.gov.uk/government/uploads/system/uploads/attachment_data/file/562750/GDP_Deflators_Qtrly_National_Accounts_September_2016_update_v2.csv}{ONS GDP_Deflators_Qtrly_National_Accounts}
#' (This document contains the latest gross domestic product (GDP) deflators.
#' The GDP deflator can be viewed as a measure of general inflation in the domestic economy)
#' or \href{https://www.pssru.ac.uk/pub/uc/uc2017/sources-of-information.pdf}{PSSRU annual inflation hospital and community health services}
#' or a fixed 3.5\%.
#'
#' Can't download directly into function because the .csv on the website is too messy as-is.
#' This would be good to do though so that can always use latest version.
#' ##TODO: webscraping? regular expressions?
#'
#' \deqn{(1 + i_1)(1 + i_2) \cdots (1 + i_n) \times C}
#'
#' @param from_year Date of cost to convert from
#' @param to_year Date to convert cost to
#' @param from_cost Cost at \code{from_year}
#' @param reference Source of data (string)
#' @param inflation_data NA default is fixed 3.5\% rate of inflation, 'GDP_deflators', 'HCHS_pay' or 'HCHS_price'.
#'
#' @return
#' @export
#'
#' @examples
#' from_year <- 2012
#' to_year <- 2015
#' from_cost <- 96.140
#'
#' inflation_adjust_cost(from_year,
#'                       to_year,
#'                       from_cost,
#'                       fixed = FALSE)
#' #100
#'
#' inflation_adjust_cost(from_year = 2010,
#'                       to_year = 2016,
#'                       from_cost = 1)
#' #1.229255
#' 1*(1+0.035)^6
#'
inflation_adjust_cost <- function(from_year,
                                  to_year,
                                  from_cost,
                                  reference = NA,
                                  inflation_data = NA){

  ##TODO##
  # the webpage defaltor file is too messy- with comments etc- to use as raw data
  # # how to always use the latest?
  # temp <- tempfile()
  # download.file("https://www.gov.uk/government/uploads/system/uploads/attachment_data/file/562750/GDP_Deflators_Qtrly_National_Accounts_September_2016_update_v2.csv", temp)
  # https://www.gov.uk/government/statistics/gdp-deflators-at-market-prices-and-money-gdp-march-2018-quarterly-national-accounts
  # deflators <- read_csv(temp)
  # unlink(temp)

  if (from_year %% 1 != 0) stop("From date must be an integer valued whole year")
  if (to_year %% 1 != 0) stop("To date must be an integer valued whole year")
  if (from_cost < 0) stop("Cost must be greater than 0")

  if (is.na(inflation_data)) inflation_data <- 0.035

  num_years <- to_year - from_year

  if (is.numeric(inflation_data) &&
      length(inflation_data) == 1 &&
      inflation_data <= 1 &&
      inflation_data >= 0) {

    data <- NULL
    data$adjustment <- rep(inflation_data, num_years)
    from_row <- 0

  } else if (inflation_data == "GDP_deflators") {

    data_files <- system.file("extdata/GDP_deflators", package = "QALY")
    datapkg_data <- datapkg::datapkg_read(data_files)
    data <- datapkg_data$data$GDP_deflators
    data$adjustment <- data$`prop change on previous year`
    from_row <- which(data$year == from_year)

  } else if (inflation_data == "HCHS_price") {

    data_files <- system.file("extdata/annual_inflation_HCHS", package = "QALY")
    datapkg_data <- datapkg::datapkg_read(data_files)
    data <- datapkg_data$data$annual_inflation_HCHS
    data$adjustment <- data$prop_change_from_prev_year_price
    from_row <- which(data$year == from_year)

  } else if (inflation_data == "HCHS_pay") {

    data_files <- system.file("extdata/annual_inflation_HCHS", package = "QALY")
    datapkg_data <- datapkg::datapkg_read(data_files)
    data <- datapkg_data$data$annual_inflation_HCHS
    data$adjustment <- data$prop_change_from_prev_year_pay
    from_row <- which(data$year == from_year)

  } else {

    stop("inflation_data not allowed")
  }

  to_cost <- from_cost

  for (i in seq_len(num_years)) {

    to_cost <- to_cost * (1 + data$adjustment[from_row + i])
  }

  attr(to_cost, "from_year") <- from_year
  attr(to_cost, "to_year") <- to_year
  attr(to_cost, "from_cost") <- from_cost
  attr(to_cost, "reference") <- reference

  return(to_cost)
}
