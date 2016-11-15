
#' Calculate Inflation Adjusted Costs
#'
#' Use ONS GDP_Deflators_Qtrly_National_Accounts
#' Can't download directly into function because the csv on the website is too messy as-is.
#' This would be good to do though so that can always use latest version.
#'
#' @param from_date date of cost to convert
#' @param to_date date to convert cost to
#' @param from_cost cost at \code{from_date}
#'
#' @return
#' @export
#'
#' @examples
#' from_year <- 2012
#' to_year <- 2015
#' from_cost <- 96.140
#'
#' inflation_adjust_cost(from_date, to_date, from_cost)
#'
inflation_adjust_cost <- function(from_date, to_date, from_cost){

  ##TODO##
  # this file is too messy to use as raw data
  # temp <- tempfile()
  # ##TODO##
  # # how to always use the latest?
  # download.file("https://www.gov.uk/government/uploads/system/uploads/attachment_data/file/562750/GDP_Deflators_Qtrly_National_Accounts_September_2016_update_v2.csv", temp)
  # deflators <- read_csv(temp)
  # unlink(temp)

  deflators <- read_csv("C:/Users/Nathan/Dropbox/TB/LTBI/data/GDP_Deflators_09_2016.csv")

  from_row <- which(deflators$`Calendar year`==from_year)
  to_row <- which(deflators$`Calendar year`==to_year)

  deflators$prop_change_previous_year <- deflators$`per cent change on previous year`/100

  to_cost <- from_cost

  num_years <- to_row - from_row

  for (i in seq_len(num_years)){

    to_cost <- to_cost + (to_cost * deflators$prop_change_previous_year[from_row + i])
  }

  return(to_cost)
}
