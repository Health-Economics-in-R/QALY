
#' Calculate Inflation Adjusted Costs
#'
#' up to the present time inflated upwards.
#' Option to use the ONS GDP_Deflators_Qtrly_National_Accounts or
#' a fixed 3.5\%.
#' Can't download directly into function because the csv on the website is too messy as-is.
#' This would be good to do though so that can always use latest version.
#' TODO: webscraping? regular expressions?
#'
#' @param from_year Date of cost to convert from
#' @param to_year Date to convert cost to
#' @param from_cost Cost at \code{from_year}
#' @param reference Source of data (string)
#' @param fixed Fixed 3.5\% rate of inflation?
#'
#' @return
#' @export
#'
#' @examples
#' from_year <- 2012
#' to_year <- 2015
#' from_cost <- 96.140
#'
#' inflation_adjust_cost(from_year, to_year, from_cost, fixed = FALSE)
#' #100
#'
#' inflation_adjust_cost(from_year=2010, to_year=2016, from_cost=1)
#' #1.229255
#' 1*(1+0.035)^6
#'
inflation_adjust_cost <- function(from_year,
                                  to_year,
                                  from_cost,
                                  reference = NA,
                                  fixed = TRUE){

  ##TODO##
  # the webpage defaltor file is too messy- with comments etc- to use as raw data
  # # how to always use the latest?
  # temp <- tempfile()
  # download.file("https://www.gov.uk/government/uploads/system/uploads/attachment_data/file/562750/GDP_Deflators_Qtrly_National_Accounts_September_2016_update_v2.csv", temp)
  # deflators <- read_csv(temp)
  # unlink(temp)

  if(from_year%%1 != 0) stop("From date must be an integer valued whole year")
  if(to_year%%1 != 0) stop("To date must be an integer valued whole year")
  if(from_cost < 0) stop("Cost must be greater than 0")

  DEFLATORS_FILES <- system.file("extdata/deflators", package = "QALY")
  datapkg_data <- datapkg::datapkg_read(DEFLATORS_FILES)
  deflators <- datapkg_data$data$deflators

  from_row <- which(deflators$`Calendar year`==from_year)
  to_row <- which(deflators$`Calendar year`==to_year)

  if (fixed){
    deflators$prop_change_previous_year <- 0.035
  }else{
    deflators$prop_change_previous_year <- deflators$`per cent change on previous year`/100
  }

  to_cost <- from_cost

  num_years <- to_row - from_row

  for (yeari in seq_len(num_years)){

    to_cost <- to_cost + (to_cost * deflators$prop_change_previous_year[from_row + yeari])
  }

  attr(to_cost, "from_year") <- from_year
  attr(to_cost, "to_year") <- to_year
  attr(to_cost, "from_cost") <- from_cost
  attr(to_cost, "reference") <- reference

  return(to_cost)
}
