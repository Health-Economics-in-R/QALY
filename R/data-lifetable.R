#' ONS National life tables: UK
#'
#' Period life expectancy by age and sex for the UK. Each national life table is
#' based on population estimates, births and deaths for a period of three
#' consecutive years. Tables are published annually.
#'
#' @format A data frame with 101 rows and 11 columns:
#' \describe{
#'   \item{age}{in years.}
#'   \item{mx_male}{Central rate of mortality, defined as the number of deaths at age \eqn{x} last birthday in the three year period to which the National Life Table relates divided by the average population at that age over the same period.}
#'   \item{qx_male}{Mortality rate between age \eqn{x} and \eqn{(x + 1)}, that is the probability that a person aged \eqn{x} exact will die before reaching age \eqn{(x + 1)}.}
#'   \item{lx_male}{Number of survivors to exact age \eqn{x} of 100,000 live births of the same sex who are assumed to be subject throughout their lives to the mortality rates experienced in the three year period to which the National Life Table relates.}
#'   \item{dx_male}{Number dying between exact age \eqn{x} and \eqn{(x + 1)} described similarly to \eqn{lx}, that is \eqn{dx = lx - lx + 1}.}
#'   \item{ex_male}{Average period expectation of life at exact age \eqn{x}, that is the average number of years that those aged \eqn{x} exact will live thereafter based on the mortality rates experienced in the three year period to which the National Life Table relates.}
#'   \item{mx_female}{Central rate of mortality, defined as the number of deaths at age \eqn{x} last birthday in the three year period to which the National Life Table relates divided by the average population at that age over the same period.}
#'   \item{qx_female}{Mortality rate between age \eqn{x} and \eqn{(x + 1)}, that is the probability that a person aged \eqn{x} exact will die before reaching age \eqn{(x + 1)}.}
#'   \item{lx_female}{Number of survivors to exact age \eqn{x} of 100,000 live births of the same sex who are assumed to be subject throughout their lives to the mortality rates experienced in the three year period to which the National Life Table relates.}
#'   \item{dx_female}{Number dying between exact age \eqn{x} and \eqn{(x + 1)} described similarly to \eqn{lx}, that is \eqn{dx = lx - lx + 1}.}
#'   \item{ex_female}{Average period expectation of life at exact age \eqn{x}, that is the average number of years that those aged \eqn{x} exact will live thereafter based on the mortality rates experienced in the three year period to which the National Life Table relates.}
#'   }
#' @source \url{https://www.ons.gov.uk/peoplepopulationandcommunity/birthsdeathsandmarriages/lifeexpectancies/datasets/nationallifetablesunitedkingdomreferencetables}
"lifetable"
