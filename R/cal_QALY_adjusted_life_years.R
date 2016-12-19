#
# calc_QALYs <- function(adjusted_life_years) UseMethod("calc_QALYs")
#
#
# calc_QALYs.default <- function(adjusted_life_years) print("Error: Not an adjusted_life_years class input object.")
#
#
# #' Calculate QALYs
# #'
# #' @param adjusted_life_years
# #'
# #' @return
# #' @export
# #'
# #' @examples
# #'
# calc_QALYs.adjusted_life_years <- function(adjusted_life_years){
#
#   total_QALYs <- 0
#   discountfactor <- make_discount(adjusted_life_years$discount_rate)
#
#   # assume half final year
#   period <- c(rep(1, adjusted_life_years$time_horizon - 1), 0.5)
#
#   for(yeari in seq_along(adjusted_life_years$utility)){
#
#     total_QALYs <- total_QALYs + (period[yeari] * adjusted_life_years$utility[yeari] * discountfactor())
#   }
#
#   return(total_QALYs)
# }
#
