#' Crop Profit
#' 
#' Calculating the profit of crop yields given crop yield given revenue, cost of irrigration, and non-irrigation costs
#'
#' @param yield_data = a dataframe of year and projected yield anomaly (in tons / acre)
#' @param revenue = an annual revenue value (US dollar per ton)
#' @param cost_irrig = an annual cost value (US dollar per acre)
#' @param cost_nonirrig = an annual cost value (US dollar per acre)
#'
#' @return a dataframe with year, yield anomaly, and profit
#' @export
#'
#' @references Richard Waycott (2010) “The Economics of Growing Almonds”. Conference presentation at the 2010 California Almond Industry Conference.
#' 
#' 

crop_profit <- function(yield_data, revenue = 3800, 
                        cost_irrig = 530, cost_nonirrig = 3367) {
  require(tidyverse)
  
  yield_data %>%
    mutate(profit = yield * revenue - (cost_irrig + cost_nonirrig))
}