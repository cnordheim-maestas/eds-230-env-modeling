#' Crop Profit
#' 
#' Calculating the profit of crop yields given crop yield given revenue, cost of irrigration, and non-irrigation costs
#'
#' @param yield_anomaly = a dataframe yield anomaly per year (in tons / acre)
#' @param price = an annual price value (US dollar per ton)
#' @param cost_irrig = an annual cost value (US dollar per acre)
#' @param cost_nonirrig = an annual cost value (US dollar per acre)
#'
#' @return a dataframe with projected profit difference (in units of US dollar / acre) due to yield anomaly and year 
#' @export
#'
#' @references Richard Waycott (2010) “The Economics of Growing Almonds”. Conference presentation at the 2010 California Almond Industry Conference.
#' 
#' 

crop_profit <- function(yield_anomaly, 
                        price = 3800, 
                        cost_irrig = 530, 
                        cost_nonirrig = 3367) {
  require(tidyverse)
  
    profit = yield_anomaly %>% 
    mutate(profit = yield * price -  (cost_irrig + cost_nonirrig))
    
    return(profit)
}

