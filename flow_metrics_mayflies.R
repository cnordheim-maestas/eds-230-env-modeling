### Flow metrics used for mayfly emergence modeling

#' flow_metrics_mayflies
#'
#' Compute percent error and correlation values between observation and model 
#' for ecologically relevant month of mayfly emergence
#' 
#' @param  m  model estimates of streamflow
#' @param  o  observations of streamflow
#' @param  month month
#' @param  day day
#' @param  year year
#' @param mayfly_month which is the month mayflies emerge, here by default in July, the 7th month
#' @param wts (vector of 4 weight values for mayfly_month_err, mayfly_month_cor, mayfly_max_err, mayfly_max_cor)
#' @return mayfly_month_err, mayfly_month_cor, mayfly_max_err, mayfly_max_cor, combined_metrics

flow_metrics_mayflies = function(m,o, month, day, year,wy, mayfly_month = c(6:7), wts=c(0.25,0.25, 0.25,0.25)) {
  
  # required libraries
  require(tidyverse)
  
  # create a dataframe with the model estimates, observations, and date information
  flow = cbind.data.frame(m,o, month, day, year, wy)
  
  ### Goal 1: calculate how well the model can predict the water flow for the entire month of interest
  
  # calculate monthly values for stream flow by taking the sum per month for model estimate and observed data
  tmp = flow %>% 
    group_by(month, year) %>% 
    summarize(model=sum(m), 
              obs=sum(o))
  
  # now extract the sum data from the mayfly emergence month
  mayfly_month_all = subset(tmp, month %in% mayfly_month)
  
  # calculate the error in the model output for the whole month of mayfly relevance
  mayfly_month_err = mean(mayfly_month_all$model-mayfly_month_all$obs)
  
  # calculate errmax
  errmax = mean(mayfly_month_all$obs) * 0.5
  
  # normalize mayfly_month_err
  mayfly_month_err_trans = max(0, (1-abs(mayfly_month_err/errmax)))
  
  # calculate the correlation value between the observed and model output for the whole month of mayfly relevance
  mayfly_month_cor=cor(mayfly_month_all$model, mayfly_month_all$obs)
  
  ### Goal 2: calculate how well the model can predict the MAXIMUM flow for the month of interest
  
  # calculate monthly values for stream flow by taking the sum per month for model estimate and observed data
  tmp_2 = flow %>% 
    group_by(month, year) %>% 
    summarize(model=max(m), 
              obs=max(o))
  
  # now extract the sum data from the mayfly emergence month
  mayfly_max = subset(tmp_2, month %in% mayfly_month)
  
  # calculate the error in the model output for the whole month of mayfly relevance
  mayfly_max_err = mean(mayfly_max$model-mayfly_max$obs, na.rm = TRUE)
  
  # calculate errmax
  errmax = mean(mayfly_max$obs, na.rm = TRUE) * 0.5
  
  # normalize mayfly_max_err
  mayfly_max_err_trans = max(0, (1-abs(mayfly_max_err/errmax)))
  
  # calculate the correlation value between the observed and model output for the whole month of mayfly relevance
  mayfly_max_cor = cor(mayfly_max$model, mayfly_max$obs)
  
  # apply weight (normalize in case they don't sum to 1)
  wts = wts/sum(wts)
  
  # combine metrics with weights
  combined = (wts[1]*mayfly_month_err_trans + 
    wts[2]*mayfly_month_cor +
    wts[3]*mayfly_max_err_trans + 
    wts[4]*mayfly_max_cor)/4
  
  return(list(mayfly_month_err=mayfly_month_err_trans,
              mayfly_month_cor=mayfly_month_cor,
              mayfly_max_err=mayfly_max_err_trans,
              mayfly_max_cor=mayfly_max_cor,
              combined_metric=combined))
}
