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
#' @return mayfly_month_err, mayfly_month_cor, mayfly_max_err, mayfly_max_cor

flow_metrics_mayflies = function(m,o, month, day, year,wy, mayfly_month = 7) {
  
  # create a dataframe with the model estimates, observations, and date information
  flow = cbind.data.frame(m,o, month, day, year,wy)
  
  
  # calculate maximum values
  
  tmp = flow %>% group_by(wy) %>% summarize(maxo=max(o), maxm=max(m))
  
  # calculate monthly values for stream flow by taking the sum per month for model estimate and observed data
  tmp = flow %>% group_by(month, year) %>% summarize(model=sum(m), obs=sum(o))
  
  # now extract the sum data from the mayfly emergence month
  mayfly_month_all = subset(tmp, month %in% mayfly_month)
  
  # calculate the error in the model output for the whole month of mayfly relevance
  mayfly_month_err = mean(mayfly_month_all$model-mayfly_month_all$obs)
  
  # calculate the correlation value between the observed and model output for the whole month of mayfly relevance
  mayfly_month_cor=cor(mayfly_month_all$model, mayfly_month_all$obs)
  
  return(list(mayfly_month_err=mayfly_month_err, mayfly_month_cor=mayfly_month_cor, low_month_err=low_month_err,
              low_month_cor=low_month_cor))
}
