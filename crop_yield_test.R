#' Projected crop yield
#'
#'Calculating annual minimum, maximum, and mean crop yield (tons/acre) for a dataset containing minimum daily temperature in degrees Celsius, maximum daily temperature in degree Celsius, and total daily precipitation in millimeters.
#'
#' @param crop = a character string denoting the type of crop yield the user wants
#' @param data = a dataframe with year, month, day, daily minimum temperature (celsius), maximum temperature (celsius), and precipitation (millimeters)
#'
#' @return A dataframe of projected almond yield anomaly (tons/acre) per year for given dataset
#'
#' @references Lobell, David B., Christopher B. Field, Kimberly Nicholas Cahill, and Celine Bonfils. 2006. “Impacts of Future Climate Change on California Perennial Crop Yields: Model Projections with Climate and Crop Uncertainties.” Agricultural and Forest Meteorology 141 (2-4): 208–18. https://doi.org/10.1016/j.agrformet.2006.10.006.
#' 
crop_yield <- function(crop, data) {
  # required libraries
  require(tidyverse)
          
  ### if statement for almonds
  if (tolower(crop) == "almonds") {
    # wrangle data for almonds
    almond_data = data %>%
      filter(month == 2) %>%
      group_by(year) %>%
      summarize(min_temp_feb = min(tmin_c, na.rm = TRUE)) %>%
      left_join(
        data %>%
          filter(month == 1) %>%
          group_by(year) %>%
          summarize(precip_jan = sum(precip,
                                     na.rm = TRUE)),
        join_by(year)
      )
    
    # almond yield calculation
    yield = (-0.015 * almond_data$min_temp_feb) +
      (-0.0046 * (almond_data$min_temp_feb ^ 2)) +
      (-0.07 * almond_data$precip_jan) +
      (0.0043 * (almond_data$precip_jan ^ 2)) +
      (0.28)
    
  }
  else if (tolower(crop) == "wine grapes") {
    # wrangle data for wine grapes
    wg_data = data %>%
      filter(month == 4) %>%
      group_by(year) %>%
      summarize(min_temp_apr = min(tmin_c,
                                   na.rm = TRUE)) %>%
      left_join(
        data %>%
          filter(month == 6) %>%
          group_by(year) %>%
          summarize(precip_jun =
                      sum(precip,
                          na.rm = TRUE)),
        join_by(year)
      ) %>%
      left_join(
        data %>%
          mutate(year = (year - 1)) %>%
          filter(month == 9) %>%
          group_by(year) %>%
          summarize(precip_prev_sept =
                      sum(precip,
                          na.rm = TRUE)),
        join_by(year)
      )
    
    # wine grape calculation
    yield = (2.65 * wg_data$min_temp_apr) +
      (-0.17 * (wg_data$min_temp_apr ^ 2)) +
      (4.78 * wg_data$precip_jun) +
      (-4.93 * (wg_data$precip_jun ^ 2)) +
      (-2.24 * wg_data$precip_prev_sept) +
      (1.54 * wg_data$precip_prev_sept ^ 2) - 
      (10.50)
  }
  else if (tolower(crop) == "table grapes") {
    
    # wrangle data for table grapes
    tg_data = data %>%
      filter(month == 7) %>%
      group_by(year) %>%
      summarize(min_temp_jul = min(tmin_c,
                                   na.rm = TRUE)) %>%
      left_join(
        data %>%
          filter(month == 4) %>%
          group_by(year) %>%
          summarize(min_temp_apr = min(tmin_c,
                                       na.rm = TRUE)),
        join_by(year)
      ) %>%
      left_join(
        data %>%
          filter(month == 1) %>%
          group_by(year) %>%
          summarize(precip_jan = sum(precip,
                                     na.rm = TRUE)),
        join_by(year)
      ) %>%
      left_join(
        data %>%
          mutate(year = (year - 1)) %>%
          filter(month == 10) %>%
          group_by(year) %>%
          summarize(precip_prev_oct =
                      sum(precip,
                          na.rm = TRUE)),
        join_by(year)
      )
    
    # table grapes yield calculation
    yield = (6.93 * tg_data$min_temp_jul) +
      (-0.19 * tg_data$min_temp_jul ^ 2) +
      (2.61 * tg_data$min_temp_apr) +
      (-0.15 * tg_data$min_temp_apr) +
      (0.035 * tg_data$precip_jan) +
      (0.024 * tg_data$precip_jan ^ 2) +
      (1.71 * tg_data$precip_prev_oct) +
      (-0.673 * tg_data$precip_prev_oct ^ 2) +
      (-73.89)
    
  }
  else if (tolower(crop) == "oranges") {
    # wrangle data for oranges
    orange_data = data %>%
      filter(month == 5) %>%
      group_by(year) %>%
      summarize(precip_may = sum(precip,
                                 na.rm = TRUE)) %>%
      left_join(
        data %>%
          mutate(year = (year - 1)) %>%
          filter(month == 12) %>%
          group_by(year) %>%
          summarize(min_temp_prev_dec =
                      min(tmin_c,
                          na.rm = TRUE)),
        join_by(year)
      )
    
    # almond yield calculation
    yield = (1.08 * orange_data$min_temp_prev_dec) +
      (-0.20 * (orange_data$min_temp_prev_dec ^ 2)) +
      (4.99 * orange_data$precip_may) +
      (-1.97 * (orange_data$precip_may ^ 2)) +
      (-2.47)
  }
  else if (tolower(crop) == "walnuts") {
    # wrangle walnut data
    walnut_data = data %>%
      filter(month == 2) %>%
      group_by(year) %>%
      summarize(precip_feb = sum(precip, na.rm = TRUE)) %>%
      left_join(
        data %>%
          mutate(year = (year - 1)) %>%
          filter(month == 11) %>%
          group_by(year) %>%
          summarize(max_temp_prev_nov = max(tmax_c,
                                            na.rm = TRUE)),
        join_by(year)
      )
    
    # walnut yield calculation
    yield = (0.68 * walnut_data$max_temp_prev_nov) +
      (-0.020 * walnut_data$max_temp_prev_nov ^ 2) +
      (0.038 * walnut_data$precip_feb) +
      (-0.0051 * walnut_data$precip_feb ^ 2) +
      (-5.83)
    
  }
  else if (tolower(crop) == "avocados") {
    # wrangle avocado data
    avocado_data = data %>%
      filter(month == 5) %>%
      group_by(year) %>%
      summarize(min_temp_may = min(tmin_c, na.rm = TRUE)) %>%
      left_join(
        data %>%
          mutate(year = (year - 1)) %>%
          filter(month == 8) %>%
          group_by(year) %>%
          summarize(max_temp_prev_aug = max(tmax_c,
                                            na.rm = TRUE)),
        join_by(year)
      ) %>%
      left_join(
        data %>%
          mutate(year = (year - 1)) %>%
          filter(month == 10) %>%
          group_by(year) %>%
          summarize(precip_prev_oct = sum(precip,
                                          na.rm = TRUE)),
        join_by(year)
      )
    
    # avocado yield calculation
    yield = (17.71 * avocado_data$max_temp_prev_aug) +
      (-0.29 * (avocado_data$max_temp_prev_aug ^ 2)) +
      (3.25 * avocado_data$min_temp_may) +
      (-0.14 * (avocado_data$min_temp_may ^ 2)) +
      (1 * avocado_data$precip_prev_oct) +
      (-0.31 * (avocado_data$precip_prev_oct ^ 2)) +
      (288.09)
    
  }
  
  #### error message for wrong input
  else {
    print("Check input for correct crop and data structure")
  }
  
  ### return year and yield 
  df = data.frame(almond_data$year, yield) %>% 
    rename(year = 'almond_data.year')
  
  return(df) 
}
