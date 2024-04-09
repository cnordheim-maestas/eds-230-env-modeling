#' Projected almond yield
#' 
#' Calculating almond yield (tons/acre) given minimum monthly temperature in degrees Celsius and total monthly precipitation in millimeters
#' @param min_temp = minimum daily temperature of *February* in degrees Celsius
#' @param precip = total precipitation for *January* in millimeters
#'
#' @return A dataframe of projected minimium, maximum, and mean of almond yield (tons/acre) for given dataset
#'
#' @references Lobell, David B., Christopher B. Field, Kimberly Nicholas Cahill, and Celine Bonfils. 2006. “Impacts of Future Climate Change on California Perennial Crop Yields: Model Projections with Climate and Crop Uncertainties.” Agricultural and Forest Meteorology 141 (2-4): 208–18. https://doi.org/10.1016/j.agrformet.2006.10.006.
 
almond_yield <- function(min_temp, precip) {
  yield = (-0.015*min_temp) + (-0.0046*(min_temp^2)) + (-0.07*precip) + (0.0043*(precip^2)) + 0.28

  df = data.frame(max(yield),
         min(yield),
         mean(yield)) %>%
    clean_names()

  return(df)
}