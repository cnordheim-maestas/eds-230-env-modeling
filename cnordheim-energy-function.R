# Author: Caitlin Nordheim-Maestas

#' Title: Energy output from photovoltaic system
#' 
#' Goal: Calculating the amount of energy output in kWh from a photovoltaic system when given values for area, panel yield, average annual solar radiation, and percent yield
#' 
#' Model inputs:
#' @param A area of solar panel in m^2
#' @param r panel yield in units of manufacture efficiency, typically around 0.2, so the default value is 0.2 in this function
#' @param H annual average solar radiation in kWh
#' @param PR performance ratio, typically 0.75, so the default value is 0.2 in this function
#'
#' Model output:
#' @return the amount of energy output in kWh
#'
#' @examples energy_output(A=10, H=1) is an example for using the default values of r and PR. 
#' energy_output(A=10, H=1, r=0.3, PR=0.8) is an example for replacing the default values of r and PR.
#' 
#' @references Equation and default values from UCSB EDS-230 course materials from Week 1, written by Dr. Tague.


energy_output<- function(A, r=0.2, H, PR=0.75){ 
  # specifying inputs of the function, here they are the parameters of the model
  # note: r and PR are assigned default values by assigning them values in the function() argument, but we can always assign something else manually if we choose by saying r= or PR = when we use the function
  E = A * r * H * PR  # E is the solution to multiplying all of the other values together
  return(E) # print out the value of the energy output (in kWh)
}