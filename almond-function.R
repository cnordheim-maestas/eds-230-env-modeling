
almonds<- function(Tn, P){ 
  # specifying inputs of the function, here they are the parameters of the model
  Y = 0.015*Tn - (0.0046*Tn)^2 - 0.07*P + (0.0043*P)^2 + 0.28 # E is the solution to multiplying all of the other values together
  return(Y) # print out the value of the almond yeild (in tons/acre)
}