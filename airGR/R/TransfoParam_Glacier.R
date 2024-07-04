TransfoParam_Glacier <- function(ParamIn, Direction) {

  ## check arguments
  isVecParamIn <- is.vector(ParamIn)
  if (isVecParamIn) {
    ParamIn <- matrix(ParamIn, nrow = 1)
  } 
  
  ## transformation
  if (Direction == "TR") {
    ParamOut <- ParamIn
    # ParamOut[, 1] <- exp(ParamIn[, 1]) / 200       ### Degree day melt coefficient Glacier
    ParamOut[, 1] <- (20/19.8) * (ParamIn[, 1]) + (10) # Fi degree day melt factor [0,20]
    ParamOut[, 2] <- 6/19.98* (ParamIn[, 2])   # Tm linear normalized  [-9.99, 9.99] to [-5, 5]
    # ParamOut[, 3] <- exp(ParamIn[, 3]) / 200
    ParamOut[, 3] <- (20/19.8) * (ParamIn[, 3]) + (10)  # SWE_th linear normalized from [-9.99, 9.99]to [0.1, 50]
  }
  if (Direction == "RT") {
    ParamOut <- ParamIn
    # ParamOut[, 1] <- log(ParamIn[, 1] * 200)       ### Degree day melt coefficient Glacier
    ParamOut[, 1] <- (ParamIn[, 1]-10)/(20/19.8) # Fi degree day melt factor [0,20]
    ParamOut[, 2] <- 19.98/6 * (ParamIn[, 2])  # Tm 
    # ParamOut[, 3] <- log(ParamIn[, 3] * 200)      
    ParamOut[, 3] <- (ParamIn[, 3]-10)/(20/19.8)  # SWE_th
  }
  
  ## check output
  if (isVecParamIn) {
    ParamOut <- as.vector(ParamOut)
  }
  return(ParamOut)
  
}




