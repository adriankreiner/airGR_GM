TransfoParam_Glacier <- function(ParamIn, Direction) {

  ## check arguments
  isVecParamIn <- is.vector(ParamIn)
  if (isVecParamIn) {
    ParamIn <- matrix(ParamIn, nrow = 1)
  } 
  
  ## transformation
  if (Direction == "TR") {
    ParamOut <- ParamIn
    ParamOut[, 1] <- exp(ParamIn[, 1]) / 200       ### Degree day melt coefficient Glacier
    ParamOut[, 2] <- 6/19.98* (ParamIn[, 2])   # Tm linear normalized  [-9.99, 9.99] to [-3, 3]
    ParamOut[, 3] <- 4.9/19.98 * (ParamIn[, 3] -(9.99 - 19.98/4.9*5))  # SWE_th linear normalized from [-9.99, 9.99]to [0.1, 5] 
  }
  if (Direction == "RT") {
    ParamOut <- ParamIn
    ParamOut[, 1] <- log(ParamIn[, 1] * 200)       ### Degree day melt coefficient Glacier
    ParamOut[, 2] <- 19.98/6 * (ParamIn[, 2]) 
    ParamOut[, 3] <- 19.98/4.9 * ((ParamIn[, 3])) + (9.99 - 19.98/4.9*5)  # SWE_th
  }
  
  ## check output
  if (isVecParamIn) {
    ParamOut <- as.vector(ParamOut)
  }
  return(ParamOut)
  
}
