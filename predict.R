# predict method

predict.mars = function(model,newdata = NULL){
  
  # if no new data just return fitted values
  if(is.null(newdata)){
    return(model$fitted.values)
  }
  
  # combine response data and new data together in dataframe
  newdata = cbind(newdata,y = model$y)
  # run mars on new data to get basis functions
  basismdl = mars(y~.,data = newdata,control = model$mc["Mmax"])
  
  # gather the coefficients from input model
  coef = model$coefficients[!is.na(model$coefficients)]
  
  # counter for predicted values
  pred = 0
  
  # from 1 to number of basis functions do
  for(i in 1:length(coef)){
    
    # equation yhat = B0+B1x1+...+Bkxk
    # where B0,...Bk represents coefficients of the input model
    # and x1,...xk represents the basis functions
    pred = pred + coef[[i]]*basismdl$B[[i]]
  }
  
  # return the predicted values
  return(pred)
  
}