# plot method for mars object

plot.mars = function(marsin){
  
  # set up 2 x 2 grid for plots
  op = par(mfrow=c(2,2))
  
  
  # save residuals in variable res
  res = marsin$residuals
  
  # save fitted values in variable fit
  fit = marsin$fitted.values
  
  # compute lowess line with residuals and fitted values
  test = lowess(res ~ fit)
  
  # plot residuals vs fitted
  plot(fit, res, main = "Residuals vs Fitted", ylab = "Residuals", xlab = "Fitted Values")
  
  # add straight line at residuals = 0
  abline(h = 0, lty = 1, col = "darkblue")
  
  #add lowess line
  lines(test, col = "firebrick")
  
  # plot response data and first explanatory data
  plot(y = marsin$y,x = marsin$mf[,1], ylab = "Response", xlab = "First Explanatory Variable",
       main = "Fitted Values on Y vs X Data")
  
  # add fitted points from the model 
  points(y = marsin$fitted.values,x = marsin$mf[,1],col = "red")
  
  # legend for fitted points
  legend("bottomright", legend = "Fitted Values", col = "red", pch = 1, bg = "transparent", cex = 0.8)
  
  
  # qqplot of residuals
  qqnorm(marsin$residuals, frame = FALSE)
  # line representing normality
  qqline(marsin$residuals, lty = 2, col = "blue")
  
  
  # finding the basis function with the lowest coefficient
  testcoef = marsin$coefficients[!is.na(marsin$coefficients)]
  low = testcoef[[1]]
  for(i in 2:length(marsin$B)){
    if(testcoef[[i]]<low){
      # keeping track of the lowest
      low = testcoef[[i]]
      # keeping track of the index
      counter = i
    }
  }
  
  # linear regression line with response on basis function with lowest coefficient
  plot(y = marsin$y,x = marsin$B[,counter], ylab = "Response", xlab = "Basis Function With Lowest Coefficients",
       main = "Y vs Basis Function with Linear Regression Line")
  
  # regression with first coefficients at intercept and lowest coefficient from mars as slope
  abline(a=marsin$coefficients[1],b=low, col = "green")
  
  # legend for regression line
  legend("bottomright", legend = "Linear Regression Line", col = "green", lty = "solid", lwd = 3, bg = "transparent", cex = 0.8)
  
  # close 2 x 2 grid just in case need to plot something else
  par(op)
}