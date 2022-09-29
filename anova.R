anova.mars = function(out){
  # save basis functions in x and response in y
  X = out$B
  y = out$y
  
  # coefficients with B0 since it is NA, 
  # thus the intercept is the coef for first basis
  coefs = out$coefficients[-2]
  # number of rows
  max = dim(X)[1]
  
  fx = rep(0,max)
  
  # for 1 to max number of rows do
  for(i in 1:max){
    
    # first sum over all basis functions with single variable
    
    # second over basis functions involving two variables
    
    fx[i] = sum(coefs * X[i,])
    
    
  }
  
  cat("Analysis of Variance Table\n")
  
  response = as.character(out$formula)
  outresp = sub(" ~.*", "", response[2])
  
  cat("\nResponse: ",
      paste(outresp, sep = "\n", collapse = "\n"), "\n\n", sep = "")
  
  
  Df = c(dim(X)[2], out$df.residual)
  # total sum of squares = regression sum of squares + sum of squared errors
  sst = sum((out$y-mean(out$y))^2)
  ssr = sum((out$fitted.values-mean(out$y))^2)
  sse = sst - ssr
  Sum.sq = c(ssr, sse)
  Mean.sq = c(ssr/Df[1], sse/Df[2])
  F.value = c(round((Mean.sq[1])/(Mean.sq[2]),3), " ")
  
  datatab = data.frame('Df' = Df, 'Sum Sq' = Sum.sq, 'Mean Sq' = Mean.sq, 'F Value' = F.value)
  rownames(datatab) = c("basis functions", "residuals")
  print(datatab)
  
  
}