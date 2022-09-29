print.mars = function(mars){
  # prints call of mars
  cat("\nCall:\n",
      paste(deparse(mars$formula), sep = "\n", collapse = "\n"), "\n\n", sep = "")
  
  # if there are coefficients then
  if(length(coef(mars))) {
    # prints selected number of basis functions of Mmax
    cat(paste0("\nSelected ",length(coef(mars))-1, " of ",mars$mc$Mmax," terms\n"))
    
    # residual sum of squares
    rss = sum(residuals(mars)^2)
    
    # generalized cross validation criterion
    gcv = LOF(y~.,data = model.frame(mars))
    
    # referenced from earth package documentation
    # generalized cross validation criterion of null model
    gcv.null = LOF(y~1,data = model.frame(mars))
    
    # total sum of squares
    tss = sum((mars$y-mean(mars$y))^2)
    
    # estimate of predictive power of the model over all responses
    grsq = 1-gcv/gcv.null
    
    # R-squared of model
    rsq = 1-rss/tss
    
    # prints all in a row
    cat(paste0("RSS: ",round(rss),
               " \tGCV: ",round(gcv,3),
               " \tGRsq: ",round(grsq,6),
               " \tRsq: ", round(rsq,6)))
  } else cat("No coefficients\n")
  cat("\n")
  invisible(mars)
}