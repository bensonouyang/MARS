summary.mars = function(mars){
  # function is like print except prints out coefficients
  cat("\nMARS Equation:\n",
      paste(deparse(mars$formula), sep = "\n", collapse = "\n"), "\n\n", sep = "")
  if(length(coef(mars))) {
    
    # save coefficients that aren't null
    coefdf = data.frame(Coefficients = mars$coefficients[!is.na(mars$coefficients)])
    # column names of explanatory variable data
    names = colnames(mars$mf)
    # from 1 to length of column names
    for(j in 1:length(names)){
      # if the name is a factor then do 
      if(is.factor(mars$mf[,names[j]])){
        
        # save levels of column name
        placeholder = levels(mars$mf[,names[j]])
        
        # replace the column name that was a factor with second level
        names[j] = placeholder[2]
        
        # from third level onwards do:
        for(k in 3:length(placeholder)){
          
          # adds every level after the second level to the vector
          names = append(names,placeholder[k])
        }
      }
    }
    # remove weird characters from level names(Wage case)
    names = gsub("^.[^A-Z|a-z]","",names)
    
    # for loop that goes to change the row name to match the hinge call
    for(i in 2:(length(coef(mars))-1)){
      row.names(coefdf)[i] = paste0("h(",mars$splits[[i]][2,"s"],
                                    "(",names[mars$splits[[i]][2,"v"]-1],"-",
                                    mars$splits[[i]][2,"t"],"))")
    }
    
    # print coefficients with new rownames 
    print(coefdf)
    
    # same as print function for mars object
    cat(paste0("\nSelected ",length(coef(mars))-1, " of ",mars$mc$Mmax," terms\n"))
    rss = sum(residuals(mars)^2)
    gcv = LOF(y~.,data = model.frame(mars))
    gcv.null = LOF(y~1,data = model.frame(mars))
    tss = sum((mars$y-mean(mars$y))^2)
    
    grsq = 1-gcv/gcv.null
    
    
    rsq = 1-rss/tss
    
    
    cat(paste0("RSS: ",round(rss),
               " \tGCV: ",round(gcv,3),
               " \tGRsq: ",round(grsq,6),
               " \tRsq: ", round(rsq,6)))
  } else cat("No coefficients\n")
  cat("\n")
  out = c(list(rsq = rsq,rss = rss,gcv=gcv,gcv.null = gcv.null,tss = tss,grsq = grsq))
  class(out) = c("mars")
  invisible(out)
}