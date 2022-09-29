library(earth)
library(rpart)

mars = function(formula, data, control = NULL,...){
  # save the mars call
  cc = match.call()
  
  # dataframe with variables needed to use formula
  mf = model.frame(formula,data)
  # save the response from the model dataframe
  y = model.response(mf)
  # get attributes of model dataframe
  mt = attr(mf, "terms")
  # make a matrix for x terms for regression formula
  x = model.matrix(mt,mf)
  
  # default control is null, thus calls helper function
  if(is.null(control)) control = mars.control()
  
  # run forward pass on x and y
  fwd = fwd_stepwise(y,x,control$Mmax)
  
  # run backwards pass on forward to remove unwanted basis functions and splits
  bwd = bwd_stepwise(fwd)
  
  # run a regression for basis functions on y
  fit = lm(y~.,data = data.frame(y = y, bwd$B))
  
  # output saves the mars call, y values, basis functions, splits, and the lm fit
  out = c(list(formula = cc,y=y, B = bwd$B, splits = bwd$splits,data = data,mc = control,mf = mf[-1]), fit)
  
  # change class of the output to mars that inherits lm
  class(out) = c("mars", class(fit))
  
  return(out)
  
}


#---------------------------------------------------------------------
# forward and backward stepwise algorithms for MARS

fwd_stepwise <- function(y,x,Mmax){
  if(Mmax<2) { # Mmax check, sets to 2 if value less than 2
    warning("Input Mmax must be >= 2; setting to 2")
    Mmax <- 2
  } # end if
  N <- length(y) # sample size
  n <- ncol(x) # number of predictors
  B <- init_B(N) # dataframe initalized with a column with N rows of 1s
  splits <- list(data.frame(m=0,v=0,s=NA,t=NA)) # list for splits
  #---------------------------------------------------
  # Looping for forward stepwise:
  M = 2 # initalize M = 2
  while(M <= Mmax) { # While loop until M is greater than Mmax
    
    lof_best <- Inf # initalize lof_best to infinity
    
    for(m in 1:(M-1)) { # choose a basis function to split
      
      # getting the x columns that weren't split already
      x_diff <- setdiff(1:n, splits[[m]]$v)
      for(v in x_diff){ # select a variable to split on
        
        tt <- split_points(x[,v],B[,m]) # split points in x
        
        for(t in tt) { # over each point after splitting
          
          # dataframe for storing data to calculate lof
          Bnew = data.frame(B[,(1:(M-1))[-m]],
                            Btem1=B[,m]*h(x[,v],1,t),Btem2=B[,m]*h(x[,v],-1,t))
          # combine y and Bnew dataframe
          gdat = data.frame(y=y,Bnew)
          
          # calculate lof by getting the sum of squares residual after regression
          lof = LOF(y~.,gdat)
          
          # if lof less than lof_best
          if(lof < lof_best) {
            
            # old lof_best becomes lof
            lof_best = lof
            
            # best split on m
            m_star = m 
            # best split on v
            v_star = v
            # best split on t
            t_star = t
            
          } # end if
        } # end loop over splits
      } # end loop over variables
    } # end loop over basis functions to split
    
    # best splits for the left and right child
    left_split = rbind(splits[[m_star]],c(m_star,v_star,-1,t_star))
    right_split = rbind(splits[[m_star]],c(m_star,v_star,1,t_star))
    
    # add left and right splits to splits list
    splits = c(splits, list(left_split),list(right_split))
    
    # add right and left basis functions to B
    B = cbind(B, B[,m_star]*h(x[,v_star],1,t_star),B[,m_star]*h(x[,v_star],-1,t_star))
    
    # increment M by 2 for while loop
    M = M + 2
  } # end while loop over M
  
  
  # add names B0,...Bmmax to basis functions
  colnames(B) = paste0("B",(0:(ncol(B)-1)))
  
  return(list(y=y,B=B,splits=splits)) # return list containing basis functions and splits
}

bwd_stepwise <- function(fwd){
  # length of basis functions provides Mmax
  Mmax = ncol(fwd$B)
  
  # initalize j star to vector from 1 to Mmax
  J_star = 2:Mmax
  
  # K_star = J_star
  K_star = J_star
  
  # dataframe containing y values and basis functions
  df = data.frame(y = fwd$y, fwd$B)
  
  lof_star = LOF(y~.,data = df) # calculate the square of sums residuals
  #---------------------------------------------------
  # Looping for backward stepwise:
  
  # for loop for Mmax to 2
  for(M in Mmax:2){
    
    # let b equal to infinity
    b = Inf
    
    # L equal to K_star
    L = K_star
    
    # for loop from 2:M basis functions
    for(m in L){
      
      # difference between L and m to check basis functions that haven't been checked
      K = setdiff(L,m)
      
      # dataframe with y values and basis function at K
      check_df = data.frame(y = fwd$y,fwd$B[,K])
      
      # square of sums residuals for basis function at k
      lof = LOF(y~.,data = check_df) 
      
      # condition to check lof less than b which is infinity in the first iteration
      if(lof < b){
        # if condition met then set b to the lof
        b = lof
        
        # new K_star is K
        K_star = K
      } # end if less than b
      
      # condition on lof less lof_star where lof_star in the first iteration is
      # square of sums residuals of all the basis functions
      if(lof < lof_star){
        
        # if condition met, change lof_star to lof
        lof_star = lof
        
        # change J_star to K since K are the indices of the basis functions that 
        # met the condition
        J_star = K
        
      } # end if less than lof_star
    } # end for m
  } # end for M
  
  # include the first basis function of just 1's and the basis functions that were kept
  J_star = c(1,J_star)
  
  # returns list of y values, basis function at the best splits, splits
  return(list(y = fwd$y, B=fwd$B[,J_star],splits=fwd$splits[J_star]))
}

#--------------------------------------------------------------------
# constructor, validator, and helper for class mars.control

# helper function that calls constructor and validator
mars.control = function(Mmax = 2, d = 3, trace = FALSE){
  control = new_mars.control(Mmax, d, trace)
  validate_mars.control(control)
}

# constructor that initializes from mars.control with defaults Mmax = 2, d = 3, trace = FALSE
new_mars.control = function(Mmax,d,trace){
  structure(list(Mmax = Mmax, d = d, trace = trace))
}

# validator that checks that Mmax is greater than or equal to 2, sets to 2 if false
validate_mars.control = function(control){
  if(control$Mmax < 2){
    warning("Mmax must be >= 2; setting to 2")
    control$Mmax = 2
  }
  control
}

#--------------------------------------------------------------------
# functions for selection process

# hinge function for actual difference between covariate of interest and split point
# returns 0 when there are negative values
h = function(x,s,t){
  return(pmax(0,s*(x-t)))
}

# old step function to see if difference between covariate of interest and split point
# is greater than 0
H <- function(x) {
  return(as.numeric(x>=0))
}


# lack-of-fit criterion, best value for d is between 2 and 4
LOF <- function(form,data,d=3){
  
  # fit the regression model on the formula and data provided
  mdl <- lm(form,data)
  
  # residual sum of squares
  RSS <- sum(residuals(mdl)^2)
  
  # number of basis functions, -1 for the y column in the data
  M <- ncol(data) - 1 
  
  # sample size
  N <- nrow(data)
  
  # complexity cost function
  C_M <- sum(lm.influence(mdl)$hat)
  
  # returns lack-of-fit criterion minimized with respect to the parameters
  return(RSS*N/(N-(C_M+d*M))^2)
}

# function to initalize B to have a column with N rows with just 1's
init_B <- function(N) {
  B <- data.frame(B0 = matrix(1,nrow=N,ncol=1))
  return(B)
}

# split covariate of interest based on basis function
split_points <- function(xv,Bm) {
  out <- sort(unique(xv[Bm>0]))
  return(out[-length(out)])
}