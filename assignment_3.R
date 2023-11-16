#-------------import library and set seed----------w
library(doParallel)
library(foreach)
library(nycflights13)
library(tidyverse)
View(flights)

set.seed(1234)

#-------------compute analytical SE---------

ana_reg <- lm(arr_delay ~ log(distance), data = flights)
summary(ana_reg)


#------------bootstrap without parallelization------
iter <- 500

system.time({
  coef_matrix <- matrix(data = NA, nrow = iter, ncol = 2);
  for (i in 1:iter){
    sample_all <- flights[sample(nrow(flights), size = 336000), replace = TRUE ]
    regression <- lm(arr_delay ~ log(distance), data = sample_all)
    coef_matrix[i, ] <- coef(regression)
  }
  
  SE_bootstrap <- sd(coef_matrix[, 2])
})

SE_bootstrap


#-------------bootstrap with parallelization---------
system.time({
  ncore <- detectCores()
  cl <- makeCluster(ncore -1, type = "FORK")
  registerDoParallel(cl)
  
  
  coef_matrix <- foreach(i = 1:iter, .combine = 'rbind') %dopar%{
    sample_all <- nycflights13 :: flights[sample(nrow(nycflights13 :: flights), 
                                                 size = 336000), replace = TRUE]
    regression <- lm(arr_delay ~ log(distance), data = sample_all)
    coef_matrix[i, ] <- coef(regression)
  }
  
  stopCluster(cl)
  
  SE_bootstrap <- sd(coef_matrix[, 2])
  
})


SE_bootstrap


