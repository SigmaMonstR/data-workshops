#############################

#LOAD DATA
  setwd("/Users/jeff/Documents/Github/bea-data-workshops/session02")
  load("example.Rda")

#SET UP DATA
  short <- qss_main[qss_main$naics == "511",]
  
  #MODEL VALIDATION
  
  #Example of overconfidence
  #Cut out data
  y <- short$growth
  x <- as.matrix(short[,4:ncol(short)])
  
  #Plot for example
  plot(y, pch = 16, col = "darkgrey")
  lines(y, col = "darkgrey")
  
  #Overfit for GLMNET and plot
  library(glmnet)
  fit <- cv.glmnet(x, y, alpha = 1)
  
  coef.cv.glmnet(fit, s = "lambda.min" )
  
  yhat_1 <- predict(fit, x, s = "lambda.min")
  
  plot(y, pch = 16, col = "darkgrey")
  lines(y, col = "darkgrey", lty = "dashed")
  lines(yhat_1, col = "red")
  
  #Example 1: Train/Test
  test.start <- 15
  fit <- cv.glmnet(x[1:(test.start-1),], y[1:(test.start-1)], alpha = 1)
  
  coef.cv.glmnet(fit, s = "lambda.min" )
  
  yhat_2 <- predict(fit, x, s = "lambda.min")
  plot(short$growth, type = "l", lty = "dashed")
  lines(yhat_1, col = "red")
  lines(c(rep(NA, (test.start-1)),yhat_2[test.start:length(y)]), col = "blue")
  
  plot(y, pch = 16, col = "darkgrey")
  lines(y, col = "darkgrey", lty = "dashed")
  lines(yhat_1, col = "red")
  lines(c(rep(NA, (test.start-1)),yhat_2[test.start:length(y)]), col = "blue")
  
  #Example 2: One Step Ahead (Chaining)
  
  yhat_3 <- yhat_2
  
  for(i in test.start:length(y)){
    fit <- cv.glmnet(x[1:(test.start-1),], y[1:(test.start-1)], alpha = 1)
    
    temp <- predict(fit,x, s = "lambda.min")
    yhat_3[i] <- temp[i]
  }
  
  lines(yhat_1, col = "red")
  lines(c(rep(NA, (test.start-1)),yhat_2[test.start:length(y)]), col = "blue")
  
  plot(y, pch = 16, col = "darkgrey")
  lines(y, col = "darkgrey", lty = "dashed")
  # lines(yhat_1, col = "red")
  lines(c(rep(NA, (test.start-1)),yhat_2[test.start:length(y)]), col = "lightblue")
  lines(c(rep(NA, (test.start-1)),yhat_3[test.start:length(y)]), col = "green")
  
  sqrt(mean((yhat_2-y)^2, na.rm = T))
  sqrt(mean((yhat_3-y)^2, na.rm = T))
  
  
  
#ALGORITHMS

  #Create time series object
    qss <- ts(short[,3:ncol(short)], start = c(2004,3), frequency = 4)
    
  #ARIMA
  # Assumes regularity and momentum
  # Stationary processes
  # Linearity
    library(forecast)
    fit <- auto.arima(y = qss[,1], 
                      seasonal = TRUE)
    
    #Apply regression model to out-of-sample set
    out <- forecast(fit)
    plot(out)

  #GLMNET
    library(glmnet)
    
    #LASSO
    fit <- cv.glmnet(x = as.matrix(qss[,-1]), 
                     y = as.vector(qss[,1]), 
                     alpha = 1)
    coef.cv.glmnet(fit, s = "lambda.min" )
    plot(fit)
    
  #Decision Trees

    library(rpart)
    fit <- rpart(growth ~., data = qss, cp = 0)
    printcp(fit)
    
    #Choose best
    best <- which(fit$cptable[,4] == min(fit$cptable[,4]))
    best.cp <- fit$cptable[best,1]
    fit <- rpart(growth ~., data = qss, cp = best.cp)
    
    yhat_dt <- predict(fit, qss)
    yhat_dt <- ts(yhat_dt , start = c(2004,2), frequency = 2)
    
  #Random Forests
    library(randomForest)

    
    
