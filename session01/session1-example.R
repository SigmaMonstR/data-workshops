####################
# PIPELINE EXAMPLE #
####################

#Description
#------------------------------------------------------------------------
#This script illustrates a complete pipeline for a data science project. 
#The idea is to predict EIA Residential Electricity Revenue using weather
#data and autoregressive regression. 

#(1) SET UP ENVIRONMENT----------------

  #Set your working directory
    setwd(getwd())
    
  #Load
    library(forecast)
    library(readxl)
    library(ggplot2)
    library(zoo)
    
  #load custom function from Github
    source("https://raw.githubusercontent.com/SigmaMonstR/weather-series/master/code/stateWeather.R")
  
  #Specify what state we're going to work with 
    loc.abbrev <- "FL"
    loc.name <- "Florida"


#(2) GET DATA ----------------
# Download and read in an excel file from EIA

  #I/O requires creating a temp file, download to temporary location
    temp <- tempfile()
    download.file("https://www.eia.gov/electricity/data/eia861m/xls/sales_revenue.xlsx", 
                  temp,
                  mode ="wb")
  
  #Read Excel file
    sales <- read_excel(temp, sheet = 1,  skip = 2)
  
  #Change headers
    colnames(sales) <- c("year", "month", "state", "data_status", 
                       "res_rev_k", "res_sales_mwh", "res_customer_cnt", "res_price",
                       "comm_rev_k", "comm_sales_mwh", "comm_customer_cnt", "comm_price",
                       "ind_rev_k", "ind_sales_mwh", "ind_customer_cnt", "ind_price",
                       "trans_rev_k", "trans_sales_mwh", "trans_customer_cnt", "trans_price",
                       "other_rev_k", "other_sales_mwh", "other_customer_cnt", "other_price",
                       "total_rev_k", "total_sales_mwh", "total_customer_cnt", "total_price")
    
  #Pull out NY state
    eia <- sales[sales$state == loc.abbrev,]
    eia <- eia[!is.na(eia$year),]
    eia <- ts(eia$res_rev_k, frequency = 12, start = c(1990,1))
  
  #Scrape weather from NOAA using custom function 
    cdd <- stateWeather("cdd", 1990, 2018, loc.name)
    hdd <- stateWeather("hdd", 1990, 2018, loc.name)
    tmin <- stateWeather("tmin", 1990, 2018, loc.name)
    tmax <- stateWeather("tmax", 1990, 2018, loc.name)
    pcp <- stateWeather("pcp", 1990, 2018, loc.name)

  #Consolidate as 1st differenced time series
    series <- cbind(eia =diff((eia)), 
                 hdd =diff(hdd), 
                 cdd = diff(cdd), 
                 tmin = diff(tmin),
                 tmax = diff(tmax),
                 pcp = diff(pcp))


#(3) ESTIMATE MODEL  ----------------

  #Split sample for training and testing
    train <- subset(series, end = 250)
    test <- subset(series, start =251)

  #Estimate a simple seasonal ARIMA(0,1,1) on training sample
  #that includes one regressor: 'cdd' or cooling degree days
  fit <- auto.arima(y = train[,1], 
               xreg = train[,3],
               seasonal = TRUE)
  
  #Apply regression model to out-of-sample set
    out <- forecast(fit, xreg = test[,3])

#(4) REVIEW OUTPUTS ------------------
  
  #Output - differenced
    output <- ts.union(y = eia, 
                   dy = test[,1],
                   dyhat = out$mean)
    pd <- as.yearmon(time(output))
    
  #Clean up result outputs
    output <- as.data.frame(output)
    output <- cbind(period = pd, output)
    output$yhat <- output$pdy <- output$pdyhat <- NA
    
  #Fill in results
    for(i in 2:nrow(output)){
      
      #Calculate percent 1st difference (actual)
      output$pdy[i] <- output$dy[i]/output$y[i-1]
      
      #Calculate predicted levels (predicted)
      output$yhat[i] <- output$y[i-1] + output$dyhat[i]
      
      #Calculate predicted percent 1st difference
      output$pdyhat[i] <- output$dyhat[i]/output$y[i-1]
    }
    
  #Clean up
    output <- output[!is.na(output$dy),]
    
  #Plot
    ggplot(output) + 
      geom_line(aes(x = period, y = yhat), colour ="blue") + 
      geom_point(aes(x = period, y = y)) +
      ylab("Revenue Growth ($)") +  xlab("Periods (Out-of-Sample)") + 
      theme_bw()
      
  #Calculate accuracies
    #R2
    cor(output[,2:ncol(output)], use = "pairwise.complete.obs")^2
    
    #RMSE
    sqrt(mean((output$y - output$yhat)^2, na.rm = TRUE))
    
    #MAE
    mean(abs(output$y - output$yhat), na.rm = TRUE)
    
  #Output
    write.csv(output, "predictions.csv", row.names = FALSE)
    
  