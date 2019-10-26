#########################################################################
# Final Project Part 2: Create an algorithm that seasonally adjusts 
# a time series, based on the X11 algorithm. 
#########################################################################

#install.packages("tidyverse")
#install.packages("zoo")
#install.packages("forecast")
#Load libraries
library(tidyverse)
library(zoo)
library(forecast)
#Clear workspace
rm(list=ls())

#START OF FUNCTION
SeasonalAdjustment <- function(series, 
                               iter = 2,            # Number of iterations the function should perform
                               freq = 12,           # Frequency of time series, i.e monthly = 12, quarterly = 4
                               trend_ord = 13,      # Moving average trend order 
                               seas_ord = 3){       # Moving average seasonal order
  #If data is character string, convert to numeric
  if (is.character(series)) {     
    series <- as.numeric(series)      
  }
  #Duplicate the series as two copies are required for this function. Both copies contain the original time series.
  series_dup <- series
  #Start a loop over i from 1 to the number of iterations input by the user. 
  for (i in 1:iter){
    #Step 1: compute the trend level from the raw series by applying a moving average.
    trend <- ma(series, order = trend_ord, centre = TRUE)
    #Step 2: Subtract the trend from the raw series to obtain the seasonal and error components. 
    season_and_error <- series_dup - trend 
    #Step 3: Create an empty vector with the same length as the time series. 
    #        Then, loop over the season_and_error variable.
    empty_vec <- rep(NA, length(series))
    for (j in 1:freq){
      seasonal <- season_and_error[seq(j, length(series), by=freq)]
      empty_vec[seq(j, length(series), by=freq)] <- ma(seasonal, order = seas_ord)
    }
    #Step 4 Subtract seasonal component from the raw series
    seasonal_series <- series_dup - empty_vec
    #Store seasonal series as the original data and allow the next iteration of the loop 
    #to seasonally adjust the first estimate hence, giving the next estimate of the seasonally adjusted series
    series <- seasonal_series
  }
  return(series)
}
#END OF FUNCTION


#EXAMPLE OF USE
# 1. Test using the data provided below.
file <- 'https://raw.githubusercontent.com/1610893/ED-Final-Project/master/data_for_testing.csv'
df <- read.csv(file, stringsAsFactors = FALSE)

# 2.Test the function using default input.
SeasonalAdjustment(df$unemployment_rate)

# 3.Append the seasonally adjusted series to the original dataset.
df$unemployment_rate_sa <- SeasonalAdjustment(df$unemployment_rate)

# 4. Graph it to see the effect of the seasonal adjustment 
df <- mutate(df, date=as.yearmon(time, "%b-%y"))
ggplot(df) + 
  geom_line(aes(x = date, y = unemployment_rate, color = "raw series")) + 
  geom_line(aes(x = date, y = unemployment_rate_sa, color = "seasonally adjusted series")) + 
  scale_color_manual(name = "", 
                     values = c("raw series" = "black",
                                "seasonally adjusted series" = "red"))+
  scale_x_continuous()+
  theme_classic()+
  theme(legend.position = 'top')+
  labs(y='Unemployment rate in percent', x='Date', title = "Example of SA function", caption = 'Source: Economic data course material')
#END OF EXAMPLE

