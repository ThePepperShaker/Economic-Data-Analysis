###################################################################################################
# Final Project Part 3A) Task B Create graphs of raw and seasonally adjusted unemployment rates for  
# females and males in the four countries from A) over the period January 2000 - December 2018
###################################################################################################

#install.packages("OECD")
#install.packages("tidyverse")
#install.packages("forecast")
#install.packages("zoo")
#install.packages('lemon')
#install.packages('tikzDevice')
#Load libraries 
library(OECD)
library(tidyverse)
library(forecast)
library(zoo)
library(lemon)
library(tikzDevice)

#Clear workspace
rm(list=ls())

#Load function for seasonal adjustment
#START OF FUNCTION
SeasonalAdjustment <- function(series, 
                               iter = 2,            # Number of iterations the function should perform
                               freq = 12,           # Frequency of time series, i.e monthly = 12, quarterly = 4
                               trend_ord = 13,      # Moving average trend order 
                               seas_ord = 3){       # Moving average seasonal order
  #If data is character string, convert to numeric
  if (is.character(series)) {     
    data <- as.numeric(series)      
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


#Obtaining the data
#Create a filter of the data we want
filter_list <- list(c("DNK", "NLD", "NOR", "SVK"),   #We want the countries with these country IDs 
                    c("LRHUTTFE", "LRHUTTMA"),       #Only the unemployment rates for men and women
                    c("ST"),                         #We want the raw series 
                    c("M"))                          #We want the monthly series 
#Obtain the data         
une_data_all <- get_dataset(dataset = "STLABOUR",    
                            filter=filter_list,      #Filter "STLABOUR" by the list we created previously
                            start_time = 2000,       #Series should start in 2000
                            end_time = 2018)         #Series should end in 2018

#Convert the time variable to "yearmon" and store in new variable "date"
une_data_all <- mutate(une_data_all, date = as.yearmon(obsTime))

#Create seasonal adjusted series using function for seasonal adjustment 
une_data_all <- une_data_all %>%
  group_by(LOCATION, SUBJECT) %>%                   #Group by country and gender 
  mutate(saValue = SeasonalAdjustment(obsValue))    #Apply the function
#Divide the data into male and female 
une_data_female <- une_data_all%>%
  filter(SUBJECT=="LRHUTTFE")                       #Filter by female
une_data_male <- une_data_all%>%
  filter(SUBJECT=="LRHUTTMA")                       #Filter by male

#Create recession data 
#Source: https://fredhelp.stlouisfed.org/fred/data/understanding-the-data/recession-bars/
Peak <- c("2001-03", "2007-12")                   
Trough <- c("2001-11", "2009-06")                 
recessions <- as.data.frame(cbind(Peak, Trough))
recessions$Peak <- as.yearmon(recessions$Peak)
recessions$Trough <- as.yearmon(recessions$Trough)

#Create line graphs, one for each country, with both the male and female unemployment rates both for the
#raw and the seasonally adjusted series.
Plot2 <- ggplot()+
  geom_line(data = une_data_male, mapping = aes(x=date, y=obsValue, color = "unadjusted, men"))+
  geom_line(data = une_data_male, mapping = aes(x=date, y=saValue, color = "seas. adjusted, men"))+
  geom_line(data = une_data_female, mapping = aes(x=date, y=obsValue, color = "unadjusted, women"))+
  geom_line(data = une_data_female, mapping = aes(x=date, y=saValue, color = "seas. adjusted, women"))+
  geom_rect(data = recessions, aes(xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf,  fill='recession'), 
            alpha=0.2)+
  facet_rep_wrap(~LOCATION, nrow = 2, repeat.tick.labels = 'all', scales = 'free_y') +
  labs(y="Unemployment rate in percent", x="Year", caption = "Sources: OECD Labour Force Statistics, FRED")+
  scale_color_manual(name = "", 
                     values = c("unadjusted, men" = "lightblue",
                                "seas. adjusted, men" = "blue", 
                                "unadjusted, women" = "pink",
                                "seas. adjusted, women" = "red"))+
  scale_fill_manual(name="", 
                    values = c("recession" = "grey"))+
  scale_x_continuous(breaks=seq(2000,2018,length.out = 10))+
  theme_classic() + 
  theme(legend.position = 'top',
        legend.justification = 'left',
        axis.text.x = element_text(angle = 90))+
  guides(color=guide_legend(nrow=2,byrow=TRUE))
Plot2

#Save the graph
tikz('plot2.tex',width = 5.5, height = 4)
Plot2
dev.off()

##################################################################
# Further analysis - Looking at aggregates and growth rates.  
# 2003-2015 was used due to data loss from the seasonal adjustment
##################################################################

#Calculate average unemployment rate by country and gender
average_gender <- une_data_all%>%
  group_by(LOCATION, SUBJECT)%>%
  separate(obsTime, into = c("Year", "Month"))%>%
  filter(Year %in% c(2003:2015))%>%
  summarise(average = mean(saValue))

#Calculate average unemployment by country and gender before the recession
average_pre_recession <- une_data_all%>%
  separate(obsTime, into = c("Year", "Month"))%>%
  filter(Year %in% c(2003:2007))%>%
  group_by(LOCATION, SUBJECT)%>%
  summarise(average = mean(saValue))

#Calculate average unemployment by country and gender after the recession
average_post_recession <- une_data_all%>%
  separate(obsTime, into = c("Year", "Month"))%>%
  filter(Year %in% c(2008:2015))%>%
  group_by(LOCATION, SUBJECT)%>%
  summarise(average = mean(saValue))

#Calculate yearly growth rates by country and gender
une_data_yearly <- une_data_all%>%
  separate(obsTime, into = c("Year", "Month"))%>%
  group_by(LOCATION, SUBJECT, Year)%>%
  summarise(average = mean(saValue))

#Filter out the data for only 2008 and 2009
une_data_recession <- une_data_yearly%>%
  filter(Year %in% c(2008,2009))

#Calculate growth of unemployment from 2008 to 2009
une_data_recession <- une_data_recession%>%
  group_by(LOCATION, SUBJECT)%>%
  mutate(growth_une = 100*(average-lag(average))/lag(average))
