#########################################################################################################
# Final Project Part 3B: A) Create poverty threshold graphs for The World, China, Bangladesh and Tanzania
#########################################################################################################

#install.packages("WDI")
#install.packages("tidyverse")
#install.packages("lemon")
#install.packages("tikzDevice")
#Load libraries 
library(WDI)
library(tidyverse)
library(lemon)
library(tikzDevice)
#Clear workspace
rm(list = ls())

#Create filter for countries we need
country_filter <- c("WLD", "BGD", "CHN", "TZA")
#Create filter for series we need 
indicator_filter <- c("1.90 USD a day (2011PPP)" = "SI.POV.DDAY" , 
                      "3.20 USD a day (2011PPP)" = "SI.POV.LMIC" , 
                      "5.50 USD a day (2011PPP)" = "SI.POV.UMIC")

#Load the data
wdi_data <- WDI(country=country_filter,         #filter by country
                indicator = indicator_filter,   #filter by indicators
                start = 1981, end = 2016,       #1981-2016 values
                extra = TRUE, cache = NULL)

#gather the poverty rates in a column and gather the name of the series in a column
td <- gather(wdi_data, series_name, poverty_rate, 4:6)

#Tidy the data
td<-td%>%
  select(3,4,11,12)%>%               #Select the columns we need
  filter(!is.na(poverty_rate))%>%    #Select the rows we need
  rename(country_code = iso3c)       #Rename the column for country codes 
str(td)

# Graph the data
Plot5 <- ggplot()+
  geom_line(data = td, size=1.2, aes(x=as.numeric(year), y=poverty_rate, col=country_code))+   #Add a line graph
  geom_point(data = td, size=2, aes(x=as.numeric(year), y=poverty_rate, col=country_code))+    #Add a scatter plot
  facet_rep_wrap(~series_name)+                               #Create a separate graph for each poverty threshhold
  theme_minimal()+                              
  theme(panel.background = element_blank(),
        legend.title=element_blank(),
        legend.key = element_blank(), 
        legend.position = 'top')+
  ylim(1,100)+
  labs(x="Year", y="Share of population living in poverty", caption = "Source: World Bank")
Plot5
#Save the graph
tikz("Plot5.tex", width = 5.5, height = 4)
Plot5
dev.off()

##################
# Further Analysis
##################

#China 1990 vs 2015 poverty rates
China <- td %>%
  filter(country_code=="CHN", year %in% c(1990, 2015))

#World 1981 vs 2015 poverty rates
World <- td %>%
  filter(country_code=="WLD", year %in% c(1981, 2015))

#Bangladesh 1990 vs 2015 poverty rates
Bangladesh <- td %>%
  filter(country_code=="BGD", year %in% c(1983, 2016))

#Tanzania 1981 vs 2015 poverty rates
Tanzania <- td %>%
  filter(country_code=="TZA", year %in% c(1991, 2011))


