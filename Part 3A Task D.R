##################################################################################################
# Final Project Part 3A) Task D Create graphs or tables comparing register based and survey based
#  unemployment rates over the period January 2000 to December 2018 for the four countries (from A)
##################################################################################################

#install.packages("tidyverse")
#install.packages("zoo")
#install.packages("forecast")
#install.packages("tikzDevice")
#install.packages("lemon")
#install.packages("reshape2")
#install.packages("kableExtra")
#Load libraries
library(tidyverse)
library(forecast)
library(zoo)
library(lemon)
library(tikzDevice)
library(reshape2)
library(kableExtra)
#Clear workspace
rm(list=ls())

# Obtain the data
# Load seasonally adjusted survey based unemployment rate data
# url for dataset: "data7_ilo_unemployment_source_oecd.csv" - Data provided in lectures
url_ilo_data<-"https://www.dropbox.com/s/fcfx7y7fbz64j5p/data7_ilo_unemployment_source_oecd.csv?dl=1"
une_ilo_data<-read_csv(url_ilo_data)%>%select(Country,Time,Value)%>%
  mutate(Type="Survey based measure")

# Load seasonally adjusted register based unemployment rate data
# url for dataset: "data8_register_unemployment_source_oecd.csv" - Data provided in lectures
url_reg_data<-"https://www.dropbox.com/s/lsusiyv2l0qnba8/data8_register_unemployment_source_oecd.csv?dl=1"
une_reg_data<-read_csv(url_reg_data)%>%
  filter(Subject=="Registered unemployment, Rate, Total, sa",Unit=="Percentage")%>%
  select(Country,Time,Value)%>%mutate(Type="Register based measure")

#Create a Quarter variable
une_ilo_data <- mutate(une_ilo_data, Quarter=as.yearqtr(Time, format = "Q%q-%Y"))
une_reg_data <- mutate(une_reg_data, Quarter=as.yearqtr(Time, format = "Q%q-%Y"))

#Filter only the countries we need 
une_ilo_all <- filter(une_ilo_data, Country %in% c("Denmark", "Netherlands", "Slovak Republic", "Norway"))
une_reg_all <- filter(une_reg_data, Country %in% c("Denmark", "Netherlands", "Slovak Republic", "Norway"))

#Combine the data
une_all <- rbind(une_ilo_all,une_reg_all)

#Aggregating the data and calculating the ratio between survey based and register based measures
aggregatedata <- une_all %>%
  mutate(Year=as.numeric(substr(Quarter,1,4)))%>%         #Create a Year variable
  group_by(Year, Country, Type) %>%                       #Group by Year, Country and Type
  summarise(Value = mean(Value)) %>%                      #Calculate the mean
  mutate(Survey_over_Register = (lead(Value)/Value))%>%   #Divide survey based values by register based values
  filter(Year%%2==0,Type=="Register based measure")       #Only keep even years

#Reshape the data into a table format
une_ratio = dcast(aggregatedata, Year ~ Country, value.var = "Survey_over_Register")
une_ratio[10,] = as.numeric(c("Total Average", colMeans(une_ratio[-1,2:5], na.rm = T)))
une_ratio[10,1] = "Total Average"

#Latex version of the table
kable(une_ratio, "latex", booktabs = T, digits = 2) %>%
  kable_styling(latex_options = "striped")

#Create an index
une_ilo_all <- une_ilo_all %>%
  group_by(Country)%>%
  mutate(index = (100*Value/Value[which.min(Quarter)]))   #Create index for survey based measure
une_reg_all <- une_reg_all %>%
  group_by(Country)%>%
  mutate(index = (100*Value/Value[which.min(Quarter)]))   #Create index for registed based measure

#Create recession data 
#Source: https://fredhelp.stlouisfed.org/fred/data/understanding-the-data/recession-bars/
Peak <- c("2001-03", "2007-12")                   
Trough <- c("2001-11", "2009-06")                 
recessions <- as.data.frame(cbind(Peak, Trough))
recessions$Peak <- as.yearmon(recessions$Peak)
recessions$Trough <- as.yearmon(recessions$Trough)

#Plot the graphs
Plot3 <- ggplot()+
  geom_line(data = une_ilo_all, mapping = aes(x=Quarter, y=index, color = "survey based measure"))+
  geom_line(data = une_reg_all, mapping = aes(x=Quarter, y=index, color = "register based measure"))+
  geom_rect(data = recessions, aes(xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf,  fill='recession'), 
            alpha=0.2)+
  facet_rep_wrap(~Country, nrow = 2, repeat.tick.labels = 'all')+
  labs(y="Indexed unemployment rate (2000-Q1 = 100)", x="Year", 
       caption = "Source: OECD Labour Force Statistics, FRED")+
  scale_color_manual(name = "", 
                     values = c("survey based measure" = "blue","register based measure" = "red"))+
  scale_fill_manual(name="", 
                    values = c("recession" = "grey"))+
  scale_x_continuous(breaks=seq(2000,2018,length.out = 10))+
  theme_classic() + 
  theme(legend.position = 'top',legend.justification = 'left',axis.text.x = element_text(angle = 90))
Plot3

#Save the graphs
tikz('plot3.tex',width = 5.5, height = 4)
Plot3
dev.off()


