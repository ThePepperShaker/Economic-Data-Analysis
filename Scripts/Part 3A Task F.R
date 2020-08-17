########################################################################
# Part 3A: F) Create beveridge curves for four countries of your choice.
########################################################################

#install.packages("tidyverse")
#install.packages("zoo")
#install.packages("lemon")
#install.packages("ggrepel")
#install.packages("tikzDevice")
#Load libraries
library(tidyverse)
library(zoo)
library(lemon)
library(ggrepel)
library(tikzDevice)
#Clear workspace 
rm(list=ls())

# url for dataset: "data7_ilo_unemployment_source_oecd.csv" - data provided in lectures
url_ilo_data<-"https://www.dropbox.com/s/fcfx7y7fbz64j5p/data7_ilo_unemployment_source_oecd.csv?dl=1"
une_data<-read_csv(url_ilo_data)%>%select(Country,Time,Value)%>%
  rename(Une=Value)

# url for dataset: "data8_register_unemployment_source_oecd.csv" - data provided in lecture 
url_reg_data <- "https://www.dropbox.com/s/lsusiyv2l0qnba8/data8_register_unemployment_source_oecd.csv?dl=1"
vac_data<-read_csv(url_reg_data)%>%
  filter(Subject=="Job vacancies, Total, Unfilled vacancies (stock), sa")%>%
  select(Country,Time,Value)%>%rename(Vac=Value)

#url for dataset:  "data9_labourforce_source_oecd.csv" - data provided in lectures 
url_lf_data <- "https://www.dropbox.com/s/7p57z2zdw9sjsih/data9_labourforce_source_oecd.csv?dl=1"
lf_data <- read_csv(url_lf_data)%>%
  select(Country,Time,Value)%>%
  rename(LF=Value)

#Create the vacancy rate
vac_rate_data <- inner_join(vac_data,lf_data,by=c("Country","Time"))%>%
  mutate(JVR=100*Vac/(LF*1000))

# Megre vacancy rate data and unemployment rate data
beveridge_data <- inner_join(vac_rate_data,une_data,by=c("Country","Time"))%>%
  mutate(Quarter=as.yearqtr(Time, format = "Q%q-%Y"),     #Convert quarter to class yearqtr
         q=substr(Time,2,2),                      #Create a variable with the quarter number (1-4)
         year=as.numeric(substr(Time,4,7)),       #Create year variable
         label=ifelse(q==1&year%%2==1,Time,""))   #Return the content of "Time" if "q" = 1 and "year" is even

#Select the countries we need and create two sets of data, before and after the great recession
#2008-2015 data
beveridge_data_after_2008 <- filter(beveridge_data, 
                                    Country %in% c("Austria", "Germany", "Poland", "Portugal"), 
                                    year>2007 & year<2015)
#2000-2007 data
beveridge_data_before_2008 <- filter(beveridge_data, 
                                     Country %in% c("Austria", "Germany", "Poland", "Portugal"), 
                                     year<2008)

#Create graphs of the beveridge curves 
Plot4 <- ggplot()+
  geom_point(data=beveridge_data_after_2008, aes(x=Une,y=JVR),size=1)+
  geom_path(data=beveridge_data_after_2008, aes(x=Une,y=JVR, color = "Q1 2008 - Q4 2014"),size=0.7)+
  geom_point(data=beveridge_data_before_2008, aes(x=Une,y=JVR),size=1)+
  geom_path(data=beveridge_data_before_2008,aes(x=Une,y=JVR, color = "Q1 2000 - Q4 2007"),size=0.7)+
  facet_rep_wrap(~Country, scales = "free",repeat.tick.labels = 'all')+
  scale_color_manual(name = "", 
                     values = c("Q1 2000 - Q4 2007" = "red",
                                "Q1 2008 - Q4 2014" = "blue"))+
  geom_text_repel(data=beveridge_data_after_2008,aes(x=Une,y=JVR,label = label), size = 2.2,  hjust = "inward")+
  geom_text_repel(data=beveridge_data_before_2008,aes(x=Une,y=JVR,label = label), size = 2.2,  hjust = "inward")+
  labs(x="Unemployment rate in percent", y="Job vacancy rate in percent", caption = "Source: OECD")+
  theme_classic() + 
  theme(legend.position = 'top', legend.justification = 'left')
Plot4

#Save the graphs
tikz('plot4.tex',width = 5.5, height = 4)
Plot4
dev.off()


