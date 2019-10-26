###############################################################################################################
# Final Project Part 3B: D) Create your own visualization that cconveys the same message as the figure in 3B:C)
###############################################################################################################

#install.packages("tidyverse")
#install.packages("lemon")
#install.packages("tikzDevice")
#Load libraries 
library(tidyverse)
library(lemon)
library(tikzDevice)
#Clear workspace
rm(list = ls())

#Load data 
file <- "https://raw.githubusercontent.com/1610893/ED-Final-Project/master/DebtToIncome.csv"
file2 <- "https://raw.githubusercontent.com/1610893/ED-Final-Project/master/Assets.csv"
DebtRatio <- read_csv(file)
Assets <- read_csv(file2)


#Tidy the data
Assets <- Assets%>%
  gather(key = "income_bracket", value = "share", 2:5)%>%
  filter(!is.na(share), income_bracket!="All Households")
DebtRatio <- DebtRatio%>%
  gather(key = "income_bracket", value = "share", 2:5)%>%
  filter(!is.na(share), income_bracket!="All Households")

#Plot the asset holding data
Plot6 <- ggplot(data=Assets, aes(x=Asset,y=share,fill=income_bracket)) +
  geom_bar(position="dodge",stat="identity") + 
  labs(x="Asset type", y="Share in percent")+
  scale_fill_brewer(name="Wealth class", palette = "Pastel1")+
  theme_classic()+
  coord_flip()+
  theme(legend.position = 'top',
        legend.justification = 'left')+
  guides(fill=guide_legend(nrow=3,byrow=TRUE))
Plot6

#Plot the debt/income ratio data
Plot7 <- ggplot(data=DebtRatio, aes(x=income_bracket,y=share, fill=income_bracket)) +
  geom_bar(position="dodge",stat="identity") + 
  labs(x=" ", y="Debt to income ratio")+
  scale_fill_brewer(name="Wealth class", palette = "Pastel1")+
  theme_classic()+
  theme(legend.position = 'top',
        legend.justification = 'left', 
        axis.ticks.x = element_blank(), 
        axis.text.x = element_blank())+
  guides(fill=guide_legend(nrow=3,byrow=TRUE))
Plot7

#Save the graphs 
#Create tex file of graphs for the report
tikz('plot6.tex',width = 5., height = 4)
Plot6
dev.off()
tikz('plot7.tex',width = 5, height = 4)
Plot7
dev.off()







