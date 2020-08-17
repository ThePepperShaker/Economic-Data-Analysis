#########################################################################
# Final Project Part 1: Tidy a messy dataset. Furthermore, aggregate the 
# data, and visualize the data.
#########################################################################

#install.packages("tidyverse")
#Load libraries
library(tidyverse)
#Clear workspace.
rm(list = ls())

#Select file 
file <- "https://raw.githubusercontent.com/1610893/ED-Final-Project/master/ps1data.csv" 
df <- read_csv(file)

#Read in the file and tidy the data as follows:
# 1. gather the columns containing "incomeyear" (i.e columns 2 to 12) and store these into a 
#    and store them in a single column and name it income.
# 2. Seperate the values in "incomeyear" into "delete" and "year" at the 11th character. 
# 3. Create a new "occupation" variable and store the occupations in this
# 4. Delete the original occupation columns and the column created earlier called "delete"
# 5. Finally, arrange the dataset by person_id from 1 to 1000
# These steps are transformed into code below. 
tidydata <- df %>%                                        
  gather(key="incomeyear", value="income", 2:12) %>%             
  separate(incomeyear, into=c("delete", "year"), sep = 11) %>%
  mutate(year = as.numeric(year)) %>%
  mutate(occupation = factor(occupation_busdriver*1 
                             + occupation_cashier*2 
                             + occupation_nurse*3, 
                             levels=c(1,2,3), labels=c("Bus driver","Cashier","Nurse"))) %>%
  select(-c(delete,occupation_busdriver,occupation_cashier,occupation_nurse)) %>%
  arrange(person_id)

#Compute the overall growth rates only by year
aggregatedata <- tidydata %>%
  group_by(year) %>%
  summarise(income = mean(income)) %>%
  mutate(growth = 100*(income-lag(income))/lag(income))

#Compute the overall growth rates by gender and year. Additionally create an index with year 2000 as the base year.
aggregatedata2 <- tidydata %>%
  group_by(gender, year) %>%
  summarise(income = mean(income)) %>%
  mutate(index = 100*(income/income[which.min(year)])) %>%
  mutate(growth = 100*(income-lag(income))/lag(income))

#Compute the overall growth rate by gender, occupation and year. Create the index again, as it is required for the graph.
aggregatedata3 <- tidydata %>%
  group_by(gender, occupation, year) %>%
  summarise(income = mean(income)) %>%
  mutate(index = 100*(income/income[which.min(year)])) %>%
  mutate(growth = 100*(income-lag(income))/lag(income))

#Visualize the data by growth rates using the index as the y-axis and year as the x-axis.
Plot <- ggplot() + 
  geom_point(data = aggregatedata3, aes(x = year, y = index, shape = occupation))+                  #Add scatter plot
  geom_line(data = aggregatedata3, aes(x = year, y = index, shape = occupation, color = gender))+   #Add line graph
  geom_line(data = aggregatedata2, aes(x = year, y = index, color = gender), size = 1.4)+           #Add line graph
  scale_shape_manual(labels=c("Busdriver", "Cashier", "Nurse" ), values = c(0,1,8))+ #Specify shapes of data points
  scale_x_continuous(breaks=seq(2000,2010,length.out = 11))+
  ylab("Annual Income (Index - 2000 = 100)") + 
  xlab("Year")+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 90))
Plot


#################################################
#Preparing graphs and tables for the final report
#################################################

#Load libraries
#install.packages("tikzDevice")
#install.packages("reshape2")
#install.packages("kableExtra")
library(tikzDevice)
library(reshape2)
library(kableExtra)

#Save the graph 
tikz('plot.tex',width = 5.5, height = 4)
Plot
dev.off()

#Sample messy data table for the report
sampleraw <- df[1:10,c(1,2,13,14)]
kable(sampleraw, "latex", booktabs = T, digits = 2)%>%
  kable_styling(latex_options = "striped")

#Sample tidy data table for the report  
sampletidy <- tidydata[1:10,]
kable(sampletidy, "latex", booktabs = T, digits = 2)%>%
  kable_styling(latex_options = "striped")

#Combine and reshape the previously created tables to produce a table with all relevant values
table = dcast(aggregatedata3, year ~ occupation + gender, value.var = "growth")
table2 = dcast(aggregatedata2, year ~ gender, value.var = "growth")
table3 = aggregatedata[,c(1,3)]
table3 <- table3 %>% 
  rename(All = growth)
table4 = left_join(table2, table)
table5 = left_join(table3, table4)
table5[12,] = as.numeric(c("Total Average", colMeans(table5[-1,2:10])))
table5[12,1] = "Total Average"

#Create latex version of the table 
kable(table5, "latex", booktabs = T, digits = 2) %>%
  add_header_above(c(" " = 4, "Bus Driver" = 2, "Cashier" = 2, "Nurse" = 2))%>%
  kable_styling(latex_options = "striped")
