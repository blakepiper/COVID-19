##COVID data pulled from the New York Times on 4/13

library(dplyr)
library(ggplot2)
setwd("/Users/blakefinnegan/Documents/Programming/R/COVID - 19/")
master = read.csv("/Users/blakefinnegan/Documents/Programming/R/COVID - 19/us-states.csv")
head(master)

date = master$date
state = master$state
fips = master$fips
cases = master$cases
deaths = master$deaths
except_NY = subset(master, subset = master$state !="New York")
##All States Cases Visualization
all_cases = ggplot() + 
  geom_line(data=master, aes(y = cases, x = date, color = "All States"), size = 1.25) +
  geom_line(data=except_NY, aes(y = cases, x = date, color = "All Except NY"), size = 1.25) +
  scale_color_manual(values = c("orange", "blue")) + 
  labs(color="Indices") +
  xlab("Time")+
  ylab("Cases")+
  scale_x_discrete(breaks = c("2020-01-21", "2020-02-21", "2020-03-20", "2020-04-13"))
all_cases

##Virginia Cases Visualization
virginia = subset(master, subset = master$state=="Virginia")
VA_cases = ggplot(virginia, aes(y=cases, x=1:nrow(virginia))) + geom_line(size = 1.5, color = "blue")
VA_cases

##New York vs. Virginia Cases Visualization
new_york = subset(master, subset = master$state=="New York")
NY_VA_cases = ggplot() + 
  geom_line(data=new_york, aes(y = cases, x = 1:nrow(new_york), color = "NY"), size = 1.25) +
  geom_line(data=virginia, aes(y = cases, x = 1:nrow(virginia), color = "VA"), size = 1.25) +
  scale_color_manual(values = c("orange", "blue")) + 
  labs(color="Indices") +
  xlab("Time")+
  ylab("Cases")
NY_VA_cases

##Louisiana vs. New York vs. Virginia Cases Visualization
louisiana = subset(master, subset = master$state=="Louisiana")
LA_NY_VA_cases = ggplot() + 
  geom_line(data=new_york, aes(y = cases, x = 1:nrow(new_york), color = "NY"), size = 1.25) +
  geom_line(data=virginia, aes(y = cases, x = 1:nrow(virginia), color = "VA"), size = 1.25) +
  geom_line(data=louisiana, aes(y = cases, x = 1:nrow(louisiana), color = "LA"), size = 1.25)+
  scale_color_manual(values = c("orange", "blue", "red")) + 
  labs(color="Indices") +
  xlab("Time")+
  ylab("Cases")
LA_NY_VA_cases

##Everyone vs. New York Cases Visualizations

everyone_NY_cases = ggplot() + 
  geom_line(data=new_york, aes(y = cases, x = 1:nrow(new_york), color = "NY"), size = 1.25) +
  geom_line(data=except_NY, aes(y = cases, x = 1:nrow(except_NY), color = "Everyone"), size = 1.25) +
  scale_color_manual(values = c("orange", "blue")) + 
  labs(color="Indices") +
  xlab("Time")+
  ylab("Cases")
everyone_NY_cases

