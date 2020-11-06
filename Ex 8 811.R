USArrests


#Graphs in Base R

#plot murders and assaults in US Arrests data set
plot(USArrests$Murder, USArrests$Assault, xlab = "Murder Arrests", ylab = "Assault Arrests", 
     main = "Correlations between Murder and Assault Arrests in the United States")

#US Rape Arrests
boxplot(USArrests$Rape, main = "US Rape Arrests", ylab = "Rape Arrests", ylim = c(0,50))

#Rape arrests per state
state.names = row.names(USArrests)
barplot(USArrests$Rape, names.arg = state.names, las = 2, ylab= "Rape Arrests per 100,000", 
        main = "Rape Rate in the United States in 1973", ylim=c(0,50))

#histogram of percentage of urban population
propurban<- prop.table(USArrests$UrbanPop)
hist(propurban, main= "Percent of Urban Population", xlab = "Percent Urban Population")
#not sure why the scale is off - I could not manipulate the limits of the axes

library("magrittr")
library("tidyverse")
library("ggplot2")
library("scales")
library("diplyr")

#Graphs with ggplot 2
#plot murders and assaults in US Arrests data set
ggplot(data = USArrests, aes(x = Murder, y = Assault)) + geom_point()

#make a boxplot of US Rape Arrests
USArrestsRape <- as.factor(USArrests$Rape)
p <- ggplot(USArrests, aes(x=, y=Rape)) + 
  geom_boxplot() + ggtitle(label = "Rape Arrests")
+ ylab(label="Rape Arrests")
p

#rape arrests by state
State<-row.names(USArrests)
r <- ggplot(USArrests, aes(Rape, State)) + 
  geom_bar(stat="identity") +ggtitle(label = "Rape Arrests per State") +
  xlab(label="Rape Arrests")
r


#histogram of percentage of urban population

h <- ggplot(USArrests, aes(x= propurban)) + 
  geom_histogram()+ggtitle(label = "Proportion of Urban Population") + xlab(label="Proportion of Urban Poplulation")+ 
  ylab(label="Frequency")
h

#My Project Graphs

#.