#Read in data
national<- read.csv("national.csv")

#load necessary packages as instructed
library(dplyr)
library(purrr)
library(tidyr)
library(tidyverse)

#loop for number of variables(there are 79)
for (i in 1:79) {
  i/1995
}
print(i)

#Problem 2a:
#find average number of protestant christian per country
#using base R:
tapply(national$christianity_protestant, national$state, mean)

#Problem 2b:
#using Tidyverse:
national %>%
  group_by(state) %>%
  summarize(
    mean_nom = mean(christianity_protestant, na.rm = TRUE)
  )

#Problem III:
#check column type for each variable that is composed in characters
#instead of numbers
sapply(national, class)


#Problem IV:
#Log buddhism variables
log(national$buddhism_all)

#Problem V:
#Write function listing all unique years with more than 300,000 Christians in total.
unique(national$year[national$christianity_all>300000])

#Problem VI:
#Group by code variable
national %>%
  group_by(code) %>%
  nest()


#Problem VII:
#Create model column

model <- lm(judaism_percent ~ dual_religion, data = national)


#Problem VIII:
#Extract the coefficients into a new column

coefficients <- c(summary(model)$coefficients[1],summary(model)$coefficients[2])


#Problem IX:
#Look at coefficients
print(coefficients)


#Problem X:
#Pull out model column
coefs <- nested_coefs %>%
  unnest(coefs) %>%
  print()


#Problem XI:
#Un-nest the coefficients
coefs <- nested_coefs %>%
  unnest(coefs) %>%
  print()