national <- read.csv("/Users/marcyshieh/Downloads/archive (1)/national.csv")

#Read in data
national<- read.csv("national.csv") # ms: where are you reading in the data from?

#load necessary packages as instructed
library(dplyr)
library(purrr)
library(tidyr)
library(tidyverse)

#loop for number of variables(there are 79)
# ms: check answer key
for (i in 1:79) {
  i/1995
}
print(i)


#Problem 2a:
#find average number of protestant christian per country
#using base R:
# ms: awesome!
tapply(national$christianity_protestant, national$state, mean)

#Problem 2b:
#using Tidyverse:
# ms: cool!
national %>%
  group_by(state) %>%
  summarize(
    mean_nom = mean(christianity_protestant, na.rm = TRUE)
  )

#Problem III:
#check column type for each variable that is composed in characters
#instead of numbers
# ms: perf!
sapply(national, class)


#Problem IV:
#Log buddhism variables
# ms: correct.
log(national$buddhism_all)

#Problem V:
#Write function listing all unique years with more than 300,000 Christians in total.
# ms: this works, but i will add a more elegant version in the answer key...
unique(national$year[national$christianity_all>300000])

#Problem VI:
#Group by code variable
# ms: awesome!
national %>%
  group_by(code) %>%
  nest()


#Problem VII:
#Create model column
#ms: this is not the function and the DV should be dual_religion and the IV should be judiasm_percent...check the answer key
model <- lm(judaism_percent ~ dual_religion, data = national)


#Problem VIII:
#Extract the coefficients into a new column
# ms: this works but there's a simpler way to do this...check answer key
coefficients <- c(summary(model)$coefficients[1],summary(model)$coefficients[2])


#Problem IX:
#Look at coefficients
# ms: good
print(coefficients)


#Problem X:
#Pull out model column
# ms: check answer key... but it is not the same
coefs <- nested_coefs %>%
  unnest(coefs) %>%
  print()


#Problem XI:
#Un-nest the coefficients
# ms: this is correct...
coefs <- nested_coefs %>%
  unnest(coefs) %>%
  print()

