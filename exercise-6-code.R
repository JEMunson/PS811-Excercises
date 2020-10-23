---
  title: 'Exercise 6: Base R vs. Tidyverse'
author: "Jessie Munson"
date: "10/22/2020"
output: pdf_document
---
  
  # load packages
library("here")
library("haven")
library("magrittr")
library("tidyverse")
library("tidyr")
library("dplyr")

# setup folders and directories
here("data")
here("code")

#read in data
victual <- read_csv(here("data", "food_coded.csv"))

#make data frame just in case
as.data.frame(victual)

#view for reference
head(victual)

#extract the first 95 rows, named extracted data frame "victuals"
victuals<- victual[1:95,]

#look at variables
view(victuals$GPA)
view(victuals$calories_chicken)
view(victuals$drink)
view(victuals$fav_cuisine)
view(victuals$father_profession)
view(victuals$mother_profession)

#Create a new variable for how healthy each person feels but convert the scale from 1 to 10 to 1 to 100.

print(victuals$healthy_feeling2)

#Filter to students who are female and have GPAs that are above 3.0.
fem3<- filter(victuals, GPA > 3.0, Gender == 1)
fem3
#arrange favorite cuisine alphabetically
fem3c<- arrange(fem3, fem3$fav_cuisine)
fem3c

fem3cdta<- tibble(fem3c)
#Find the mean and standard deviation for the following variables, and summarize them in a data frame.

 mean(fem3c$calories_chicken)
 mean(fem3c$tortilla_calories)
 mean(fem3c$turkey_calories)
 mean(fem3c$waffle_calories)
 
 sd(fem3c$calories_chicken)
 sd(fem3c$tortilla_calories)
 sd(fem3c$turkey_calories)
 sd(fem3c$waffle_calories)
 
 #summarize in new data frame
 
 fem3_data <- tibble(c( mean(fem3c$calories_chicken),
                 mean(fem3c$tortilla_calories),
                 mean(fem3c$turkey_calories),
                 mean(fem3c$waffle_calories),
                 
                 sd(fem3c$calories_chicken),
                 sd(fem3c$tortilla_calories),
                 sd(fem3c$turkey_calories),
                 sd(fem3c$waffle_calories)))
 
 #summarize GPA -> I'm not sure this is right
 
 summary(fem3cdta$GPA, fem3cdta$weight)
 
 #now to tidyverse stuff
 
 #read in the csv
 veracity<- read.csv(here("data", "facebook-fact-check.csv"))

 #extract the last 500 rows
 veracity2<- veracity %>% top_n(-500)
 
 #Look at the even-numbered column indices only. Identify them by name.
 #I don't understand this
 
 
 #make new coded post type variable
 post_type_coded <- mutate(veracity2,
                           
                           Post.Type = case_when(
                             
                           Post.Type == "link" ~ 1,
                           Post.Type == "photo" ~ 2,
                           Post.Type == "text" ~ 3,
                           Post.Type == "video" ~ 4))
 
 
 
 #page names in reverse order
 arrange(veracity2, desc(Page))

 #Find the mean and standard deviation for the following variables, and summarize them.
 summarise(veracity2,
    share_count.mean = mean(share_count, na.rm = TRUE),
    share_count.sd = sd(share_count, na.rm = TRUE),
    reaction_count.mean = mean(reaction_count),
    reaction_count.sd = sd(reaction_count),
    comment_count.mean = mean(comment_count),
    comment_count.sd = sd(comment_count))
 
 #na.rm removed the NAs I was getting in the summary.
 
 
#Summarize the mean and standard deviations in Question 7 with the
 #"mainstream" values in the `category` variable.
 
veracity2 %>% 
  group_by("mainstream", Category) %>% 
 summarise(veracity2,
           share_count.mean = mean(share_count, na.rm = TRUE),
           share_count.sd = sd(share_count, na.rm = TRUE),
           reaction_count.mean = mean(reaction_count),
           reaction_count.sd = sd(reaction_count),
           comment_count.mean = mean(comment_count),
           comment_count.sd = sd(comment_count))
 

#Jess helped me with the last two as I don't understand the tidyverse yet.
#Do you know of any good tidyverse primers I could refer to? Especially one that talks
#about piping? 