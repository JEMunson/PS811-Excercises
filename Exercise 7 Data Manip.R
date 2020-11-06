# installed packages for haven and magrittr
# install.packages("haven")
# install.packages("magrittr")

# load libraries
library("here")
library("haven")
library("magrittr")
library("tidyverse")
library("tidyr")
library("dplyr")

#1) Create data frame from table provided

#1a) Create column vectors
justice <- c("Clarence Thomas", "Ruth Bader Ginsburg", "Stephen Breyer", "John Roberts", "Samuel Alito", "Sonia Sotomayor", "Elena Kagan", "Neil Gorsuch", "Brett Kavanaugh")
state <- c("GA", "NY", "MA", "MD", "NJ", "NY", "MA", "CO", "MD")
position <- c("Associate Justice", "Associate Justice", "Associate Justice", "Chief Justice", "Associate Justice", "Associate Justice", "Associate Justice", "Associate Justice", "Associate Justice")
replacing <- c("Thurgood Marshall", "Byron White", "Harry Blackmun", "William Rehnquist", "Sandra Day O'Conner", "David Souter", "John Paul Stevens", "Antonin Scalia", "Anthony Kennedy")
year_confirmed <- c(1991, 1993, 1994, 2005, 2006, 2009, 2010, 2017, 2018)
senate_conf_vote <- c("52-48", "96-3", "87-9", "78-22", "58-42", "68-31", "63-37", "54-45", "50-48")
nominated_by <- c("George H.W. Bush", "Bill Clinton", "Bill Clinton", "George W. Bush", "George W. Bush", "Barack Obama", "Barack Obama", "Donald Trump", "Donald Trump")

#1b) Create a tibble from row vectors
current_supreme_court <- tibble(justice, state, position, replacing, year_confirmed, senate_conf_vote, nominated_by)
print(current_supreme_court)

#1C) Change the tibble to a data frame
supremecourt_df <- as.data.frame(current_supreme_court)
view(supremecourt_df)

# ms: you can just do this:
supremecourt_df <- data.frame(justice, state, position, replacing, year_confirmed, senate_conf_vote, nominated_by)

#2) Download Justices.csv. I'm not sure how to download the file from the
#repository but did figure out I can download it from the URL without a local version
library (readr)

justicesonline <- "https://raw.githubusercontent.com/marcyshieh/ps811/master/data/justices.csv"

justices<-read_csv(url(justicesonline))


#3)Merge data files

#first, download the second set of data to be merged
#this one couldn't be done via the above procedure but luckily I am able to download it locally


# ms: 
SCDB<- read_dta(here("SCDB_2020_01_justiceCentered_Citation.dta"))

#view the data to see if the names for variables are the same in each set
view(SCDB)
view(justices)
#think that's a no

#the names function used in lecture is helpful because the "view" versions are 
#hard to compare and see at the same time

names(SCDB)
names(justices)

table(justices$justiceName)
table(SCDB$justiceName)
#I am unsure why "justices" also includes a variable where the specific justice
#is coded as a number and as a word variable as well.
#these seem like they don't need variables to be renamed


#merge the data sets
# ms: you should specify the variables you are using to "link" the datasets
megaSCOTUS<- inner_join(justices, SCDB)

#4) Filter out justices with Martin-Quinn scores.

SCOTUSScore <- megaSCOTUS %>%
  select("justiceName", "post_mn", "decisionDirection", "term")%>%
  filter(!is.na("post_mn"))

#5)Mean MQ score for each term
MQ_means<- SCOTUSScore%>%
  group_by(term)%>%
  summarise(mean= mean(post_mn, na.rm = TRUE))

#6) Mean decision direction w/ rescaled direction variable

#first mutate the decision directions

mutated<- mutate(megaSCOTUS,
                 decisionDirection=
                   case_when(decisionDirection== 1 ~ 1,
                             decisionDirection == 2 ~ -1,
                             decisionDirection == 3 ~ 0))

#create a new variable in which to collect the MQ data (from Jess)

decision_byterm<- mutated%>%
  group_by(term)%>%
  summarise(mean= mean(decisionDirection, na.rm = TRUE))

print(decision_byterm)
#7) compare the mean Martin-Quinn scores and vote directions

compare<- inner_join(decision_byterm, mutated, by="term")
colnames(compare)<- c("term", "MQ Score", "Vote Direction")

view(compare)

# ms: I think you have the right idea but take a look at the answer key :)
#plot the data
plot<- plot(compare$"MQ Score",compare$"Vote Direction")

#I'm not sure what's gone awry with this graph

## Brainstorm Final Project

# ms: which class is this for?

#What questions are you interested in?
#I will be looking to find the dominant concerns people have about moving to online instruction during the Pandemic

#What are your independent and dependent variables
#rural vs urban origins of posts 
# ms: these are not independent and dependent variables
# ms: think of your question this way--what is your x and what is your y?
# ms: you want to identify a cause and an effect
# ms: what is your hypothesis?

#How do you plan to measure the variables.
#using text analysis of posts using the phrases "as a teacher", "as a student", and "as a parent" to learn what terms are associated with tweets using each phrase

#What data will you need to collect? Which Datasets will you use
# ms: I would maybe focus on specific states or specify which state you're looking at
# ms: not every state/county moved online at the same time...
#Twitter data collected between March 1st, 2020 and July 15th, 2020

#Methods to analyze data
#I will evaluate the frequency at which topics occur in tweets using my selected phrases. 
#.
