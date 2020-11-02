#The code for the cleaning steps are included in this R code.

library(haven)
library(tidyverse)
library(labelled)
library(tidybayes)
library(ROCR)
library(brms)
library(caret)
library(lme4)
library(pROC)

### CLEANING ###

## SURVEY DATA

raw_data <- read_dta("ns20200625.dta")
raw_data <- labelled::to_factor(raw_data)

# Just keep some variables
reduced_data <- 
  raw_data %>% 
  select(vote_2020,
         vote_intention,
         registration,
         age,
         gender,
         education,
         state,
         household_income,
         race_ethnicity)

# Make it a binary vote(vote for Trump or not) because we are going to use 
# logistic regression
reduced_data<-
  reduced_data %>%
  mutate(vote_trump = 
           ifelse(vote_2020=="Donald Trump", 1, 0))

reduced_data<-reduced_data %>%filter(vote_intention!="No, I am not eligible to vote"&
                                       vote_intention!="No, I will not vote but I am eligible")

reduced_data<-reduced_data %>%filter(vote_2020!="	I am not sure/don't know")


# Omitting NA
reduced_data <- na.omit(reduced_data)

##CENSUS DATA

raw_data_2 <- read_dta("usa_00004.dta.gz")

# Add the labels
raw_data_2 <- labelled::to_factor(raw_data_2)

# Keep some variables that may be of interest (9 variables selected)
reduced_data_2 <- raw_data_2 %>% 
  select(perwt, statefip, hhincome, sex, age, race,)

# Only adults can vote, filtering for age >= 18
reduced_data_2$ age <- as.numeric(reduced_data_2$age)
data_filt <- reduced_data_2 %>% filter(age >= 18)

# Omitting NA
data_filt <-na.omit(data_filt)

## Mapping between census and survey

# Mapping sex-gender

data_filt $ sex <- ifelse(data_filt $ sex=="female","Female","Male")
data_filt <- rename(data_filt,gender=sex)

# age

# put age into seven groups, convert to categorical variables.
# age <= 25 denotes people who are younger than 25 with the eligibility to vote
# (bigger than 18 but smaller than 25)

reduced_data <- reduced_data %>% 
  mutate(age_group = case_when(age <=25 ~ '25 and less', age > 25  & age <= 35 ~ '25 ~ 35', age > 35  & age <= 45 ~ '35 ~ 45',
                               age > 45  & age <= 55 ~ '45 ~ 55',age > 55  & age <= 65 ~ '55 ~ 65',age > 65  & age <= 75 ~ '65 ~ 75',age > 75 ~ 'above 75'))

data_filt<-data_filt %>% 
  mutate(age_group = case_when(age <=25 ~ '25 and less',
                               age > 25  & age <= 35 ~ '25 ~ 35', age > 35  & age <= 45 ~ '35 ~ 45',
                               age > 45  & age <= 55 ~ '45 ~ 55', age > 55  & age <= 65 ~ '55 ~ 65',
                               age > 65  & age <= 75 ~ '65 ~ 75', age > 75 ~ 'above 75'))

# race - race_ethnicity
# Mapping between 'race' and 'race_ethnicity' ,mutate race into four major 
# categories: White; Black/African American; Asian and Other race.

# I consider race as a very important factor in the model. Many Trump supporters
# are known as "red neck". Their typical profile is white Americans that are earning 
# lower to middle income (usually blue collar worker). 
# Trump are not popular among Black or African Americans.

asian<-c("Asian (Asian Indian)", "Asian (Vietnamese)",
         "Asian (Other)", "Asian (Chinese)", 
         "Pacific Islander (Native Hawaiian)","Pacific Islander (Other)",
         "Pacific Islander (Samoan)", "Asian (Filipino)",
         "Asian (Japanese)","Pacific Islander (Guamanian)", "Asian (Korean)")

reduced_data<-reduced_data %>% 
  mutate(race = case_when(race_ethnicity=="American Indian or Alaska Native"~"Other race",
                          race_ethnicity =="Some other race" ~ 'Other race',
                          race_ethnicity =="Black, or African American" ~ 'Black or African American',
                          race_ethnicity=="Other race "~"Other race",
                          race_ethnicity =="White" ~ 'White',
                          race_ethnicity %in% asian ~"Asian"))
reduced_data$race_ethnicity<-NULL


data_filt<-data_filt %>% 
  mutate(race2 = case_when(race=="white"~"White",
                           race=="chinese"~"Asian",
                           race=="japanese"~"Asian",
                           race=="two major races"~"Other race",
                           race=="other asian or pacific islander"~"Asian",
                           race=="other race, nec"~"Other race",
                           race=="three or more major races"~"Other race",
                           race=="black/african american/negro"~"Black or African American",
                           race=="american indian or alaska native"~"Other race"))

data_filt$race<-data_filt$race2
data_filt$race2<-NULL


# HHIncome

x<-unique(reduced_data $ household_income)

data_filt <- data_filt  %>% 
  mutate(household_income = case_when(hhincome<=14999 ~ "Less than $14,999",
                                      hhincome>=15000 & hhincome<=19999~"$15,000 to $19,999",
                                      hhincome>=20000 & hhincome<=24999~"$20,000 to $24,999",
                                      hhincome>=25000 & hhincome<=29999~"$25,000 to $29,999",
                                      hhincome>=30000 & hhincome<=34999~"$30,000 to $34,999",
                                      hhincome>=35000 & hhincome<=39999~"$35,000 to $39,999",
                                      hhincome>=40000 & hhincome<=44999~"$40,000 to $44,999",
                                      hhincome>=45000 & hhincome<=49999~"$45,000 to $49,999",
                                      hhincome>=50000 & hhincome<=54999~"$50,000 to $54,999",
                                      hhincome>=55000 & hhincome<=59999~"$55,000 to $59,999",
                                      hhincome>=60000 & hhincome<=64999~"$60,000 to $64,999",
                                      hhincome>=65000 & hhincome<=69999~"$65,000 to $69,999",
                                      hhincome>=70000 & hhincome<=74999~"$70,000 to $74,999",
                                      hhincome>=75000 & hhincome<=79999~"$75,000 to $79,999",
                                      hhincome>=80000 & hhincome<=84999~"$80,000 to $84,999",
                                      hhincome>=85000 & hhincome<=89999~"$85,000 to $89,999",
                                      hhincome>=90000 & hhincome<=94999~"$90,000 to $94,999",
                                      hhincome>=95000 & hhincome<=99999~"$95,000 to $99,999",
                                      hhincome>=100000 & hhincome<=124999~"$100,000 to $124,999",
                                      hhincome>=125000 & hhincome<=149999~"$125,000 to $149,999",
                                      hhincome>=150000 & hhincome<=174999~"$150,000 to $174,999",
                                      hhincome>=175000 & hhincome<=199999~"$175,000 to $199,999",
                                      hhincome>=200000 & hhincome<=249999~"$200,000 to $249,999",
                                      hhincome>=250000~"$250,000 and above")) 

data_filt $ hhincome <- NULL


# State - statefip

# Some states or cities in US are historically known as being discriminatory
# Trump's 'America First' ideology is in line with those thoughts.
# Thus it is very likely that Trump is more popular within some states.

data_filt <- data_filt %>% 
  mutate(state = case_when(statefip=="alabama"~"AL",statefip=="alaska"~"AK",
                           statefip=="arkansas"~"AR", statefip=="arizona"~"AZ",
                           statefip=="california"~"CA", statefip=="colorado"~"CO",                        
                           statefip=="connecticut"~"CT",  statefip=="delaware"~"DE",                        
                           statefip =="florida"~"FL", statefip =="georgia"~"GA",                        
                           statefip =="hawaii"~"HI", statefip =="idaho"~"ID",  statefip =="illinois"~"IL",                       
                           statefip =="iowa"~"IA",statefip =="indiana"~"IN",  statefip =="kansas"~"KS",                       
                           statefip =="kentucky"~"KY", statefip =="louisiana"~"LA",statefip =="massachusetts"~"MA",
                           statefip =="maryland"~"MD", statefip =="maine"~"ME", statefip =="michigan"~"MI",                       
                           statefip =="minnesota"~"MN", statefip =="missouri"~"MO", statefip =="mississippi"~"MS",                       
                           statefip =="montana"~"MT", statefip =="north carolina"~"NC", statefip =="north dakota"~"ND",
                           statefip =="nebraska"~"NE", statefip =="new hampshire"~"NH",                      
                           statefip =="new jersey"~"NJ", statefip =="new mexico"~"NM",
                           statefip =="nevada"~"NV", statefip =="new york"~"NY",                        
                           statefip =="ohio"~"OH",statefip =="oklahoma"~"OK", statefip =="oregon"~"OR",
                           statefip =="pennsylvania"~"PA",statefip =="rhode island"~"RI",
                           statefip =="south carolina"~"SC",statefip =="south dakota"~"SD",
                           statefip =="tennessee"~"TN", statefip =="texas"~"TX",  statefip =="utah"~"UT",                       
                           statefip =="virginia"~"VA", statefip =="vermont"~"VT",                       
                           statefip =="washington"~"WA", statefip =="wisconsin"~"WI",                       
                           statefip =="west virginia"~"WV", statefip =="wyoming"~"WY",                        
                           statefip =="district of columbia"~"DC"))                         

data_filt $ statefip <-NULL

unique(data_filt$state)
unique(reduced_data$state)

### POPULATING DATASET###

data_s<-reduced_data %>% select(vote_2020, vote_trump, age, age_group, gender, state, 
                                household_income, race)
reduced_data
data_c<-data_filt %>% select(perwt,age,age_group,gender,state,
                             household_income,race)

rm(data_filt, reduced_data)

#Convert variables to factor

f.cols.survey<-c("age_group","gender","state","household_income" ,
                 "race", "vote_2020")
data_s[f.cols.survey] <- lapply(data_s[f.cols.survey], factor) 

f.cols.census<-c("age_group","gender","state",
                 "household_income" ,"race")
data_c[f.cols.census] <- lapply(data_c[f.cols.census], factor) 
data_s$vote_2020 <- relevel(data_s $vote_2020, ref = "Donald Trump") 

###MODEL###

# Empirically, gender shall not have a significant impact on one's political
# preference, so in model1 I removed gender from my model and use function lm().

model2 <- glm(vote_trump ~ age_group+gender+race+state+household_income,data = data_s, family= binomial)

summary(model2)
prob2 <- predict(model2, type=c('response'))
model2_res <- ifelse(prob2 >= 0.5, "Joe Biden", "Donald Trump")
data_s_res2 <- cbind(data_s, model2_res)
roc2 <- roc(data_s_res2$vote_2020, prob2)
auc(roc2)
plot(roc2,print.auc = TRUE)

# AUC = 0.7028

post_stra <- 
  data_c %>%
  count(gender,race,state,household_income,age_group) %>%
  group_by(gender,race,state,household_income,age_group) 

post_stra$estimate <-
  model2 %>%
  predict(newdata = post_stra, type = "response")

post_stra$estimate <-
  exp(post_stra$estimate)/(1+exp(post_stra$estimate))

post_stra$alp_predict_prop <-post_stra$estimate*post_stra$n 
sum(post_stra$alp_predict_prop)/sum(post_stra$n)