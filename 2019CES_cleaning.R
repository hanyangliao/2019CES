library(haven)
library(janitor)
library(tidyverse)
library(reshape)
setwd("C:/Users/18729/Desktop/2019CES")
oldata <- read_dta("2019CESOL.dta")
phdata <- read_dta("2019CESPH.dta")
census_data <- read_csv("pumf-98M0002-E-2016-hierarchical_F1.csv")
oldata <- oldata %>%
  select(cps19_citizenship,
         cps19_gender,
         cps19_province,
         cps19_education,
         cps19_votechoice,#likely people vote
         cps19_vote_unlikely,#unlikely people vote
         cps19_employment,
         cps19_age,)
phdata <- phdata %>%
  select(q1,#citizenship
         q2,#bornage
         q3,#gender
         q4,#province
         q61,#highest education
         q11,#all vote
         q68,#employment
         )
##filter eligibility for vote
oldata <- oldata%>%
  filter(cps19_citizenship==4 & cps19_age>=18)
phdata <- phdata%>%
  filter(q1==1 & q2<=2001)
##combine online survey and phone survey data
###gender
oldata <- oldata %>%
  mutate(gender = case_when(cps19_gender==1~'M',
                            cps19_gender==2~'F',
                            cps19_gender==3~'Other'))
oldata$cps19_gender=NULL
phdata <- phdata %>%
  mutate(gender = case_when(q3==1~'M',
                            q3==2~'F',
                            q3==3~'Other'))
phdata$q3=NULL
###age group
oldata <- oldata%>%
  mutate(age_group = case_when(cps19_age < 20 ~ '19 or less',
                               cps19_age >= 20 & cps19_age < 35 ~ '20 to 34',
                               cps19_age >= 35 & cps19_age < 50 ~ '35 to 49',
                               cps19_age >= 50 & cps19_age < 65 ~ '50 to 64',
                               cps19_age >= 65 & cps19_age < 75 ~ '65 to 74',
                               cps19_age >= 75 ~ '75 and above'))
oldata$cps19_age=NULL
phdata <- phdata %>%
  mutate(age_group = case_when(q2 >1999 ~ '19 or less',
                               q2 <=1999 & q2 >1984 ~ '20 to 34',
                               q2 <=1984 & q2 >1969 ~ '35 to 49',
                               q2 <=1969 & q2 >1954 ~ '50 to 64',
                               q2 <=1954 & q2 >1944 ~ '65 to 74',
                               q2 <=1944 ~ '75 and above'))
phdata$q2=NULL
### Province
oldata <- oldata %>%
  mutate(province = case_when(cps19_province==14~'AB',
                              cps19_province==15~'BC',
                              cps19_province==16~'MB',
                              cps19_province==17~'NB',
                              cps19_province==18~'NL',
                              cps19_province==19~'Northern Canada',
                              cps19_province==20~'NS',
                              cps19_province==21~'Northern Canada',
                              cps19_province==22~'ON',
                              cps19_province==23~'PE',
                              cps19_province==24~'QC',
                              cps19_province==25~'SK',
                              cps19_province==26~'Northern Canada'))
oldata$cps19_province=NULL
phdata <- phdata %>%
  mutate(province = case_when(q4==9~'AB',
                              q4==10~'BC',
                              q4==7~'MB',
                              q4==4~'NB',
                              q4==1~'NL',
                              q4==11~'Northern Canada',
                              q4==3~'NS',
                              q4==13~'Northern Canada',
                              q4==6~'ON',
                              q4==2~'PE',
                              q4==5~'QC',
                              q4==8~'SK',
                              q4==12~'Northern Canada'))
phdata$q4=NULL
###education
oldata <- oldata %>%
  mutate(education = case_when(cps19_education<=5~'highschool or lower',
                               cps19_education>5 & cps19_education<=9~'college to bachelor',
                               cps19_education>=9~'master or higher'))
oldata$cps19_education=NULL
phdata <- phdata %>%
  mutate(education = case_when(q61<=5~'highschool or lower',
                               q61>5 & q61<=9~'college to bachelor',
                               q61>=9~'master or higher'))
phdata$q61=NULL
###vote
oldata <- oldata%>%
  mutate(vote = case_when(cps19_votechoice==1~'liberal party',
                          cps19_vote_unlikely==1~'liberal party',
                          cps19_votechoice==2~'conservative party',
                          cps19_vote_unlikely==2~'conservative party',
                          cps19_votechoice==3~'ndp',
                          cps19_vote_unlikely==3~'ndp',
                          cps19_votechoice==4~'bloc quebecois',
                          cps19_vote_unlikely==4~'bloc quebecois',
                          cps19_votechoice==5~'green party',
                          cps19_vote_unlikely==5~'green party',
                          cps19_votechoice==6~'peoples party',
                          cps19_vote_unlikely==6~'peoples party'))
oldata$cps19_votechoice=NULL
oldata$cps19_vote_unlikely=NULL
phdata <- phdata%>%
  mutate(vote = case_when(q11==1~'liberal party',
                          q11==2~'conservative party',
                          q11==3~'ndp',
                          q11==4~'bloc quebecois',
                          q11==5~'green party',
                          q11==6~'peoples party'))
phdata$q11=NULL
###employment
oldata <- oldata%>%
  mutate(employ = case_when(cps19_employment<=3~'employed',
                              cps19_employment>=9 & cps19_employment<=11~'employed',
                            cps19_employment==4~'not in labour force',
                            cps19_employment>=6 & cps19_employment<=8~'not in labour force',
                              cps19_employment==5~'unemployed'))
phdata <- phdata %>%
  mutate(employ = case_when(q68<=3~'employed',
                            q68>=9 & q68<=11~'employed',
                            q68==4~'not in labour force',
                            q68>=6 & q68<=8~'not in labour force',
                            q68==5~'unemployed'))
oldata$cps19_employment=NULL
phdata$q68=NULL
oldata$cps19_citizenship=NULL
phdata$q1=NULL
###combine online suevey and phone suevey data
survey_data <- rbind(oldata,phdata)
survey_data <- na.omit(survey_data)
survey_data <- survey_data%>%
  filter(vote==c('liberal party','conservative party'))
##clean census data
###filter eligibility
census_data <- census_data%>%
  filter(AGEGRP>=4 & CITIZEN==c(1,2))
census_data$CITIZEN=NULL
census_data$X8=NULL
###age
census_data <- census_data%>%
  mutate(age_group = case_when(AGEGRP >= 4 & AGEGRP < 7 ~ '20 to 34',
                               AGEGRP >= 7 & AGEGRP < 10 ~ '35 to 49',
                               AGEGRP >= 10 & AGEGRP < 12 ~ '50 to 64',
                               AGEGRP == 12 ~ '65 to 74',
                               AGEGRP == 13 ~ '75 and above'))
census_data$AGEGRP=NULL
census_data$EFDIMBM=NULL
###education
census_data <- census_data%>%
  mutate(education = case_when(HDGREE<=2~'highschool or lower',
                               HDGREE>2 & HDGREE<8~'college to bachelor',
                               HDGREE==8~'master or higher'))
census_data$HDGREE=NULL
###gender
census_data <- census_data%>%
  mutate(gender = case_when(SEX==1~'F',
                            SEX==2~'M'))
census_data$SEX=NULL
###province
census_data <- census_data%>%
  mutate(province = case_when(PR==48~'AB',
                              PR==59~'BC',
                              PR==46~'MB',
                              PR==13~'NB',
                              PR==10~'NL',
                              PR==12~'NS',
                              PR==35~'ON',
                              PR==11~'PE',
                              PR==24~'QC',
                              PR==47~'SK',
                              PR==70~'Northern Canada'))
census_data$PR=NULL
###employment
census_data <- census_data%>%
  mutate(employ = case_when(LFTAG<=2~'employed',
                            LFTAG>=11 & LFTAG<=14~'not in labour force',
                            LFTAG>=3 & LFTAG <=10~'unemployed'))
census_data$LFTAG=NULL
census_data <- na.omit(census_data)
###output csv
write_csv(survey_data, "survey_data.csv")
write_csv(census_data, "census_data.csv")
