library(readxl) # To read an excel file 
Hospital <- read_excel("D:/Simplilearn/Project Data Sets/7/Hospital.xlsx") # Location of excel file
View(Hospital) # To view the inputted dataset
Hospital
library(dplyr)
library(ggplot2) # To visualize the data

?hist

hist(Hospital$AGE,main = 'Frequency of patients', col = 'Pale green', 
     xlab= 'Age', ylab = 'No of visits')

?attach

attach(Hospital)

?as.factor

AGE <- as.factor(AGE)

?summary

summary(AGE)

?aggregate

?~
  
aggregate(TOTCHG~AGE,FUN = sum, data = Hospital)

?max

max(aggregate(TOTCHG~AGE,FUN = sum, data = Hospital))

hist(APRDRG,col = 'Yellow',main = 'Frequency of Treatments', xlab = 'Treatment Categories',
                    ylab = 'Frequency')

?as.factor

APRDG_fact <- as.factor(Hospital$APRDRG)

summary(APRDG_fact)

?which.max()

which.max(summary(APRDG_fact))

?df

df <- aggregate(TOTCHG~APRDRG,FUN = sum,data = Hospital)

df

df[which.max(df$TOTCHG),]

?na.omit

?is.na

Hospital <- is.na(Hospital)

Hospital$RACE <- as.factor(Hospital$RACE)

model_aov <- aov(TOTCHG~RACE,data = Hospital)

model_aov

summary(model_aov)

summary(Hospital$RACE)

Hospital$FEMALE <- as.factor(Hospital$FEMALE)

?lm

model_lm1 <- lm(TOTCHG~AGE+FEMALE,data = Hospital)

summary(model_lm1)

summary(Hospital$FEMALE)

Hospital$RACE <- as.factor(Hospital$RACE)

model_lm2 <- lm(LOS~AGE+FEMALE+RACE,data = Hospital)

summary(model_lm2)

model_lm3 <- lm(TOTCHG~AGE+FEMALE+RACE+LOS+APRDRG,data = Hospital)

summary(model_lm3)
