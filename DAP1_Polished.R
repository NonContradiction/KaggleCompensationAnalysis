##########################
#Analysis of Kaggle Salary Survey Data
#Data Analysis Project #1 for ST 765, Fall MMXIX
#Noah Diekemper
##########################

#IMPORTS
#for fread(): 
library(data.table)
#for filtering and such: 
library(tidyverse)
#for all manner of things
library(dplyr)
#our script uses popup window graphics with dev.new(), a feature of the library from the book this semester worked with, thusly: 
library(HH)

#We read in our data: 
RawData <- fread(file="C:/Users/NoahDiekemper/Downloads/multipleChoiceResponses.csv", 
                      header=TRUE, 
                      sep = ',')

#If we want to inspect it: 
#View(RawData)

my_tibble <- as_tibble(RawData)

#Features of interest: 
bettercols <- my_tibble %>% dplyr::select(Country, 
       GenderSelect, 
       Age, 
       EmploymentStatus, 
       CodeWriter, 
       CurrentJobTitleSelect, 
       TitleFit, 
       CurrentEmployerType, 
       LanguageRecommendationSelect, 
       EmployerIndustry, 
       EmployerSize, 
       JobSatisfaction, 
       CompensationAmount, 
       CompensationCurrency)

#To do an apples-to-apples comparison, we filter for one Country/Sex/PayCurrency
filter1 <- bettercols[which(bettercols$Country == 'United States'), names(bettercols)]
filter2 <- filter1[which(filter1$GenderSelect == 'Male'), names(filter1)]
filter3 <- filter2[which(filter2$CompensationCurrency == 'USD'), names(filter2)]

#And we want salaries, not unemployed numbers: 
filter3 <- filter3[which(filter3$EmploymentStatus != 'Not employed, and not looking for work'), names(filter3)]
filter3 <- filter3[which(filter3$EmploymentStatus != 'Not employed, but looking for work'), names(filter3)]

#Get some data typing in order
rawmoney <- filter3$CompensationAmount
filter3$CompensationAmount <- as.numeric(as.character(rawmoney))

#Address/Interpolate missing data
filter3$CompensationAmount[filter3$CompensationAmount<10] <- 0
filter3$CompensationAmount[is.na(filter3$CompensationAmount)] <- 0
medsal <- median(filter3$CompensationAmount[filter3$CompensationAmount!=0])
filter3$CompensationAmount[filter3$CompensationAmount==0] <- medsal
filter3 <- filter3[which(filter3$CompensationAmount < 225000), names(filter3)]
filter3 <- filter3[which(filter3$EmployerIndustry != ''), names(filter3)]

#Bar graph
#Employer Industry
counts <- table(filter3$EmployerIndustry)
dev.new()
par(las=2)
par(mar=c(5,15,4,2)) # increase y-axis margin.
barplot(counts, main="Employer Industry", horiz=TRUE)
mym<-mean(filter3$CompensationAmount)
#Ironically, the powerpoint graph has an issue at the bottom that isn't reproduced.

#Box and whiskers plot: 
dev.new()
bwplot (x=filter3$CompensationAmount~filter3$EmployerIndustry,
        horizontal=FALSE,
        main='Boxplot of Means of Compensation Amount', 
        ylab='$US',
        panel=function(...) {
          panel.abline(h=mym, col="green")
          panel.bwplot(...)
        })


#STATISTICAL ANALYSIS

##Variable 1: 
#Employer Industry

numbers <- filter3$EmployerIndustry
filter3$EmployerIndustry <- as.factor(numbers)
salmeans.aov <- aov(CompensationAmount~EmployerIndustry, data=filter3)
anova(salmeans.aov)
summary(salmeans.aov)

model.tables(salmeans.aov, "means")

#WONT WORK ***unless you feed it FACTORS, not CHARS***
salmm.mmc <- mmc(salmeans.aov)
salmm.mmc 

#Variable 2: Job Satisfaction

#Data prep to finesse the entries 
rawjob <- filter3$JobSatisfaction
filter3$JobSatisfaction <- as.character(rawjob)

filter3$JobSatisfaction[filter3$JobSatisfaction=='10 - Highly Satisfied'] <- '10'
filter3$JobSatisfaction[filter3$JobSatisfaction=='1 - Highly Dissatisfied'] <- '1'
filter3$JobSatisfaction[filter3$JobSatisfaction==''] <- '0'
filter3$JobSatisfaction[filter3$JobSatisfaction=='I prefer not to share'] <- '0'
#And to make it numeric once again
numbers <- filter3$JobSatisfaction
filter3$JobSatisfaction <- as.numeric(numbers)

#Another bar plot, real quick
counts <- table(filter3$JobSatisfaction)
dev.new()
par(las=2)
barplot(counts, main="Job Satisfaction")

#Another box and whiskers plot: 
#mym<-mean(filter3$CompensationAmount)
dev.new()
bwplot (x=filter3$CompensationAmount~filter3$JobSatisfaction,
        horizontal=FALSE,
        main='Boxplot of Means of Compensation Amount', 
        ylab='$US',
        xlab = list(sort(unique(filter3$JobSatisfaction))),
        panel=function(...) {
          panel.abline(h=mym, col="green")
          panel.bwplot(...)
        })

#Statistical Analysis
#Making it a factor
numbers <- filter3$JobSatisfaction
filter3$JobSatisfaction <- as.factor(numbers)

satismeans.aov <- aov(CompensationAmount~JobSatisfaction, data=filter3)
anova(satismeans.aov)

model.tables(satismeans.aov, "means")

satmm.mmc <- mmc(satismeans.aov)
satmm.mmc 

#Variable 3: Employer Size

#box and whiskers plot again: 
dev.new()
bwplot (x=filter3$CompensationAmount~filter3$EmployerSize,
        horizontal=FALSE,
        main='Boxplot of Means of Compensation Amount', 
        ylab='$US',
        panel=function(...) {
          panel.abline(h=mym, col="green")
          panel.bwplot(...)
        })

#char -> factor for the benefit of the mmc()
numbers <- filter3$EmployerSize
filter3$EmployerSize <- as.factor(numbers)

#Fixing this issue to ward off an error message: 
levels(filter3$EmployerSize)
levels(filter3$EmployerSize)[1] <- "Blank"

#ANOVA: 
sizemeans.aov <- aov(CompensationAmount~EmployerSize, data=filter3)
anova(sizemeans.aov)

model.tables(sizemeans.aov, "means")

#Multiple means comparisons yet again: 
sizemm.mmc <- mmc(sizemeans.aov)
sizemm.mmc 


