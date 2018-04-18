#Group 10 R Script file.
#Team 10:
#Akash Tiwari
#Anchal Mukesh
#Meera Pradeep
#Ravi Gupta
#Shirin Bhat

#========================================================================================

View(Real_estate_NY_consolidated)
Real_estate_NY_consolidated<-Real_estate_NY_consolidated[,c(1:25)]

#Data cleaning was done by removinf rows that contained mostly NA values as we had large dataset and removinf few rows did not affect the model.
#Outliars were removed by plotting each variable and then using identify function and by using regression testing.



attach(Real_estate_NY_Project)

#Integrating the walkscore API

Real_estate_NY_Project$Walkscore[i]<-getWS(Real_estate_NY_Project$Longitude[i],Real_estate_NY_Project$Latitude[i],"24dde54ead291ea8d1c09476aa2ed2c2")$walkscore

#library

library(ggplot2)
library(gridExtra)
library(lubridate) 
library(tidyverse) 
library(psych)
library(dplyr)
library(walkscoreAPI)

#Data Cleaning and Transformation

Real_estate_NY_Project$Zone<-as.factor(Real_estate_NY_Project$Zone)
Real_estate_NY_Project$`Condominiums  Neighborhood`<-as.factor(Real_estate_NY_Project$`Condominiums  Neighborhood`)
Real_estate_NY_Project$`Condominiums Building Classification`<-as.factor(Real_estate_NY_Project$`Condominiums Building Classification`)
Real_estate_NY_Project$WalkscoreCategories<-as.factor(Real_estate_NY_Project$WalkscoreCategories)
Real_estate_NY_Project$YearBuiltCategories<-as.factor(Real_estate_NY_Project$YearBuiltCategories)
Real_estate_NY_Project$`Condominums Boroughs`<-as.factor(Real_estate_NY_Project$`Condominums Boroughs`)

#Transformation

#Walkscore Tranformation

Real_estate_NY_Project$WalkscoreCategories[Real_estate_NY_Project$`Condominiums Walkscore`<80]<-"Car-Dependent"
Real_estate_NY_Project$WalkscoreCategories[Real_estate_NY_Project$`Condominiums Walkscore`>=80]<-"Somewhat Walkable"
Real_estate_NY_Project$WalkscoreCategories[Real_estate_NY_Project$`Condominiums Walkscore`>95]<-"Walker's Paradise"

#Year Category Tranformation

Real_estate_NY_Project$YearBuiltCategories[Real_estate_NY_Project$`Condominiums Year Built`<2000]<-"Before_2000"
Real_estate_NY_Project$YearBuiltCategories[Real_estate_NY_Project$`Condominiums Year Built`>=2000]<-"After_2000"

#Univariate Analysis

table(`Condominums Boroughs`)
barplot(table(`Condominums Boroughs`), main="Condominium distribution borough wise",ylab="No of Condominum",col=c("orange", "lightblue4","green","red","grey"))

table(Real_estate_NY_Project$WalkscoreCategories)
barplot(table(Real_estate_NY_Project$WalkscoreCategories), main="No. of Condos.by Walkscore categories", ylab="No of Condominum",col=c("lightblue4","grey","red"))

table(Real_estate_NY_Project$`Condominiums Building Classification`)
barplot(table(Real_estate_NY_Project$`Condominiums Building Classification`), main="No. of Condos.By Building Classification", ylab="No of Condominum",col=c("yellow","Blue","red","Green"))

table(Real_estate_NY_Project$YearBuiltCategories)
barplot(table(Real_estate_NY_Project$YearBuiltCategories), main="No. of Condos. Milleniam wise",ylab="No of Condominum",col=c("Black","Green"))

plot(density(Real_estate_NY_Project$`Condominiums Total Units`))
plot(density(log(Real_estate_NY_Project$`Condominiums Total Units`)))

describe(`Condominiums Total Units`)
describe(`Condominiums Gross SqFt`)

describe(log(`Condominiums Total Units`))
describe(log(`Condominiums Gross SqFt`))

plot(density(l(`Condominiums Gross SqFt`)))
plot(density(log(`Condominiums Gross SqFt`)))

#BiVariate Analysis

#1.	Market Price per sq.ft. and Condominium Building Classification

table(`Condominiums Building Classification`)

barplot(table(`Condominiums Building Classification`),xlab="Building Classification", ylab="No. of Condominiumns",col=c("orange", "lightblue4","green","red"))
boxplot(`Condominiums Market Value per SqFt`~ `Condominiums Building Classification`,xlab="Building Classification", ylab="Market price per sq. ft.",col=c("orange", "lightblue4","green","red"))

mpsc<-Real_estate_NY_Project%>% group_by(`Condominiums Building Classification`) %>% summarise(avg = mean(`Condominiums Market Value per SqFt`),med = median(`Condominiums Market Value per SqFt`), std= sd(`Condominiums Market Value per SqFt`))
mpsc

price.aov <-aov(`Condominiums Market Value per SqFt`~ as.factor(`Condominiums Building Classification`), data=Real_estate_NY_Project)

summary(price.aov)
par(mfrow=c(2,2))
plot(price.aov)

price.tk<-TukeyHSD((price.aov))
dev.off()
plot(price.tk)

#2.	Market Price per sq. ft. and Zone


table(Zone)

mpsc<-Real_estate_NY_Project%>% group_by(Zone) %>% summarise(avg = mean(`Condominiums Market Value per SqFt`),med = median(`Condominiums Market Value per SqFt`), std= sd(`Condominiums Market Value per SqFt`))
mpsc

barplot(table(Zone),xlab="Zone", ylab="No. of Condominiumns",col=c("orange", "lightblue4","green","red","blue"))
boxplot(`Condominiums Market Value per SqFt`~ Zone,xlab="Zone", ylab="No. of Condominiumns",col=c("orange", "lightblue4","green","red","blue"))

price.aov <-aov(`Condominiums Market Value per SqFt`~ as.factor(Zone), data=Real_estate_NY_Project)
summary(price.aov)

par(mfrow=c(2,2))
plot(price.aov)

price.tk<-TukeyHSD((price.aov))
dev.off()
plot(price.tk)

#3.	Market Price per sq. ft. and Condominiums Borough

table(`Condominums Boroughs`)
mpsc<-Real_estate_NY_Project%>% group_by(`Condominums Boroughs`) %>% summarise(avg = mean(`Condominiums Market Value per SqFt`),med = median(`Condominiums Market Value per SqFt`), std= sd(`Condominiums Market Value per SqFt`))
mpsc

barplot(table(`Condominums Boroughs`),xlab="Condominums Boroughs", ylab="No. of Condominiumns",col=c("orange", "lightblue4","green","red","blue"))
price.aov <-aov(`Condominiums Market Value per SqFt`~ as.factor(`Condominums Boroughs`), data=Real_estate_NY_Project)

par(mfrow=c(2,2))
plot(price.aov)
summary(price.aov)

price.tk<-TukeyHSD((price.aov))
dev.off()
plot(price.tk)

#4.	Market Price per sq. ft. and WalkScore Categories

table(Real_estate_NY_Project$WalkscoreCategories)
mpsc<-Real_estate_NY_Project%>% group_by(WalkscoreCategories) %>% summarise(avg = mean(`Condominiums Market Value per SqFt`),med = median(`Condominiums Market Value per SqFt`), std= sd(`Condominiums Market Value per SqFt`))
mpsc

barplot(table(WalkscoreCategories),xlab="Walkscore Categories", ylab="No. of Condominiumns",col=c("orange", "lightblue4","green","red","blue"))
boxplot(`Condominiums Market Value per SqFt`~ WalkscoreCategories,xlab="WalkScore Categories", ylab="No. of Condominiumns",col=c("green","red","blue"))

price.aov <-aov(`Condominiums Market Value per SqFt`~ as.factor(WalkscoreCategories), data=Real_estate_NY_Project)
summary(price.aov)

par(mfrow=c(2,2))
plot(price.aov)
price.tk<-TukeyHSD((price.aov))
dev.off()
plot(price.tk)


#5.	Market Price per sq. ft. and YearBuilt Categories

table(YearBuiltCategories)
mpsc<-Real_estate_NY_Project%>% group_by(YearBuiltCategories) %>% summarise(avg = mean(`Condominiums Market Value per SqFt`),med = median(`Condominiums Market Value per SqFt`), std= sd(`Condominiums Market Value per SqFt`))
mpsc

barplot(table(YearBuiltCategories),xlab="YearBuilt Categories", ylab="No. of Condominiumns",col=c("Blue","Pink"))
boxplot(`Condominiums Market Value per SqFt`~ YearBuiltCategories,xlab="YearBuilt Categories", ylab="No. of Condominiumns",col=c("green","red"))

price.aov <-aov(`Condominiums Market Value per SqFt`~ as.factor(YearBuiltCategories), data=Real_estate_NY_Project)
summary(price.aov)

tt<-t.test(`Condominiums Market Value per SqFt`[YearBuiltCategories=="Before_2000"],`Condominiums Market Value per SqFt`[YearBuiltCategories=="After_2000"],alternative = "greater")
tt

#6.	Market Price per sq. ft. and Condominiums Total Units
#7.	Market Price per sq. ft. and Condominiums Total Units


plot(`Condominiums Market Value per SqFt`~log(`Condominiums Gross SqFt`), pch=16, col="lightblue")
abline(lm(`Condominiums Market Value per SqFt`~log(`Condominiums Gross SqFt`)), lwd=3)
plot(`Condominiums Market Value per SqFt`~log(`Condominiums Total Units`), pch=16, col="lightblue")
abline(lm(`Condominiums Market Value per SqFt`~log(`Condominiums Total Units`)), lwd=3)

cor(`Condominiums Market Value per SqFt`,log(`Condominiums Gross SqFt`), use="complete.obs", method="pearson")
cor(`Condominiums Market Value per SqFt`,log(`Condominiums Total Units`), use="complete.obs", method="pearson")



#Linear Regression Model

#Model1
mod<-lm(formula = `Condominiums Market Value per SqFt` ~ as.factor(`Condominiums Building Classification`)+ as.factor(`Condominums Boroughs`)+ log(`Condominiums Gross SqFt`)+log(`Condominiums Total Units`), data = out)
summary(mod)

#Model2
mod<-lm(formula = `Condominiums Market Value per SqFt` ~ as.factor(`Condominiums Building Classification`)+ as.factor(`Condominums Boroughs`)+ log(`Condominiums Gross SqFt`)+ log(`Condominiums Total Units`)+YearBuiltCategories, data = out)
summary(mod)

#Model3
mod<-lm(formula = `Condominiums Market Value per SqFt` ~ as.factor(`Condominiums Building Classification`)+ as.factor(`Condominums Boroughs`)+ log(`Condominiums Gross SqFt`)+ log(`Condominiums Total Units`)+as.factor(YearBuiltCategories)+as.factor(Zone), data = out)
summary(mod)

#Model4
mod<-lm(formula = `Condominiums Market Value per SqFt` ~ as.factor(`Condominiums Building Classification`)+ as.factor(`Condominums Boroughs`)+ log(`Condominiums Gross SqFt`)+ log(`Condominiums Total Units`)+YearBuiltCategories+as.factor(Zone)+as.factor(WalkscoreCategories), data = out)
summary(mod)
par(mfrow=c(2,2))
plot(mod)


#============================code Ended========================================================================


       

        
