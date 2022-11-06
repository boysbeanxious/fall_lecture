##################################################
#### KAIST IMMBA Advanced Business Analytics ####
##################################################

# Multi-level modeling for marketing analytics

# remove all variables and dataset
rm(list=ls(all=TRUE))

# install and load required packages
install.packages("arm")
install.packages("lme4")

library(MASS)
library(Matrix)
library(lme4)
library(arm)


##### RADON STUDY #####
# load and attach dataset

radon.data <- read.table("radon.txt",header=TRUE)
attach(radon.data)
head(radon.data)

# No-pooling and complete-pooling models

complete.pooling <- lm(radon ~ floor)
display(complete.pooling)

no.pooling <- lm(radon ~ floor + factor(county)-1)
display(no.pooling)

# Varying Intercepts, No Predictor
M0 <- lmer(radon ~ 1 + (1 | county))
display(M0)

# Varying Intercepts, Floor Predictor
M1 <- lmer(radon ~ floor + (1 | county))

# Summarizing and Displaying the Fitted Model
display(M1)
summary(M1)
coef(M1)
fixef(M1)
ranef(M1)
se.fixef(M1)
se.ranef(M1)

fixef(M1)["floor"] + c(-2,2)*se.fixef(M1)["floor"]
coef(M1)$county[26,1] + c(-2,2)* se.ranef(M1)$county[26] 

# Varying Slopes, Fixed Intercept
M2 <- lmer(radon ~ floor + (floor - 1 | county))
display(M2)
coef(M2)

# Varying Slopes, Varying Intercepts
M3 <- lmer(radon ~ floor + (1 + floor | county) )
display(M3)
coef(M3)

# Varying Slopes for Two Variables
complete.pooling2 <- lm(radon ~ floor + uranium)
summary(complete.pooling2)

M4 <- lmer(radon ~ floor  + uranium + ( floor + uranium - 1 | county) )
display(M4)
coef(M4)

complete.pooling2 <- lm(radon ~ floor + uranium)
summary(complete.pooling2)

##### CHEESE STUDY #####
# load and attach dataset
cheese <- read.csv("cheese.csv", header = T)
attach(cheese)

# converting char to num
is.numeric(Retailer)
retailer_code <- as.numeric(factor(Retailer))

is.numeric(City)
city_code <- as.numeric(factor(City))

# transforming to logarithm term
hist(VOLUME)
lvolume <- log(VOLUME)
hist(lvolume)

hist(PRICE)
lprice <- log(PRICE)
hist(lprice)

hist(DISP)
ldisp <- log(DISP)
hist(ldisp)


# No-pooling and complete-pooling models

complete.pooling1 <- lm(lvolume ~ lprice)
display(complete.pooling1)

complete.pooling2 <- lm(lvolume ~ lprice + DISP)
display(complete.pooling2)

no.pooling1 <- lm(lvolume ~ lprice + factor(retailer_code)-1)
display(no.pooling1)

no.pooling2 <- lm(lvolume ~ lprice + DISP + factor(retailer_code)-1)
display(no.pooling2)

# TWO-LEVEL SETTING at retailer

# Varying Slopes for Two Variables
M5 <- lmer(lvolume ~ lprice + DISP + ( lprice + DISP - 1 | retailer_code) )
display(M5)
coef(M5)

# Varying Slopes for Two Variables and Varying Intercept
M6 <- lmer(lvolume ~ lprice + DISP + ( lprice + DISP | retailer_code) )
display(M6)
coef(M6)

# TWO-LEVEL SETTING at City
# Varying Slopes for Two Variables and Varying Intercept
M7 <- lmer(lvolume ~ lprice + DISP + ( lprice + DISP | city_code) )
display(M7)
coef(M7)

# THREE-LEVEL SETTING at Retailer and City
# Varying Slopes for Two Variables and Varying Intercept
M8 <- lmer(lvolume ~ lprice + DISP + ( lprice + DISP | city_code:retailer_code) )
display(M8)
coef(M8)

