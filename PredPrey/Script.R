# Script: Script.R
# Author: Joshua L. Eubanks
# Date: 10 Aug 2018

# This script generates test data then implements the test data into the equations outlined in Models.pdf

library('forecast')
options(scipen = 999)
set.seed(123457)

Date <- seq(as.Date("2017-01-01"), as.Date("2017-12-31"), by="days")
N <- length(Date)

Minnows <- rnorm(N, 10000, 500)
sDolphin <- rnorm(N, 7500, 500)
Dolphin <- rnorm(N, 5000, 500)
sWhale <- rnorm(N, 2500, 500)
Whale <- rnorm(N, 2000, 500)

Pop <- data.frame(Date, Minnows , sDolphin, Dolphin, sWhale, Whale)
par(mfrow=c(2,1))
# Regression equations for minnows
MinnowMod <- lm(diff(log(Pop[,2])) ~ Pop[-1, 2] + Pop[-1,3] + Pop[-1,4] + Pop[-1,5] + Pop[-1,6] )
summary(MinnowMod)
Acf(MinnowMod$residuals)
Pacf(MinnowMod$residuals)


# Regression equation for small dolphins
sDolphinMod <- lm(diff(log(Pop[,3])) ~ Pop[-1, 3] + Pop[-1,2] + Pop[-1,4] + Pop[-1,5] + Pop[-1,6] )
summary(sDolphinMod)
Acf(sDolphinMod$residuals)
Pacf(sDolphinMod$residuals)

# Regression equation for dolphins
DolphinMod <- lm(diff(log(Pop[,4])) ~ Pop[-1, 4] + Pop[-1,2] + Pop[-1,3] + Pop[-1,5] + Pop[-1,6] )
summary(DolphinMod)
Acf(DolphinMod$residuals)
Pacf(DolphinMod$residuals)

# Regression equation for small whales
sWhaleMod <- lm(diff(log(Pop[,5])) ~ Pop[-1, 5] + Pop[-1,2] + Pop[-1,3] + Pop[-1,4] + Pop[-1,6] )
summary(sWhaleMod)
Acf(sWhaleMod$residuals)
Pacf(sWhaleMod$residuals)


# Regression equation for whales
WhaleMod <- lm(diff(log(Pop[,6])) ~ Pop[-1, 6] + Pop[-1,2] + Pop[-1,3] + Pop[-1,4] + Pop[-1,5] )
summary(WhaleMod)
Acf(WhaleMod$residuals)
Pacf(WhaleMod$residuals)