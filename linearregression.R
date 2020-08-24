# Linear Regression in R
# Copyright 2013 by Ani Katchova

mydata<- read.csv("D:/R/Linear Regression/regression_auto.csv")
attach(mydata)

# Define variables
Y <- cbind(mpg)
X1 <- cbind(weight1)
X <- cbind(weight1, price, foreign)

# Descriptive statistics
summary(Y)
summary(X)

# Correlation among variables
cor(Y, X)

# Plotting data on a scatter diagram
plot(Y ~ X1, data = mydata)

# Simple linear regression 
olsreg1 <- lm(Y ~ X1)
summary(olsreg1)
confint(olsreg1, level=0.95)
anova(olsreg1)

# Plotting regression line
abline(olsreg1)
plot(olsreg1)
# Predicted values for dependent variable
Y1hat <- fitted(olsreg1)
summary(Y1hat)
plot(Y1hat ~ X1)

# Regression residuals
e1hat <- resid(olsreg1)
summary(e1hat)
plot(e1hat ~ X1)

# Multiple linear regression
olsreg2 <- lm(Y ~ X)
summary(olsreg2)
confint(olsreg2, level=0.95)
anova(olsreg2)

# Predicted values for dependent variable
Yhat <- fitted(olsreg2)
summary(Yhat)

# Regression residuals
ehat <- resid(olsreg2)
summary(ehat)
plot(olsreg2)

#Homoscedasticity
par(mfrow=c(2,2)) # set 2 rows and 2 column plot layou
plot(olsreg2)
#qqnormal untuk cek normalitas, diagonal = normal

#Autocorellation
# Method 1: Visualise with acf plot
library(ggplot2)
par(mfrow=c(1,1))
acf(olsreg2$residuals) #Result - No Autocol

# Method 2: Runs test to test for randomness
lawstat::runs.test(olsreg2$residuals)

# Method 3: Durbin-Watson test
lmtest::dwtest(olsreg2)

#How to rectify? - Add lag1 of residual as an X variable to the original model. This can be conveniently done using the slide function in DataCombine package.
library(DataCombine)
econ_data <- data.frame(economics, resid_mod1=lmMod$residuals)
econ_data_1 <- slide(econ_data, Var="resid_mod1", NewVar = "lag1", slideBy = -1)
econ_data_2 <- na.omit(econ_data_1)
lmMod2 <- lm(pce ~ pop + lag1, data=econ_data_2)

#No Perfect Multicolinearity
library(car)
vif(lm(Y ~ weight1 + price + foreign))

#Cara Perbaikan
#Buang X dengan VIF > 4 atau
#Buang X Korelasi Tinggi
library(corrplot)
corrplot(cor(mydata[, -1]))

#Check Assumptions Automatically
gvlma::gvlma(olsreg2)
#Pada distribusi normal, nilai skewness adalah 0 dan kurtosis 3.

par(mfrow=c(2,2))
plot(olsreg2)
olsreg3 <- lm(Y~X, data=mydata[-c(4, 16, 15, 24), ])
gvlma::gvlma(olsreg3)

