data = read.csv("C:/Users/asd/Desktop/R/Macromodel/macroeconomicmodel_Indiadata.csv")
View(data)
attach(data)
library('ggplot2')
library('forecast')
library('tseries')
library(car)
library(MASS)

model = lm(CIT_REVENUE_Growth_Rate~EXCHANGE_RATE+BOP_INFLOWS_Growth_Rate+BOP_OUTFLOWS_Growth_Rate+EXCHANGE_RATE+BOP_INFLOWS_Growth_Rate+BOP_OUTFLOWS_Growth_Rate+LENDING_RATE+CIT_RATE+GDP_PERCAPITA_Growth_Rate+INFLATION_RATE+URBAN_POPULATION_GROWTH_RATE)
summary = summary(model)
summary 

# check for multi collinearity
VIF = vif(model)
VIF
dataq = data[,3:10]
View(dataq)
cor(dataq)
modelq = lm(CIT_REVENUE_Growth_Rate~EXCHANGE_RATE+BOP_INFLOWS_Growth_Rate+BOP_OUTFLOWS_Growth_Rate+LENDING_RATE+CIT_RATE+GDP_PERCAPITA_Growth_Rate+INFLATION_RATE+URBAN_POPULATION_GROWTH_RATE)
stepmodel= stepAIC(modelq, direction = "both")


# going ahead without changes
pred = fitted(model)
res = residuals(model)
table = cbind(CIT_REVENUE_Growth_Rate,pred,res)
table 
anova(model)


# Fit and forecast EXCHANGE_RATE with Arima()
autoArimaFitEx <- auto.arima(data$EXCHANGE_RATE)
plot(forecast(autoArimaFitEx, h=20))
arimaFitEx <- Arima(data$EXCHANGE_RATE,order=c(3,1,0))
plot(forecast(arimaFitEx,h=40))
forecastsEx = forecast(data$EXCHANGE_RATE, h=2)
forecastsEx 

# Fit and forecast BOP_INFLOWS_Growth_Rate with Arima()
autoArimaFitBI <- auto.arima(data$BOP_INFLOWS_Growth_Rate)
plot(forecast(autoArimaFitBI, h=20))
arimaFitBI <- Arima(data$BOP_INFLOWS_Growth_Rate,order=c(3,1,0))
plot(forecast(arimaFitBI,h=40))
forecastsBI = forecast(data$BOP_INFLOWS_Growth_Rate, h=2)
forecastsBI

# Fit and forecast BOP_OUTFLOWS_Growth_Rate with Arima()
autoArimaFitBO <- auto.arima(data$BOP_OUTFLOWS_Growth_Rate)
plot(forecast(autoArimaFitBO, h=20))
arimaFitBO <- Arima(data$BOP_OUTFLOWS_Growth_Rate,order=c(3,1,0))
plot(forecast(arimaFitBO,h=40))
forecastsBO = forecast(data$BOP_OUTFLOWS_Growth_Rate, h=2)
forecastsBO

# Fit and forecast LENDING_RATE with Arima()
autoArimaFitLr <- auto.arima(data$LENDING_RATE)
plot(forecast(autoArimaFitLr, h=20))
arimaFitLr <- Arima(data$LENDING_RATE,order=c(3,1,0))
plot(forecast(arimaFitLr,h=40))
forecastsLr = forecast(data$LENDING_RATE, h=2)
forecastsLr

# Fit and forecast CIT_RATE with Arima()
autoArimaFitCr <- auto.arima(data$CIT_RATE)
plot(forecast(autoArimaFitCr, h=20))
arimaFitCr <- Arima(data$CIT_RATE,order=c(3,1,0))
plot(forecast(arimaFitCr,h=40))
forecastsCr = forecast(data$CIT_RATE, h=2)
forecastsCr

# Fit and forecast GDP_PERCAPITA_Growth_Rate with Arima()
autoArimaFitGp <- auto.arima(data$GDP_PERCAPITA_Growth_Rate)
plot(forecast(autoArimaFitGp, h=20))
arimaFitGp <- Arima(data$GDP_PERCAPITA_Growth_Rate,order=c(3,1,0))
plot(forecast(arimaFitGp,h=40))
forecastsGp = forecast(data$GDP_PERCAPITA_Growth_Rate, h=2)
forecastsGp

# Fit and forecast INFLATION_RATE with Arima()
autoArimaFitIr <- auto.arima(data$INFLATION_RATE)
plot(forecast(autoArimaFitIr, h=20))
arimaFitIr <- Arima(data$INFLATION_RATE,order=c(3,1,0))
plot(forecast(arimaFitIr,h=40))
forecastsIr = forecast(data$INFLATION_RATE, h=2)
forecastsIr

# Fit and forecast URBAN_POPULATION_GROWTH_RATE with Arima()
autoArimaFitUp <- auto.arima(data$URBAN_POPULATION_GROWTH_RATE)
plot(forecast(autoArimaFitUp, h=20))
arimaFitUp <- Arima(data$URBAN_POPULATION_GROWTH_RATE,order=c(3,1,0))
plot(forecast(arimaFitUp,h=40))
forecastsUp = forecast(data$URBAN_POPULATION_GROWTH_RATE, h=2)
forecastsUp




# Forecasting model


data1 = read.csv("C:/Users/asd/Desktop/R/Macromodel/macroeconomicmodel_Indiadata1.csv")
View(data1)
attach(data1)
model1 = lm(CIT_REVENUE_Growth_Rate~EXCHANGE_RATE+BOP_INFLOWS_Growth_Rate+BOP_OUTFLOWS_Growth_Rate+LENDING_RATE+CIT_RATE+GDP_PERCAPITA_Growth_Rate+INFLATION_RATE+URBAN_POPULATION_GROWTH_RATE)
summary1 = summary(model)
summary1 
pred1 = fitted(model1)
res1 = residuals(model1)
table1 = cbind(CIT_REVENUE_Growth_Rate,pred1,res1)
table1 
anova(model1)








