data = read.csv("C:/Users/asd/Desktop/R/Macromodel/macroeconomicmodel_Indiadata.csv")
View(data)
attach(data)
library('ggplot2')
library('forecast') 
library('tseries')
library(car)
library(MASS)

model = lm(CIT_REVENUE_Growth_Rate~EXCHANGE_RATE+BOP_INFLOWS_Growth_Rate+BOP_OUTFLOWS_Growth_Rate+LENDING_RATE+L_LENDING_RATE+CIT_RATE+GDP_PERCAPITA_Growth_Rate+INFLATION_RATE+URBAN_POPULATION_GROWTH_RATE)
summary = summary(model)
summary 

# check residuals
res = residuals(model)
sum = sum(res**2)
sum 
avg = sum/26


# check for multi collinearity
VIF = vif(model)
VIF
dataq = data[,3:10]
View(dataq)
cor(dataq)
modelq = lm(CIT_REVENUE_Growth_Rate~EXCHANGE_RATE+BOP_INFLOWS_Growth_Rate+BOP_OUTFLOWS_Growth_Rate+LENDING_RATE+L_LENDING_RATE+CIT_RATE+GDP_PERCAPITA_Growth_Rate+INFLATION_RATE+URBAN_POPULATION_GROWTH_RATE)
stepmodel= stepAIC(modelq, direction = "both")
pred = fitted(model)
res = residuals(model)
table = cbind(CIT_REVENUE_Growth_Rate,pred,res)
table

# going ahead with changes1

model1 = lm(CIT_REVENUE_Growth_Rate~GDP_PERCAPITA_Growth_Rate+URBAN_POPULATION_GROWTH_RATE)
summary1 = summary(model1)
summary1

pred1 = fitted(model1)
res1 = residuals(model1)
table1 = cbind(CIT_REVENUE_Growth_Rate,pred1,res1)
table1   

# going ahead with changes2

model2 = lm(CIT_REVENUE_Growth_Rate~GDP_PERCAPITA_Growth_Rate+INFLATION_RATE+URBAN_POPULATION_GROWTH_RATE)
summary2 = summary(model2)
summary2

pred2 = fitted(model2)
res2 = residuals(model2)
table2 = cbind(CIT_REVENUE_Growth_Rate,pred2,res2)
table2   

# going ahead with changes3

model3 = lm(CIT_REVENUE_Growth_Rate~GDP_PERCAPITA_Growth_Rate+INFLATION_RATE)
summary3 = summary(model3)
summary3

pred3 = fitted(model3)
res3 = residuals(model3)
table3 = cbind(CIT_REVENUE_Growth_Rate,pred3,res3)
table3

# going ahead with changes4

model4 = lm(CIT_REVENUE_Growth_Rate~INFLATION_RATE+URBAN_POPULATION_GROWTH_RATE)
summary4 = summary(model4)
summary4

pred4 = fitted(model4)
res4 = residuals(model4)
table4 = cbind(CIT_REVENUE_Growth_Rate,pred4,res4)
table4   

# going ahead with changes5.

model5 = lm(CIT_REVENUE_Growth_Rate~L_LENDING_RATE+GDP_PERCAPITA_Growth_Rate+URBAN_POPULATION_GROWTH_RATE)
summary5 = summary(model5)
summary5

pred5 = fitted(model5)
res5 = residuals(model5)
table5 = cbind(CIT_REVENUE_Growth_Rate,pred5,res5)
table5   

# going ahead with changes6

model6 = lm(CIT_REVENUE_Growth_Rate~L_LENDING_RATE+GDP_PERCAPITA_Growth_Rate+INFLATION_RATE+URBAN_POPULATION_GROWTH_RATE)
summary6 = summary(model6)
summary6

pred6 = fitted(model6)
res6 = residuals(model6)
table6 = cbind(CIT_REVENUE_Growth_Rate,pred6,res6)
table6   

# going ahead with changes7

model7 = lm(CIT_REVENUE_Growth_Rate~L_LENDING_RATE+INFLATION_RATE+URBAN_POPULATION_GROWTH_RATE)
summary7 = summary(model7)
summary7

pred7 = fitted(model7)
res7 = residuals(model7)
table7 = cbind(CIT_REVENUE_Growth_Rate,pred7,res7)
table7

# going ahead with changes8

model8 = lm(CIT_REVENUE_Growth_Rate~L_LENDING_RATE+GDP_PERCAPITA_Growth_Rate+INFLATION_RATE)
summary8 = summary(model8)
summary8

pred8 = fitted(model8)
res8 = residuals(model8)
table8 = cbind(CIT_REVENUE_Growth_Rate,pred8,res8)
table8   

# going ahead with changes9

model9 = lm(CIT_REVENUE_Growth_Rate~L_LENDING_RATE+GDP_PERCAPITA_Growth_Rate)
summary9 = summary(model9)
summary9

pred9 = fitted(model9)
res9 = residuals(model9)
table9 = cbind(CIT_REVENUE_Growth_Rate,pred9,res9)
table9   

# going ahead with changes10

model10 = lm(CIT_REVENUE_Growth_Rate~L_LENDING_RATE+INFLATION_RATE)
summary10 = summary(model10)
summary10

pred10 = fitted(model10)
res10 = residuals(model10)
table10 = cbind(CIT_REVENUE_Growth_Rate,pred10,res10)
table10   

# going ahead with changes11

model11 = lm(CIT_REVENUE_Growth_Rate~L_LENDING_RATE+URBAN_POPULATION_GROWTH_RATE)
summary11 = summary(model11)
summary11

pred11 = fitted(model11)
res11 = residuals(model11)
table11 = cbind(CIT_REVENUE_Growth_Rate,pred11,res11)
table11

# going ahead with changes12

model12 = lm(CIT_REVENUE_Growth_Rate~EXCHANGE_RATE+BOP_INFLOWS_Growth_Rate+BOP_OUTFLOWS_Growth_Rate+LENDING_RATE+L_LENDING_RATE+GDP_PERCAPITA_Growth_Rate+INFLATION_RATE+URBAN_POPULATION_GROWTH_RATE)
summary12 = summary(model12)
summary12

pred12 = fitted(model12)
res12 = residuals(model12)
table12 = cbind(CIT_REVENUE_Growth_Rate,pred12,res12)
table12   

# going ahead with changes13

model13 = lm(CIT_REVENUE_Growth_Rate~EXCHANGE_RATE+BOP_INFLOWS_Growth_Rate+BOP_OUTFLOWS_Growth_Rate+CIT_RATE+GDP_PERCAPITA_Growth_Rate+INFLATION_RATE+URBAN_POPULATION_GROWTH_RATE)
summary13 = summary(model13)
summary13

pred13 = fitted(model13)
res13 = residuals(model13)
table13 = cbind(CIT_REVENUE_Growth_Rate,pred13,res13)
table13 

# going ahead with changes14

model14 = lm(CIT_REVENUE_Growth_Rate~EXCHANGE_RATE+BOP_INFLOWS_Growth_Rate+BOP_OUTFLOWS_Growth_Rate+CIT_RATE+GDP_PERCAPITA_Growth_Rate+INFLATION_RATE+URBAN_POPULATION_GROWTH_RATE)
summary14 = summary(model14)
summary14

pred14 = fitted(model14)
res14 = residuals(model14)
table14 = cbind(CIT_REVENUE_Growth_Rate,pred14,res14)
table14   

# going ahead with changes15

model15 = lm(CIT_REVENUE_Growth_Rate~EXCHANGE_RATE+LENDING_RATE+L_LENDING_RATE+CIT_RATE+GDP_PERCAPITA_Growth_Rate+INFLATION_RATE+URBAN_POPULATION_GROWTH_RATE)
summary15 = summary(model15)
summary15

pred15 = fitted(model15)
res15 = residuals(model15)
table15 = cbind(CIT_REVENUE_Growth_Rate,pred15,res15)
table15

# going ahead with changes16

model16 = lm(CIT_REVENUE_Growth_Rate~BOP_INFLOWS_Growth_Rate+BOP_OUTFLOWS_Growth_Rate+LENDING_RATE+L_LENDING_RATE+CIT_RATE+GDP_PERCAPITA_Growth_Rate+INFLATION_RATE+URBAN_POPULATION_GROWTH_RATE)
summary16 = summary(model16)
summary16

pred16 = fitted(model16)
res16 = residuals(model16)
table16 = cbind(CIT_REVENUE_Growth_Rate,pred1,res1)
table16   

# check residuals of changed models
sum1 = sum(res1**2)
sum1 
avg1 = sum1/26

sum2 = sum(res2**2)
sum2 
avg2 = sum2/26

sum3 = sum(res3**2)
sum3 
avg3 = sum3/26

sum4 = sum(res4**2)
sum4 
avg4 = sum4/26

sum5 = sum(res5**2)
sum5 
avg5 = sum5/26

sum6 = sum(res6**2)
sum6 
avg6 = sum6/26

sum7 = sum(res7**2)
sum7 
avg7 = sum7/26

sum8 = sum(res8**2)
sum8 
avg8 = sum8/26

sum9 = sum(res9**2)
sum9 
avg9 = sum9/26

sum10 = sum(res10**2)
sum10 
avg10 = sum10/26

sum11 = sum(res11**2)
sum11 
avg11 = sum11/26

sum12 = sum(res12**2)
sum12 
avg12 = sum12/26

sum13 = sum(res13**2)
sum13 
avg13 = sum13/26

sum14 = sum(res14**2)
sum14 
avg14 = sum14/26

sum15 = sum(res15**2)
sum15 
avg15 = sum15/26

sum16 = sum(res16**2)
sum16 
avg16 = sum16/26


avg
avg1
avg2
avg3
avg4
avg5
avg6
avg7
avg8
avg9
avg10
avg11
avg12
avg13
avg14
avg15
avg16






