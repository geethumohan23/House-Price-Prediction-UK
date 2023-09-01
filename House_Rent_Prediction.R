setwd('C:/Users/geeth/Desktop/AppliedStat/')
house_data <- read.csv('house-data.csv')
#install.packages('ggplot2')
#install.packages('GGally')

library('ISLR')
library('tidyverse')
library('ggplot2')
library('corrplot')
library('dplyr')
library(corrplot)
library(mice) 
library("faraway")
library('tidyr')
library('olsrr')

str(house_data)
head(house_data)
summary(house_data)
#ggplot(dataLife,aes(DensityPop,LifeExp))+geom_point(size=1)+geom_line(color="red")
#view(dataLife)

#Attaching  
dim(house_data)
df<-data.frame(house_data)
library(ggplot2)
library(GGally)
ggpairs(df)

pairs(df)


#renaming
dataLife<-data.frame(dataLife)
colnames(dataLife)[1:20]<-c("LifeExp","AccessElectricity","IncomePerCapita","NationalIncome","HIVKids","NoSchool","Infant_Mor","PrimaryComp","Interest","GrowthPop","DensityPop","ExpenseGDP","Health","Unemploy","GDPGrowth","GDPPerCapita","BirthRate","HIVAdult","DrinkWater","Education")
view(dataLife)


#Imputation Starts
data_Impute<-mice(dataLife, method="cart",seed=2318)
complete(data_Impute) #imputated variable
data_Impute$imp

#analysis of Imputed Value with the filtered dataset
Model_Imputed<- with(data_Impute, lm(LifeExp~AccessElectricity+IncomePerCapita+NationalIncome+HIVKids+NoSchool+Infant_Mor+PrimaryComp+Interest+GrowthPop+ExpenseGDP+Health+Unemploy+GDPGrowth+GDPPerCapita+BirthRate+HIVAdult+DrinkWater+Education))
summary(Model_Imputed)


#ploting with imputed values
xyplot(data_Impute,LifeExp  ~ NationalIncome| .imp, pch = 20, cex = 1.4)
xyplot(data_Impute,LifeExp  ~ Infant_Mor| .imp, pch = 20, cex = 1.4)
#md.pattern(Model_Imputed)

#pooling
pooled.model<-pool(Model_Imputed)
summary(pooled.model) #completed data imputation with data in pooled.model



data_impute_complete<-complete(data_Impute,2)
#collinearity plot
collinear_model<-lm(LifeExp~.,data=data_impute_complete)
summary(collinear_model)
summary(data_impute_complete)
Data_relate<-cor(data_impute_complete)
summary(Data_relate)
corrplot(Data_relate,tl.pos ='lt', tl.cex=0.55,method='circle')

#testing multicollinearity
ols_vif_tol(collinear_model)
evif(collinear_model)
ols_plot_resid_fit_spread(collinear_model)

#Forward and Backward Selection
data_impute_complete<- data_impute_complete[,c(-17)]
view(data_impute_complete)
dt<-data.frame(data_impute_complete)


#write.csv(dt,"C:/Users/geeth/Desktop/Modelling_Coursework/final_data.csv", row.names=FALSE)
BestModelBuild<-lm(LifeExp~., data=data_impute_complete) #linear regression for the model()
summary(BestModelBuild) #Read details which all fields needs to be rejected
data_impute_complete<- data_impute_complete[,c(-3,-4,-5,-6,-7,-8,-10,-11,-12,-20)]
data_impute_complete<- data_impute_complete[,c(-5,-8)]
view(data_impute_complete)
ModelForward<-lm(LifeExp~1,data=data_impute_complete)
step1<-step(BestModelBuild,ModelForward=~ PrimaryComp+Health+Unemploy+GDPGrowth, method='forward')
AIC(step1)

#backward Elimination
forward_list<-step(BestModelBuild,direction="forward")
AIC(forward_list)

#Backward feature selection
ModelBackward<-lm(LifeExp~.,data=data_impute_complete)
step2<-step(BestModelBuild,ModelBackward=~.,data=data_impute_complete)
AIC(step2)


backward_list<-step(BestModelBuild,direction="backward")
AIC(backward_list)

ModelFit_Backward<-lm(LifeExp ~ AccessElectricity + PrimaryComp + Interest + DensityPop + 
                        Health + GDPGrowth + HIVAdult + DrinkWater, data=data_impute_complete)
AIC(ModelFit_Backward)
summary(ModelFit_Backward)

ModelFit_Backward<-lm(LifeExp ~ AccessElectricity + HIVKids + NoSchool + PrimaryComp + 
                        Interest + DensityPop + Health + Unemploy + GDPGrowth + HIVAdult + 
                        DrinkWater + Education, data=data_impute_complete)
AIC(ModelFit_Backward)
summary(ModelFit_Backward)
summary(step1)
Final_linear_model<-lm(LifeExp ~ NoSchool+HIVAdult+Interest+Unemploy + GDPGrowth +Health+PrimaryComp)
