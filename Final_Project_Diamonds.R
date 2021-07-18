#Project 1: Diamonds
#Stat 6021
#Summer 2021
#members: Alice Bogdan, Melanie Hazlett, June Suh
#Final R script

#necessary libraries
library(MASS) 
library(lawstat) #for levene.test()
library(multcomp) #for pairise, glht
library(tidyverse)
library(ggplot2) #EDA
library(dplyr)
library(gridExtra)
library(faraway)

#load diamonds dataset
data2 <- read.csv("diamonds4.csv", header = TRUE, sep = ",")
attach(data2)

#easy attach and detach calls
#entire diamonds dataset
#attach(data2)
#detach(data2)
#attach(data_30KL)
#detach(data_30KL)

#general stats of dataset for EDA
min(price)
max(price)
mean(price)
median(price)
median(carat)
#mode(color)

#look at subsets of data to understand the effect potential outliers have on the average diamond price
#diamonds that cost more than $30K
data_30K <- data2[which(price >= 30000),]
stat_30K <- nrow(data_30K)/nrow(data2)
stat_30K #4% of diamonds

#diamonds that cost less than $30K
data_30KL <- data2[which(price < 30000),]
#average diamond price for diamonds that cost less than $30K
mean(data_30KL$price)

#diamonds that cost more than $50K
data_50K <- data2[which(price >= 50000),]
stat_50K <- nrow(data_50K)/nrow(data2)
stat_50K #2% of diamonds

#diamonds that cost more than $100K
data_100K <- data2[which(price >= 100000),]
stat_100K <- nrow(data_100K)/nrow(data2)
stat_100K #1% of diamonds

#EDA Visuals for 4C's
#bar plot cut
ggplot(data = data2) + geom_bar(mapping = aes(x = cut))+ggtitle("Figure 1: Bar Plot of Diamond Cut")
#bar plot color
ggplot(data = data2) + geom_bar(mapping = aes(x = color))+ggtitle("Figure 2: Bar Plot of Diamond Color")
#bar plot clarity
ggplot(data = data2) + geom_bar(mapping = aes(x = clarity))+ggtitle("Figure 3: Bar Plot of Diamond Clarity")
#histogram carat
ggplot(data = data2)+geom_histogram(mapping = aes(x=carat), binwidth = 0.5)+ggtitle("Figure 4: Histogram of Diamond Carat")
#histogram price
#ggplot(data = data_30KL)+geom_histogram(mapping = aes(x=price), binwidth = 1000)+ggtitle("Figure 5: Histogram of Diamond Price")

#are the categorical predictors numerical?
is.numeric(cut)
is.numeric(color)
is.numeric(clarity)
#all three are False

#have R treat cut, color, and clarity as categorical variables
data2$cut <- factor(data2$cut)
data2$color <- factor(data2$color)
data2$clarity <- factor(data2$clarity)
contrasts(data2$cut) #reference variable Astor Ideal
contrasts(data2$color) #reference variable D
contrasts(data2$clarity) #reference variable FL

#assign variables for 4C's and price to avoid R code errors
cut1 <- data2$cut
color1 <- data2$color
clarity1 <- data2$clarity
carat1 <- data2$carat
price1 <- data2$price

#MODEL BUILDING PROCESS: THE FULL MODEL (Start)
#fit regression model. First-order. No collapsed variables (Model 1a)
result_1a <- lm(price1~cut1+color1+carat1+clarity1)
summary(result_1a) #Model 1a
sum(anova(result_1a)[2]) #SST= 705,838,000,000

#check multicollinearity. Some high values especially between clarity and cut (Appendix Fig A.3)
#vif(result_1a)

#test regression assumptions (first order, no collapsed variables)
#residual plot
plot(result_1a$fitted.values, result_1a$residuals,xlab = "Fitted Values", ylab = "Residuals", main = "Residual Plot (Model 1a)")
abline(h=0, col = "blue")
#doesn't look good. Look at boxcox plot to assess constant variance assumption

#ACF plot
#acf(result_1a$residuals, main ="ACF Plot (Model 1a)")

##QQ plot of residuals
#qqnorm(result_1a$residuals)
#qqline(result_1a$residuals)

#boxcox plot to assess constant variance assumption
boxcox(result_1a, lambda = seq(0.3,0.4,0.01)) #lambda 1 not included in interval. Indicates transformation on response is required

#transformation on response variable
price_trans <- price^(1/3)

#add transformed price to data2
data2$price_trans <- price_trans

#fit regression model. First-order. No collapsed variables, transformation on response variable (Model 1b)
result_1b <- lm(data2$price_trans~cut1+color1+carat1+clarity1)

#other transformations considered but not included
#price_log <- log(price) #doesn't help
#data2$price_log <- price_log #doesn't help
#result_F_log <- lm(data2$price_log~cut1+color1+carat1+clarity1) #doesn't help
#boxcox(result_F_log, lambda = seq(2,5,0.1)) #doesn't help

#summary stats
summary(result_1b) # Appendix Fig A.1 

#check boxcox again
boxcox(result_1b, lambda = seq(0.9,1.1,0.1)) #lambda 1 included in interval

#residual plot (Model 1b)
plot(result_1b$fitted.values, result_1b$residuals,xlab = "Fitted Values", ylab = "Residuals", main = "Residual Plot (Model 1b)")
abline(h=0, col = "blue")

#ACF plot (transformed response, no collapse)
acf(result_1b$residuals, main ="ACF Plot (Model 1b)")

##QQ plot of residuals (Model 1b)
qqnorm(result_1b$residuals)
qqline(result_1b$residuals)
#regression assumptions look decent with cubic root transformation
#MODEL BUILDING PROCESS: THE FULL MODEL (End)

#Discussed in Unused Analyses Section
#redefine and recalculate
#CI for mean response. Considering most common aspects listed on BlueNile website and individual experience
#newdata_F_trans <- data.frame(cut1 = "Ideal", color1 = "F", carat1 = 0.5, clarity1 = "SI1")
#predict(result_F_trans, newdata_F_trans, level =0.95, interval = "confidence")

#lower bound:10.6463
#predict(result_F_trans, newdata_F_trans, level =0.95, interval = "confidence")[2]
#upper bound: 11.30773
#predict(result_F_trans, newdata_F_trans, level =0.95, interval = "confidence")[3]
#fit:10.97701
#predict(result_F_trans, newdata_F_trans, level =0.95, interval = "confidence")[1]

#PI for diamond with average characteristics
#predict(result_F_trans, newdata_F_trans, level =0.95, interval = "prediction")

#fit: 10.97701
#predict(result_F_trans, newdata_F_trans, level =0.95, interval = "prediction")[1]
#lower: 7.376
#predict(result_F_trans, newdata_F_trans, level =0.95, interval = "prediction")[2]
#upper: 14.57725
#predict(result_F_trans, newdata_F_trans, level =0.95, interval = "prediction")[3]

#do the CI and PI make sense?
#subset data and find basic statistics of diamonds that fall in the chosen "average categories"
#data_average_F_trans <- data2[which(cut1 == "Ideal" & color1 == "F" & carat1 == 0.5 & clarity1 == "SI1"),]
#mean(data_average_F_trans$price_trans) #average is 11.21
#although not easily interpretable, 11.21 falls into both the CI and PI intervals. 
#BUT not helpful because only 2/1214 obs have all four characteristics. 
#The model and characteristics are too specific given all the possible classes
#mean(price_trans) 

#check for 1 carat. No observations fall under these characteristics
#newdata_F_trans1 <- data.frame(cut1 = "Ideal", color1 = "F", carat1 = 1, clarity1 = "SI1")
#predict(result_F_trans, newdata_F_trans1, level =0.95, interval = "confidence")
#predict(result_F_trans, newdata_F_trans1, level =0.95, interval = "prediction")
#data_average_F_trans1 <- data2[which(cut1 == "Ideal" & color1 == "F" & carat1 == 1 & clarity1 == "SI1"),]
#mean(data_average_F_trans1$price_trans) #average is 11.21

#Therefore, we would like to try and fit a simpler model by decreasing the number of options

#MODEL BUILDING PROCESS: A SIMPLIFIED MODEL (Start)
#collapse variables
#collapse color to be colorless and near colorless
contrasts(color) #reference class is D
levels(color)

#match with levels(color) order
new.color <- c("Colorless","Colorless","Colorless","Near-colorless","Near-colorless","Near-colorless","Near-colorless") 

#add new binary variable to dataframe
data2$color2 <- factor(new.color[color])
attach(data2)
contrasts(color2) #Colorless is the reference class

#collapse clarity 
contrasts(clarity) #reference class is FL
levels(clarity)

#match with levels(clarity) order
new.clarity <- c("Undetectable","Undetectable","Slightly detectable","Slightly detectable","Slightly detectable","Slightly detectable","Undetectable","Undetectable")

#Add new variable to dataframe
data2$clarity2 <- factor(new.clarity[clarity])
attach(data2)
contrasts(clarity2) 

#relevel so reference class for clarity2 is Undetectable
clarity2 <- relevel(clarity2, ref = "Undetectable")
contrasts(clarity2)

#Part of Unused Analyses
#refit regression model with collapsed color and clarity
#result2 <- lm(price~cut+color2+carat+clarity2)
#summary(result2)
#sum(anova(result2)[2])

#collapse cut
contrasts(cut)
levels(cut) #Astor Ideal / Ideal = Ideal, Very Good / Good = Good
new.cut <- c("Ideal","Good","Ideal","Good")
data2$cut2 <- factor(new.cut[cut])

#relevel so reference class for cut2 is Good
cut2 <- relevel(cut2, ref = "Ideal")
attach(data2)
contrasts(cut2)

#reattach data
attach(data2)

#refit regression equation with collapsed variables for cut, color, and clarity (Model 2a)
result_2a <- lm(price_trans~cut2+color2+carat+clarity2)
summary(result_2a)
sum(anova(result_2a)[2])

#Discussed in Unused Analyses section
#checking multicollinearity. None as expected. 
#vif(result_2a) #Appendix Fig A.4

#check linear assumptions
boxcox(result_2a, lambda = seq(0.5,1.5,0.01)) #lambda 1 still included

#check regression assumptions for fitted model with transformed response variable
#residual plot
plot(result_2a$fitted.values, result_2a$residuals, xlab = "Fitted Values", ylab = "Residuals", main = "Residual Plot (Model 2a)")
abline(h=0, col = "blue")

#ACF plot
acf(result_2a$residuals, main ="ACF Plot (Model 2a)")

##QQ plot of residuals
qqnorm(result_2a$residuals)
qqline(result_2a$residuals)

#can we drop any 4C? Ex: BlueLink claims clarity is least important 4C
result_clar <- lm(price_trans~cut2+color2+carat)
summary(result_clar)
sum(anova(result_clar)[2])
#MODEL BUILDING PROCESS: A SIMPLIFIED MODEL (End)

#partial F test to determine if reduced model without clarity can be used
anova(result_clar,result_2a) #Appendix Fig A.5
#no, we reject the null (clarity is nonzero). Therefore, we must use the full transformed 4Cs model

#Part of Unused Analyses
#CI for mean response. Considering most common aspects listed on BlueNile website and individual experience
#newdata3 <- data.frame(cut2 = "Good", color2 = "Colorless", carat = 1, clarity2 = "Slightly dectable")
#predict(result_2a, newdata3, level =0.95, interval = "confidence")

#lower bound: 15.75304
#predict(result_2a, newdata3, level =0.95, interval = "confidence")[2]
#upper bound: 16.18934
#predict(result_2a, newdata3, level =0.95, interval = "confidence")[3]
#fit: 15.97119
#predict(result_2a, newdata3, level =0.95, interval = "confidence")[1]

#PI for diamond with average characteristics
#predict(result_2a, newdata3, level =0.95, interval = "prediction")

#fit: 15.97119
#predict(result_2a, newdata3, level =0.95, interval = "prediction")[1]
#lower: 12.13032
#predict(result_2a, newdata3, level =0.95, interval = "prediction")[2]
#upper: 19.81206
#predict(result_2a, newdata3, level =0.95, interval = "prediction")[3]

#do the CI and PI make sense?
#subset data and find basic statistics of diamonds that fall in the chosen "average categories"
#data_average3 <- data2[which(cut2 == "Good" & color2 == "Colorless" & carat == 1 & clarity2 == "Slightly dectable"),]
#mean(data_average3$price_trans) #16.9041
#contained in PI but slightly higher than upper bound for CI

#repeat with carat 0.5
#newdata3b <- data.frame(cut2 = "Good", color2 = "Colorless", carat = 0.5, clarity2 = "Slightly dectable")
#predict(result_2a, newdata3b, level =0.95, interval = "confidence")
#predict(result_2a, newdata3b, level =0.95, interval = "prediction")
#data_average3b <- data2[which(cut2 == "Good" & color2 == "Colorless" & carat == 0.5 & clarity2 == "Slightly dectable"),]
#mean(data_average3b$price_trans) #10.72725
#again contained in PI but just slightly out of CI
#but very close to fitted value

#Briefly discussed in Unused Analyses.
#Other transformations not used
#price_log <- log(price)
#carat_log <- log(carat)

#refit regression equation with transformed response. PRICING MODEL THAT ANSWERS question 2 OF PROJECT. 
#result_log <- lm(price_log~cut2+color2+carat+clarity2)
#result_log2 <- lm(price_log~cut2+color2+carat_log+clarity2)
#result_log3 <- lm(price~cut2+color2+carat_log+clarity2)

#check that interval contains lambda 1 (if it does then variance is constant)
#boxcox(result_log) 
#boxcox(result_log2) 
#boxcox(result_log3, lambda = seq(-0.1,0.1,0.1)) 

#result_trans_full <- lm(price_trans~cut+color+carat+clarity)
#result_trans_full2 <- lm(price_trans~carat)
#summary(result_log3)

#interactions? Discussed in Unused Analyses section
#cut
##split data by manufacturer type to produce different plots
a1<-subset(data2,cut2 =="Ideal") 
a2<-subset(data2,cut2 =="Good") 

##create separate regression lines for each manufacturer
reg1<-lm(price_trans~carat,data=a1)
reg2<-lm(price_trans~carat,data=a2)

##create scatterplot with separate colors and symbols for each manufacturer
plot(carat,price_trans, main="Price (Transformed) against Carat, by Cut")
points(a2$carat,a2$price_trans, pch=2, col="red") 
##overlay separate regression lines
abline(reg1,lty=1)
abline(reg2,lty=2, col="red") 
##add legend to plot
legend("topleft", c("Ideal","Good"), lty=c(1,2), pch=c(1,2), col=c("black","red"))
#no significant interaction

#color
##split data by manufacturer type to produce different plots
b1<-subset(data2,color2 =="Colorless") 
b2<-subset(data2,color2 =="Near-colorless") 

##create separate regression lines for each manufacturer
reg1b <-lm(price_trans~carat,data=b1)
reg2b <-lm(price_trans~carat,data=b2)

##create scatterplot with separate colors and symbols for each manufacturer
plot(carat,price_trans, main="Price (Transformed) against Carat, by Color")
points(b2$carat,b2$price_trans, pch=2, col="blue") 
##overlay separate regression lines
abline(reg1b,lty=1)
abline(reg2b,lty=2, col="blue") 
##add legend to plot
legend("topleft", c("Colorless","Near-colorless"), lty=c(1,2), pch=c(1,2), col=c("black","blue")) 
#slightly more interaction but not much

#clarity
##split data by manufacturer type to produce different plots
d1<-subset(data2,clarity2 == "Undetectable") 
d2<-subset(data2,clarity2 == "Slightly detectable") 

##create separate regression lines for each manufacturer
reg1d <-lm(price_trans~carat,data=d1)
reg2d <-lm(price_trans~carat,data=d2)

##create scatterplot with separate colors and symbols for each manufacturer
plot(carat,price_trans, main="Price (Transformed) against Carat, by Clarity")
points(d2$carat,d2$price_trans, pch=2, col="lightgray") 
##overlay separate regression lines
abline(reg1d,lty=1)
abline(reg2d,lty=2, col="lightgray") 
##add legend to plot
legend("topleft", c("Undetectable","Slightly detectable"), lty=c(1,2), pch=c(1,2), col=c("black","lightgray")) 
#no significant interaction

#regression with interaction between carat and clarity
result_inter <- lm(price_trans~data2$cut2+data2$color2+data2$carat*data2$clarity2)
#summary(result_inter)
anova(result_2a, result_inter) #Appendix Figure A.7
#reject null. interaction is signficant

#regression with interaction between carat and clarity (no transformed response variable)
#result_car_clar <-lm(price~data2$cut2+data2$color2+data2$carat*data2$clarity2)
#result_car_clar_red <-lm(price~data2$cut2+data2$color2+data2$carat+data2$clarity2)
#summary(result_car_clar)
#summary(result_car_clar_red)
#anova(result_car_clar_red,result_car_clar)
#interaction is significant but 1) can't think of reason to have it 2) doesn't improve model much

#library(multcomp)
#pairwise<-glht(result3, linfct = mcp(color2= "Tukey"))
#summary(pairwise)
#Appendix Figure A.6
levene.test(price_trans,data2$color2) 
levene.test(price_trans,data2$cut2)
levene.test(price_trans,data2$clarity2)

#collapsed model without transformation. This model is not included in the report. CI and PI do not make sense.
#result4 <- lm(price~cut2+color2+carat+clarity2)
#summary(result4)

#plot(result4$fitted.values, result4$residuals, xlab = "Residuals", ylab = "Fitted Values", main = "Residual Plot (Simple Model, No Transformation)")
#abline(h=0, col = "blue")

#ACF plot
#acf(result4$residuals)

#QQ plot
#qqnorm(result4$residuals)
#qqline(result4$residuals)

#CI on mean response and PI for "common" diamond
#newdata_4 <- data.frame(cut2 = "Good", color2 = "Colorless", carat = 1, clarity2 = "Slightly dectable")
#predict(result4, newdata_4, level =0.95, interval = "confidence")
#predict(result4, newdata_4, level =0.95, interval = "prediction")

#SLR model with only price against carat
result_car <- lm(price1~carat1)
#result_1a <- lm(price1~cut1+color1+carat1+clarity1)
result_car2 <- lm(price_trans~carat)

#can we use only carat? #no, cut, color, and clarity are important
anova(result_car, result_1a) 
anova(result_car2,result_2a) 

#can we drop any predictors? Using uncollapsed. No we need all 4C's
#SLR of price against cut
result_cut <- lm(price1~cut1)

#cannot drop all predictors except cut from our full model
anova(result_cut,result_1a)

#regression model without clarity
result_dropclar <- lm(price1~cut1+carat1+color1)

#cannot drop clarity from our full model
anova(result_dropclar,result_1a)

#regression model without color or clarity
result_dropclarcolor <- lm(price1~cut1+carat1)

#cannot drop color and clarity from full model
anova(result_dropclarcolor,result_1a)

#full model ending with carat
full_model_carat <- lm(price1~cut1+color1+clarity1+carat1)

#reduced model without carat
drop_carat <- lm(price1~cut1+color1+clarity1)

#cannot drop carat from full model
anova(drop_carat,full_model_carat)

#==============================================================
#At one point, part of the modeling process included assessing whether to include or exclude outliers
#mainly to see whether we could exclude cubic transformation on the response variable
#similar results with or without outliers so we did not include the following in our report

#SUBSET DATA TO REFLECT AVERAGE CUSTOMER. ASSUMING AVERAGE CUSTOMER WILL NOT EXCEED $30K FOR DIAMOND
#subset data to reflect a population closer to the average consumer
#diamonds that cost less than $30K (from earlier in the code. Just a reminder. No need to run line again)
#data_30KL <- data2[which(price < 30000),]

detach(data2)
attach(data_30KL)

#are the categorical predictors numerical?
is.numeric(cut2)
is.numeric(color2)
is.numeric(clarity2)
#all are FALSE

#have R treat cut, color, and clarity as categorical variables
cut2 <- factor(cut2)
color2 <- factor(color2)
clarity2 <- factor(clarity2)
contrasts(cut2) #reference variable Good
contrasts(color2) #reference variable Colorless
contrasts(clarity2) #reference variable Undectable

#relevel cut2 reference class
#cut2 <- relevel(cut2, ref = "Ideal")
#attach(data_sub)
#contrasts(cut2)

#fit regression equation for subsetted data
result_sub <- lm(data_30KL$price~data_30KL$cut2+data_30KL$color2+data_30KL$carat+data_30KL$clarity2)

#summary results
summary(result_sub) #better R-squared value of 88% but we see the regression assumptions are still violted
sum(anova(result_sub)[2]) #SST

#check for constant variance for subsetted model (no trans)
boxcox(result_sub, lambda = seq(0.2,0.4,0.01)) #lambda 1 is not included

#residual plot
plot(result_sub$fitted.values, result_sub$residuals, main = "Residual Plot (Subset Data)")

#scatter
plot(price~carat)
abline(lm(price~carat), col = "blue")

#refit subsetted data with transformed price. FINAL MODEL FOR SUBSET DATA (AVERAGE CONSUMER)
result_sub_trans <- lm(data_30KL$price_trans~data_30KL$cut2+data_30KL$color2+data_30KL$carat+data_30KL$clarity2)
summary(result_sub_trans)
sum(anova(result_sub_trans)[2]) #SST

#check regression assumptions
#constant variance test using BoxCox
boxcox(result_sub_trans, lambda = seq(0.5,1.3,0.01)) #lambda 1 is now included

#residual plot
plot(result_sub_trans$fitted.values, result_sub_trans$residuals, main = "Residual Plot (Transformed Price, Subset Sample)")
#for the most part, looks good. Just a few outliers skewing the data

#ACF plot
acf(result_sub_trans$residuals, main ="ACF Plot with Transformed Price (Subset Data)")

##QQ plot of residuals
qqnorm(result_sub_trans$residuals)
qqline(result_sub_trans$residuals)

#plot carat against transformed price
plot(data_30KL$carat, data_30KL$price_trans, main = "Scatterplot of Trans Price against Carat")
abline(lm(data_30KL$price_trans~data_30KL$carat), col = "red")

#earlier, we wanted to see if we could drop clarity (BlueLink claims least important 4C)
#let's try again to see if we can drop this predictor
#result_sub_clar <- lm(data_sub$price_trans~data_sub$cut2+data_sub$color2+data_sub$carat)
#summary(result_sub_clar)
#sum(anova(result_sub_clar)[2])

#partial F test to determine if clarity can we dropped from subsetted model
#anova(result_sub_clar,result_sub_trans)
#no. we reject the null. Clarity is signficant predictor in our subsetted model

#CI for mean response (subsetted data and transformed response). 
#Considering most common aspects listed on BlueNile website and individual experience

#name data_sub variables intended to be used in mean response CI and PI
C1 <- data_30KL$cut2 
C2 <- data_30KL$color2
C3 <- data_30KL$carat
C4 <- data_30KL$clarity2
P1 <- data_30KL$price_trans

#refit regression equation using named variables
result_sub_trans2 <- lm(P1~C1+C2+C3+C4)
summary(result_sub_trans2)

#mean response CI for average diamond characteristics (subset data)
newdata_sub <- data.frame(C1 = "Good", C2 = "Colorless", C3 = 1, C4 = "Slightly detectable")
predict(result_sub_trans2, newdata_sub, level = 0.95, interval = "confidence")
#PI for diamond with average characteristics
predict(result_sub_trans2, newdata_sub, level =0.95, interval = "prediction")

#does this CI makes sense?
#subset data and find basic statistics of diamonds that fall in the chosen "average categories"
data_average_sub <- data_30KL[which(C1 == "Good" & C2 == "Colorless" & C3 == 1 & C4 == "Slightly dectable"),]
mean(data_average_sub$price)  #no value