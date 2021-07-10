#Project 1
#Stat 6021
#Summer 2021
#Diamonds


#necessary libraries
library(MASS) #for boxplot or ACF (can't remember which)
library(lawstat) #for levene.test()
library(multcomp) #for pairise, glht

#load dataset
data2 <- read.csv("diamonds4.csv", header = TRUE, sep = ",")
attach(data2)

#easy attach and detach calls
#entire diamonds dataset
attach(data2)
detach(data2)
attach(data_sub)
detach(data_sub)

#fit regression model. First-order. No collapsed variables
result_F_alt <- lm(price~cut+color+carat+clarity)
summary(result_F_alt)
sum(anova(result_F_alt)[2]) #SST= 705,838,000,000

#are the categorical predictors numerical?
is.numeric(cut)
is.numeric(color)
is.numeric(clarity)
#all three are False

#have R treat cut, color, and clarity as categorical variables
cut <- factor(cut)
color <- factor(color)
clarity <- factor(clarity)
contrasts(cut) #reference variable Astor Ideal
contrasts(color) #reference variable D
contrasts(clarity) #reference variable FL

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
new.clarity <- c("Undectable","Undectable","Slightly dectable","Slightly dectable","Slightly dectable","Slightly dectable","Undectable","Undectable")

#Add new variable to dataframe
data2$clarity2 <- factor(new.clarity[clarity])
attach(data2)
contrasts(clarity2) 

#relevel so reference class for clarity2 is undectable
clarity2 <- relevel(clarity2, ref = "Undectable")
contrasts(clarity2)

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

#refit regression equation with collapsed variables for cut, color, and clarity
result3 <- lm(price~cut2+color2+carat+clarity2)
summary(result3)
sum(anova(result3)[2])

#CI for mean response. Considering most common aspects listed on BlueNile website and individual experience
newdata <- data.frame(cut2 = "Good", color2 = "Colorless", carat = 1, clarity2 = "Slightly dectable")
predict(result3, newdata, level =0.95, interval = "confidence")

#lower bound: $9,407.86
predict(result3, newdata, level =0.95, interval = "confidence")[2]
#upper bound: $12,386.46
predict(result3, newdata, level =0.95, interval = "confidence")[3]
#fit: $10,897.16
predict(result3, newdata, level =0.95, interval = "confidence")[1]

#PI for diamond with average characteristics
predict(result3, newdata, level =0.95, interval = "prediction")

#fit: $10,897.16
predict(result3, newdata, level =0.95, interval = "prediction")[1]
#lower: -$15,324.34
predict(result3, newdata, level =0.95, interval = "prediction")[2]
#upper: $37,118.67
predict(result3, newdata, level =0.95, interval = "prediction")[3]

#do the CI and PI make sense?
#subset data and find basic statistics of diamonds that fall in the chosen "average categories"
data_average <- data2[which(cut2 == "Good" & color2 == "Colorless" & carat == 1 & clarity2 == "Slightly dectable"),]
mean(data_average$price) #$4,971.56, so model isn't accurate. FItted price is 2x as much as average price of diamonds with these characteristics

#check linear assumptions
boxcox(result3, lambda = seq(0.3,0.4,0.01)) #lambda 1 is not included

#transformation on response variable
price_trans <- price^(1/3)
result_trans <- lm(price_trans~cut2+color2+carat+clarity2)

#refit regression equation with transformed response
boxcox(result_trans, lambda = seq(0.9,1.1,0.1)) #lambda 1 is now included in interval

#summary results
summary(result_trans)
#model still significant
#individual pvalues for coefficients still signficant
#but now R squared is much higher from 69% to 94%
#and standard error as signficiantly decreased
sum(anova(result_trans)[2])
#transformed response variable also significantly reduces the SST

#check regression assumptions for fitted model with transformed response variable
#residual plot
plot(result_trans$fitted.values, result_trans$residuals, main = "Residual Plot (Transformed Price)")

#ACF plot
acf(result_trans$residuals, main ="ACF Plot with Transformed Price")

##QQ plot of residuals
qqnorm(result_trans$residuals)
qqline(result_trans$residuals)

#add transformed price to data2
data2$price_trans <- price_trans

#fit data for average diamond characteristics
#recall newdata
predict(result_trans, newdata, level =0.95, interval = "confidence")

#lower bound: 15.75 --> 15.75^3 = 3,909.25
predict(result_trans, newdata, level =0.95, interval = "confidence")[2]^3
#upper bound: 16.19 --> 16.19^3 = 4,243.14 
predict(result_trans, newdata, level =0.95, interval = "confidence")[3]^3
#fit: 15.97 --> 15.96^3 = 4,073.91 
predict(result_trans, newdata, level =0.95, interval = "confidence")[1]^3

#PI for diamond with average characteristics
predict(result_trans, newdata, level =0.95, interval = "prediction")
#fit: 15.97 --> 15.97^3 = 4,073.91 
predict(result_trans, newdata, level =0.95, interval = "prediction")[1]^3
#lower: 12.13 --> 12.13^3 = 1,784.77
predict(result_trans, newdata, level =0.95, interval = "prediction")[2]^3
#upper: 19.81 --> 19.81^3 = 7,774.16
predict(result_trans, newdata, level =0.95, interval = "prediction")[3]^3

#transformed price data is more accurate with average price for diamonds with average characteristics
#recall the mean price was
mean(data_average$price)

#can we drop any 4C? Ex: BlueLink claims clarity is least important 4C
result_clar <- lm(price_trans~cut2+color2+carat)
summary(result_clar)
sum(anova(result_clar)[2])

#partial F test to determine if reduced model without clarity can be used
anova(result_clar,result_trans)
#no, we reject the null (clarity is nonzero). Therefore, we must use the full transformed 4Cs model

"""
#check for interactions
#looking for interaction terms between carat, price, and specific predictor variables
#cut (a)
#subset the data frame into each of the classes
a1 <- subset(data2, cut2 =="Ideal")
a2 <- subset(data2, cut2 == "Good")

#seperate regressions for each cut subset
reg1 <- lm(price_trans~carat, data2 = a1)
reg2 <- lm(price_trans~carat, data2 = a2)

#Plot scatterplot to see if interactions exist (visually)
plot(carat, price_trans, main = "Trans Price of Diamonds against Carat, by Cut2")
points(a2$carat, a2$price_trans, pch =12, col = "blue")
abline(reg1,lty=1)
abline(reg2,lty=2, col="blue") 
legend("topleft", c("Ideal","Good"), lty=c(1,2), pch=c(1,12), col=c("black","blue")) 

#color (b)
#cut (a)
#subset the data frame into each of the classes
b1 <- subset(data2, color2 =="Colorless")
b2 <- subset(data2, color2 == "Near-colorless")

#seperate regressions for each cut subset
reg1b <- lm(price_trans~carat, data2 = b1)
reg2b <- lm(price_trans~carat, data2 = b2)

#Plot scatterplot to see if interactions exist (visually)
plot(carat, price_trans, main = "Trans Price of Diamonds against Carat, by Color2")
points(b2$carat, b2$price_trans, pch =12, col = "blue")
abline(reg1b,lty=1)
abline(reg2b,lty=2, col="blue") 
legend("topleft", c("Colorless","Near-colorless"), lty=c(1,2), pch=c(1,12), col=c("black","blue")) 

#we don't expect there to be interactions between the 4Cs and two of the predictors confirm our suspicions
#therefore, not fitting model for interactions
"""
#SUBSET DATA TO REFLECT AVERAGE CUSTOMER
#subset data to reflect a population closer to the average consumer
data_sub <- data2[which(data2$price < 30000),]

attach(data_sub)

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
cut2 <- relevel(cut2, ref = "Ideal")
attach(data_sub)
contrasts(cut2)

#fit regression equation for subsetted data
result_sub <- lm(data_sub$price~data_sub$cut2+data_sub$color2+data_sub$carat+data_sub$clarity2)

#summary results
summary(result_sub)
sum(anova(result_sub)[2]) #SST

#check for constant variance for subsetted model (no trans)
boxcox(result_sub, lambda = seq(0.2,0.4,0.01)) #lambda 1 is not included

#refit subsetted data with transformed price
result_sub_trans <- lm(data_sub$price_trans~data_sub$cut2+data_sub$color2+data_sub$carat+data_sub$clarity2)
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
plot(data_sub$carat, data_sub$price_trans, main = "Scatterplot of Trans Price against Carat")
abline(lm(data_sub$price_trans~data_sub$carat), col = "red")

#earlier, we wanted to see if we could drop clarity (BlueLink claims least important 4C)
#let's try again to see if we can drop this predictor
#result_sub_clar <- lm(data_sub$price_trans~data_sub$cut2+data_sub$color2+data_sub$carat)
#summary(result_sub_clar)
#sum(anova(result_sub_clar)[2])

#partial F test to determine if clarity can we dropped from subsetted model
#anova(result_sub_clar,result_sub_trans)
#no. we reject the null. Clarity is signficant predictor in our subsetted model

detach(data_sub)
attach(data_sub)
#CI for mean response (subsetted data and transformed response). 
#Considering most common aspects listed on BlueNile website and individual experience

#name data_sub variables intended to be used in mean response CI and PI
C1 <- data_sub$cut2 
C2 <- data_sub$color2
C3 <- data_sub$carat
C4 <- data_sub$clarity2
P1 <- data_sub$price_trans

#refit regression equation using named variables
result_sub_trans2 <- lm(P1~C1+C2+C3+C4)
summary(result_sub_trans2)

#mean response CI for average diamond characteristics (subset data)
newdata_sub <- data.frame(C1 = "Good", C2 = "Colorless", C3 = 1, C4 = "Slightly dectable")
predict(result_sub_trans2, newdata_sub, level = 0.95, interval = "confidence")

#lower bound: $4,230.68
predict(result_sub_trans2, newdata_sub, level = 0.95, interval = "confidence")[2]^3
#upper bound:$4,468.66
predict(result_sub_trans2, newdata_sub, level = 0.95, interval = "confidence")[3]^3
#fit: $4,348.58
predict(result_sub_trans2, newdata_sub, level = 0.95, interval = "confidence")[1]^3

#PI for diamond with average characteristics
predict(result_sub_trans2, newdata_sub, level =0.95, interval = "prediction")
#fit: $4,348.58
predict(result_sub_trans2, newdata_sub, level =0.95, interval = "prediction")[1]^3
#lower: $2,640.15
predict(result_sub_trans2, newdata_sub, level =0.95, interval = "prediction")[2]^3
#upper: $6,669.71
predict(result_sub_trans2, newdata_sub, level =0.95, interval = "prediction")[3]^3

#does this CI makes sense?
#subset data and find basic statistics of diamonds that fall in the chosen "average categories"
data_average_sub <- data_sub[which(C1 == "Good" & C2 == "Colorless" & C3 == 1 & C4 == "Slightly dectable"),]
mean(data_average$price) #mean price of subsetted data $4,971.55
#transformed model fits with our estimated price, mean response CI, and PI
