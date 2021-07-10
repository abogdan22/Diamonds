#Project 1
#Stat 6021
#Diamonds

#necessary libraries
library(MASS) #for boxplot or ACF (can't remember which)
library(lawstat) #for levene.test()
library(multcomp) #for pairise, glht

#load and attach data
data <- read.csv("diamonds4.csv", header = TRUE, sep = ",")
attach(data)

#unique entries for categorical variables
unique(clarity)
unique(color)
unique(cut)

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

#general stats of dataset
min(price)
max(price)
mean(price)
median(price)
median(carat)

#fit regression model (full model) 
result <- lm(price~carat+color+cut+clarity)
summary(result)

#drop cut? cannot drop cut
#red <- lm(price~carat+clarity+color)
#anova(red,result)
#reject the null hypothesis meaning cut is significant to the model

#fit a reduced model without clarity
reduced2 <- lm(price~carat+color+cut)
summary(reduced2)

#anova test to determine whether we can drop clarity predictor from the full model
anova(reduced2, result)

#collapse color to be colorless and near colorless
contrasts(color) #reference class is D
levels(color)
#match with levels(color) order
new.color <- c("Colorless","Colorless","Colorless","Near-colorless","Near-colorless","Near-colorless","Near-colorless") 
#add new binary variable to dataframe
data$Color.binary <- factor(new.color[color])

#collapse clarity 
contrasts(clarity) #reference class is FL
levels(clarity)
#match with levels(clarity) order
new.clarity <- c("FL2","FL2","SI","SI","VS4","VS4","VS4","VS4")
#Add new variable to dataframe
data$Clarity_3types <- factor(new.clarity[clarity])

#relevel so reference class for color.binary is colorless
Color.binary <- relevel(Color.binary, ref = 'Colorless')
contrasts(Color.binary)

#collapse
contrasts(cut)
levels(cut) #Astor Ideal / Ideal = Ideal, Very Good / Good = Good
new.cut <- c("Ideal","Good","Ideal","Good")
data$cut.binary <- factor(new.cut[cut])

#reattach data
attach(data)

#fit regression equation with collapsed clarity and color variables
#THIS ONE IS OUR MODEL
result2 <- lm(price~carat+Color.binary+cut.binary+Clarity_3types)
summary(result2)

#-7078.7 +25461.8carat -3092.6colorless +2540.8ideal -8820.3SI -5900.0VS4
-7078.7+25461.8*1-3092.6-5900.0

#Confidence interval for this type of diamond

#prediction interval for this type of diamond
  

#not useful?
#anova(result2,result)


result_carat <- lm(price~carat)
#ANOVA test to determine if cut, clarity, and color can be dropped from full model
#cannot drop those three predictor variables
anova(result_carat,result2)

#regression equation with "order of importance" *order determined by group using research obtained from the internet
full_model_cut <- lm(price~cut+carat+color+clarity)
summary(full_model_cut)

#SLR of price against cut
result_cut <- lm(price~cut)

#cannot drop all predictors except cut from our full model
anova(result_cut,full_model_cut)

#regression model without clarity
result_dropclar <- lm(price~cut+carat+color)

#cannot drop clarity from our full model
anova(result_dropclar,full_model_cut)

#regression model without color or clarity
result_dropclarcolor <- lm(price~cut+carat)

#cannot drop color and clarity from full model
anova(result_dropclarcolor,full_model_cut)

#full model ending with carat
full_model_carat <- lm(price~cut+color+clarity+carat)

#reduced model without carat
drop_carat <- lm(price~cut+color+clarity)

#cannot drop carat from full model
anova(drop_carat,full_model_carat)

#drop cut?
reduced <- lm(price~carat+Clarity_3types+Color.binary)
anova(reduced,result2)

#subset data to exclude price > 30K bc average consumer does not spend that much money on an engagement ring
#subsetting the data excluding expensive diamonds is to answer particular hypothesis question
data_small <- data[which(data$price < 30000),]

#attach subsetted pricing data
#attach(data_small)

#scatterplot of the subsetted data
plot(data_small$carat,data_small$price)

#regression equation (SLR) for subsetted data of price against carat
small_result <-lm(data_small$price~data_small$carat)
abline(lm(price~carat))

#residual plot of subsetted data of SLR equation
plot(small_result$fitted.values,small_result$residuals)
abline(h=0)

#check for constant variance of subsetted data
boxcox(small_result, lambda = seq(0.2,0.5,0.01))

#looking for interaction terms between carat, price, and specific predictor variables
#cut (a)
#subset the data frame into each of the classes
a1 <- subset(data, cut =="Ideal")
a3 <- subset(data, cut == "Good")

#seperate regressions for each cut subset
reg1 <- lm(price~carat, data = a1)
reg3 <- lm(price~carat, data = a3)

plot(carat, price, main = "Price of Diamonds against Carat, by Cut.binary")
points(a3$carat, a3$price, pch =12, col = "blue")

#color (b)
#subset the data frame into each of the classes
#PRIOR TO COLLAPSING. NEED TO REWRITE CODE FOR COLLAPSED DATA
b1 <- subset(data, color =="D")
b2 <- subset(data, color =="E")
b3 <- subset(data, color =="F")
b4 <- subset(data, color =="G")
b5 <- subset(data, color =="H")
b6 <- subset(data, color =="I")
b7 <- subset(data, color =="J")

#separate regressions for each cut subset
regb1 <- lm(price~carat, data = b1)
regb2 <- lm(price~carat, data = b2)
regb3 <- lm(price~carat, data = b3)
regb4 <- lm(price~carat, data = b4)
regb5 <- lm(price~carat, data = b5)
regb6 <- lm(price~carat, data = b6)
regb7 <- lm(price~carat, data = b7)

plot(carat, price, main = "Price of Diamonds against Carat, by Color")
points(b2$carat, b2$price, pch =2, col = "red")
points(b3$carat, b3$price, pch =12, col = "blue")
points(b4$carat, b4$price, pch = 6, col = "green")
points(b5$carat, b5$price, pch = 3, col = "orange")
points(b6$carat, b6$price, pch = 9, col = "grey")
points(b7$carat, b7$price, pch = 5, col = "pink")

#transformation needed on carat

#model with only price against carat (SLR)
result_carat <- lm(price~carat)

#check variance assumption
plot(result$fitted.values, result$residuals, main = "Residual Plot (FM)")
#both mean and variance violated. Check boxcox just in case
plot(result_carat$fitted.values, result_carat$residuals, main = "Residual Plot (Carat Only)")

#boxcox
boxcox(result_carat, lambda = seq(0.28,0.35,0.01))
#lambda = 1 is not included in the confidence interval

#transformation on response variable needed
price_t <- price^(0.31)
result_t <- lm(price_t~carat)
plot(result_t$fitted.values, result_t$residuals, main = "Residual Plot (Transformed Price, Carat Only)")
boxcox(result_t, lambda = seq(0.8,1.2,0.01))
min(carat)
max(carat)
plot(carat, price_t)

#exp_carat <- exp(carat)
#result_te <- lm(price_t~exp_carat)
#plot(result_te$fitted.values, result_te$residuals, main = "Residual Plot (Transformed Price, Carat Only)")

"""log_carat <- log(carat)
result_tl <- lm(price_t~log_carat)
plot(result_tl$fitted.values, result_tl$residuals, main = "Residual Plot (Transformed Price, Carat Only)")
plot(log_carat,price_t)

sq_carat <- carat^2
result_ts <- lm(price_t~sq_carat)
plot(result_ts$fitted.values, result_ts$residuals, main = "Residual Plot (Transformed Price, Carat Only)")
"""