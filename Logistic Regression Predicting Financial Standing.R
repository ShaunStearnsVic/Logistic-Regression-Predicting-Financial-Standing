library(gains)
library(caret)
setwd("~/Desktop/CSU Global Data Analytics/MIS510/Module 4/")
bank.df <- read.csv("banks.csv")
dim(bank.df)
t(t(names(bank.df)))
head(bank.df)
bank.df <- bank.df[ , -c(1)]  # Drop ID column.


# treat Education as categorical (R will create dummy variables)
bank.df$Financial.Condition <- factor(bank.df$Financial.Condition, levels = c(1, 0),
                                      labels = c("Strong", "Weak"))

t(t(names(bank.df)))
#glm() (general linear model) with family = "binomial" to fit a logistic regression. 
#Max iteration added for convergence
logit.reg <- glm(Financial.Condition ~ ., data = bank.df, family = "binomial", 
                 control=glm.control(maxit = 1)) 
options(scipen=999)
summary(logit.reg)


#Multiple regression model
bank.df <- read.csv("banks.csv")
bank.df <- bank.df[ , -c(1)]  # Drop ID column.
Mult.Reg <- lm(Financial.Condition ~ ., data = bank.df)
summary(Mult.Reg)


#Lostic Regression Model with coefficients and odds
bank.df$Financial.Condition <- factor(bank.df$Financial.Condition, levels = c(1, 0),
                                      labels = c("Strong", "Weak"))
logit.reg <- glm(Financial.Condition ~ ., data = bank.df, family = "binomial", 
                 control=glm.control(maxit=1))
data.frame(summary(logit.reg)$coefficients, odds = exp(coef(logit.reg))) 
round(data.frame(summary(logit.reg)$coefficients, odds = exp(coef(logit.reg))), 5)


#Confusion Matrix
predict <- predict(logit.reg, type = 'response')
table(bank.df$Financial.Condition, predict > .5)

library(gains)
bank.df <- read.csv("banks.csv")
bank.df <- bank.df[ , -c(1)]  # Drop ID column.

pred <- predict(logit.reg, bank.df)
gain <- gains(bank.df$Financial.Condition, pred, groups=2)

plot(c(0,gain$cume.pct.of.total*sum(bank.df$Financial.Condition))~
       c(0,gain$cume.obs), 
     xlab="# cases", ylab="Cumulative", main="", type="l")
lines(c(0,sum(bank.df$Financial.Condition))~c(0, dim(bank.df)[1]), lty=2)
                
                