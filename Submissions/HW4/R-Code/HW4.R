# 
# Course: SCM 651
# Name: Team 73
# Homework #4
# Code: Loan Analysis - Main Code
# Due Date: 06/02/2019
#

# ----------------------------------------------------------------------------------------------------------------------
# DESCRIPTIVE STATISTICS
# from pastecs package
# only works on numeric variables in the set
# I did not find this useful
# Reflected in Figure 3 
# show general descriptive statistics of the data
summary(bankDF)
# show standard deviations of each variable
sapply (bankDF, sd)


# ---------------------------------------------------------------------------------------------------------------
# DESCRIPTIVE STATISTICS
# from pastecs package
# only works on numeric variables in the set
# NOT USEFUL
genStat <- stat.desc(bankDF, basic=F)
View(genStat)

# overall description (lengthy)
describe(bankDF)

test<-as.matrix(bankDF)
# Reflected in Figure 2 
summary(test)

# descriptive statistics from using stargazer package
stargazer(bankDF, type="text")

# ---------------------------------------------------------------------------------------------
# 1. Perform a logit analysis 
# PersonalLoan is a binary variable showing if a loan was issued (1) or not (0)
# using this variable, we will use the logit model to determine the logistics regression model
# using 'glm' to predict if a loan will be issued or not
mylogit <- glm(PersonalLoan ~ Age + CCAvg + CDAccount + CreditCard +
                 Education + Experience + Family + Income + Mortgage + Online +
                 SecuritiesAccount,  
               data = bankDF, family = binomial(logit))
summary(mylogit)
# easier
mylogit <- glm(PersonalLoan ~ ., data=bankDF, family=binomial(logit))

logit.or <- exp(coef(mylogit))
stargazer(mylogit,type="text", coeff=list(logit.or), p.auto=FALSE)

# from rcmdr
GLM.1 <- glm(PersonalLoan ~ Age + CCAvg + CDAccount + CreditCard + 
               Education + Experience + Family + Income + Mortgage + Online + 
               SecuritiesAccount + ZIP.Code, family=binomial(logit), data=bankDF)

summary(GLM.1)
exp(coef(GLM.1))
exp(coef(mylogit))

# force no scientific notation
options(scipen=999)

# show the summary of this information
summary(mylogit)

# Confidence Intervals
confint(mylogit, level=0.95) # CIs for model parameters

# step through
TopAICm <- step(mylogit, data=bankDF, direction="backward")
# AIC Results
#Step:  AIC=1305.81
# PersonalLoan ~ Experience + Income + Family + CCAvg + Education + 
# SecuritiesAccount + CDAccount + Online + CreditCard
# This is the same as what we concluded looking at p-values

BestglmModel <- glm(PersonalLoan ~ Experience + Income + Family + CCAvg + Education + 
                      SecuritiesAccount + CDAccount + Online + CreditCard, 
                    data = bankDF,family = binomial(logit))
summary(BestglmModel)

# could drop Experience
BestglmModel2 <- glm(PersonalLoan ~ Income + Family + CCAvg + Education + 
                      SecuritiesAccount + CDAccount + Online + CreditCard, 
                    data = bankDF,family = binomial(logit))
summary(BestglmModel2)

# could drop Experience
BestglmModel3 <- glm(PersonalLoan ~ Income + Age + Family + CCAvg + Education, 
                     data = bankDF,family = binomial(logit))
summary(BestglmModel3)


Bestlogit.or <- exp(coef(BestglmModel2))
stargazer(BestglmModel2,type="text", coeff=list(Bestlogit.or), p.auto=FALSE)

# part 2 - probit model
myprobit <- glm(PersonalLoan ~ ., family = binomial(probit), data=bankDF)
summary(myprobit)
View(myprobit)
myprobit$coefficients
# AIC Best Model
TopAICp <- step(myprobit, data=bankDF, direction="backward")

Best.probit <- glm(PersonalLoan ~ Income + Family + CCAvg + Education + 
                     SecuritiesAccount + CDAccount + Online + CreditCard, family=binomial(probit), data=bankDF)

summary(Best.probit)

# test for linearity
Ramsey <- resettest(PersonalLoan ~ Income + Family + CCAvg + Education + 
                      SecuritiesAccount + CDAccount + Online + CreditCard, 
                    power = 2:3, type = "regressor", data=bankDF)

# results show p-value < 0.05 so no linearity
# 	RESET test (lmtest package)
#
# data:  PersonalLoan ~ Income + Family + CCAvg + Education + SecuritiesAccount +     CDAccount + Online + CreditCard
# RESET = 42.094, df1 = 16, df2 = 4975, p-value < 0.00000000000000022
#

# Variance Inflation Factor (car package)
vif(Best.probit)

# Results from vif
# Income            Family             CCAvg         Education SecuritiesAccount         CDAccount            Online        CreditCard 
# 2.108947          1.251912          1.411301          1.629667          1.328519          1.903146          1.115083          1.353919 
# all under 10, so no multicollinearity

# Breusch-Pagan test of heteroscedasticity
# get error on "atomic vectors"
# bptest(PersonalLoan ~ Income + Family + CCAvg + Education, fitted.values(Best.probit), studentize = FALSE, data=bankDF)

outlierTest(BestglmModel2)
# Results for outlierTest is to eliminate customerID 1578
# rstudent unadjusted p-value Bonferonni p
# 1578  4.47774       0.0000075438     0.037719
# CustomerID PersonalLoan Age Experience Income ZIP.Code Family CCAvg Education Mortgage SecuritiesAccount CDAccount Online CreditCard
#   1578            1     34       8      65     92093      1     3       1      227                 0         0      1          0

# remove row 1578
#bankDF <- bankDF[-c(1578),]
#bankDF <- bankDF[-c(4584),]
#View(bankDF)

# neuro network
# using entire set
bankNNet2 <- neuralnet(PersonalLoan ~ Income + Family + CCAvg + Education + 
                        SecuritiesAccount + CDAccount + Online + CreditCard, 
                      bankDF, hidden=2, lifesign="minimal",
                      linear.output=FALSE, threshold=0.1)

plot(bankNNet2)

bankNNet2$result.matrix

# neural Network without binary variables
bankNNetA <- neuralnet(PersonalLoan ~ Income + Family + CCAvg + Education + Age, 
                                 bankDF, hidden=2, lifesign="minimal",
                                 linear.output=FALSE, threshold=0.1)

plot(bankNNetA)

# neural Network with significant variables without binary variables
bankNNetB <- neuralnet(PersonalLoan ~ Income + Family + CCAvg + Education, 
                       bankDF, hidden=2, lifesign="minimal",
                       linear.output=FALSE, threshold=0.1)

plot(bankNNetB)

abc <- data.frame(bankNNetB$result.matrix)
abc
# shows error
abc[1,1]
