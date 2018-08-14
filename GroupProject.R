rm(list=ls())
library(dplyr)
library(alr3)
library(leaps)
happiness <- read.table("Happiness.txt",header=T)
attach(happiness)




# Converting certain values to NA
happiness[, 13][happiness[, 13] == 0] <- NA
happiness[, 13][happiness[, 13] == 8] <- NA
happiness[, 13][happiness[, 13] == 9] <- NA
# Getting rid of NA values
na_index <- which( is.na(happiness$Happy))
happiness <- happiness[ -c(na_index), ]



# Household: Convert certain values to mode
happiness[, 1][happiness[, 1] == 9] <- NA
Household_most_freq <- names(sort(table(happiness$Household),decreasing=TRUE)[1]) %>% as.numeric()
happiness[, 1][is.na(happiness$Household)] <- Household_most_freq

# Health: Convert certain values to mode
happiness[, 2][happiness[, 2] == 8 |
                 happiness[, 2] == 9 |
                 happiness[, 2] == 0] <- NA
Health_most_freq <- names(sort(table(happiness$Health),decreasing=TRUE)[1]) %>% as.numeric()
happiness[, 2][is.na(happiness$Health)] <- Health_most_freq

# OwnHome: Convert certain values to mode
happiness[, 3][happiness[, 3] == 8 |
                 happiness[, 3] == 9 |
                 happiness[, 3] == 0] <- NA
OwnHome_most_freq <- names(sort(table(happiness$OwnHome),decreasing=TRUE)[1]) %>% as.numeric()
happiness[, 3][is.na(happiness$OwnHome)] <- OwnHome_most_freq

# Instagram: Convert certain values to mode
happiness[, 4][is.na(happiness[,4]) == TRUE] <- 2
happiness[, 4][happiness[,4] != 1] <- 2


# Marital
happiness[, 5][happiness[, 5] == 9] <- NA
Marital_most_freq <- names(sort(table(happiness$Marital),decreasing=TRUE)[1]) %>% as.numeric()
happiness[, 5][is.na(happiness$Marital)] <- Marital_most_freq
happiness[,6][happiness[,6] == 1] <- 0
happiness[,6][happiness[,6] == 2] <- 1
# Age
happiness[, 7][happiness[, 7] == 98 | happiness[, 7] == 99] <- NA
Age_most_freq <- names(sort(table(happiness$Age),decreasing=TRUE)[1]) %>% as.numeric()
happiness[, 7][is.na(happiness$Age)] <- Age_most_freq
# Children
happiness[, 8][happiness[, 8] == 9] <- NA
Children_most_freq <- names(sort(table(happiness$Children),decreasing=TRUE)[1]) %>% as.numeric()
happiness[, 8][is.na(happiness$Children)] <- Children_most_freq
# Education
happiness[, 9][happiness[, 9] == 99 |
                 happiness[, 9] == 98 |
                 happiness[, 9] == 97] <- NA
Education_most_freq <- names(sort(table(happiness$Education),decreasing=TRUE)[1]) %>% as.numeric()
happiness[, 9][is.na(happiness$Education)] <- Education_most_freq
# JobSat
happiness[, 10][happiness[, 10] == 8 | happiness[, 10] == 9 |  happiness[, 10] == 0] <- NA
JobSat_most_freq <- names(sort(table(happiness$JobSat),decreasing=TRUE)[1]) %>% as.numeric()
happiness[, 10][is.na(happiness$JobSat)] <- JobSat_most_freq
# Income
happiness[, 11][happiness[, 11] == 999998 |
                  happiness[, 11] == 999999 |
                  happiness[, 11] == 0] <- NA
Income_average <- mean(happiness$Income,na.rm = TRUE) %>% round(digits = 0)
happiness[, 11][is.na(happiness$Income)] <- Income_average
# WorkHrs
happiness[, 12][happiness[, 12] == -1 |
                  happiness[, 12] == 998 |
                  happiness[, 12] == 999] <- NA
WorkHrs_most_freq <- names(sort(table(happiness$WorkHrs),decreasing=TRUE)[1]) %>% as.numeric()
happiness[, 12][is.na(happiness$WorkHrs)] <- WorkHrs_most_freq

head(happiness,20)



# Create  regression equation
happiness_lm <- lm(Happy ~ Household + Health + OwnHome + Instagram + Marital + Sex + Age + Children + Education + JobSat + Income + WorkHrs, data = happiness)




summary(happiness_lm)
pairs(happiness_lm$model)





s_residual <- rstandard(happiness_lm)
plot(happiness_lm$fitted.values, s_residual, xlab = "y hat", ylab = "Standardized Residual")
abline(happiness_lm$fitted.values, s_residual)

# Looking at additional diagnostic plots which confirm that transformation is needed.
par(mfrow=c(2,2))
plot(happiness_lm)


# Transforming both y and predictor variables since scatterplot matrix showed nonlinearity and skewness in alot of the variables.
summary(t1 <- powerTransform(cbind(Household, Health, OwnHome, Marital, Age, JobSat, Income, (Children+0.01), (Education+0.01), (WorkHrs+0.01), Happy) ~ 1 , happiness))
coef(t1, round=TRUE)


# Creating transformed regression lm
summary(happiness_t <- lm(Happy ~ I(Household^-0.5) + I(OwnHome^-5) + Instagram + log(Marital) + Sex + sqrt(Age) + log(JobSat) + sqrt(Income) + log(Children+0.01) + Education + WorkHrs + sqrt(Health), data = happiness))



s_residual <- rstandard(happiness_t)
plot(happiness_t$fitted.values, s_residual, xlab = "y hat", ylab = "Standardized Residual")
abline(happiness_t$fitted.values, s_residual)

# Looking at additional diagnostic plots which confirm that transformation is needed.
par(mfrow=c(2,2))
plot(happiness_t)


pairs(happiness_t$model)


par(mfrow=c(2,3))
avPlot(happiness_t, variable = I(Household^-0.5))
avPlot(happiness_t, variable = I(OwnHome^-5))
avPlot(happiness_t, variable = Instagram )
avPlot(happiness_t, variable = log(Marital))
avPlot(happiness_t, variable = Sex)
avPlot(happiness_t, variable = sqrt(Age))


avPlot(happiness_t, variable =  log(JobSat))
avPlot(happiness_t, variable =sqrt(Income))
avPlot(happiness_t, variable = log(Children+0.01) )
avPlot(happiness_t, variable =  Education)
avPlot(happiness_t, variable =  WorkHrs)
avPlot(happiness_t, variable = sqrt(Health))



reduced.model <- step(happiness_t, direction = "backward")

summary(reduced.model)
anova(reduced.model, happiness_t) #it's better to reduce the model


happiness_t_paired <- lm(Happy ~ I(Household^-0.5) + I(OwnHome^-5) + Instagram + sqrt(Age) + log(JobSat)*sqrt(Income) + log(Marital)*log(Children+0.01) + Education + WorkHrs + Sex + sqrt(Health), data = happiness)

summary(happiness_t_paired)
reduced.paired.model <- step(happiness_t_paired, direction = "backward")
summary(reduced.paired.model)


anova(reduced.paired.model, happiness_t_paired) #decided to select reduced paired model


summary(reduced.paired.model)
summary(reduced.model)

par(mfrow=c(2,2))
plot(reduced.model)

anova(reduced.paired.model, reduced.model)
# We've compared the reduced paired model and the regular reduced model then made a conclusion that we'd better not to use the paired one. 


# plots for the final model
par(mfrow=c(2,2))


final.model <- lm(Happy ~ I(Household^-0.5) + log(Marital) + log(JobSat) + sqrt(Income) + log(Children + 0.01) + sqrt(Health), data = happiness)

plot(final.model)

summary(final.model)
anova(final.model, reduced.model) # Partial F-test says it's better to reduce the model


# avPlot, VIF, and bad leverages
par(mfrow=c(2,3))
avPlot(final.model, variable = I(Household^-0.5))
avPlot(final.model, variable = log(Marital))
avPlot(final.model, variable = log(JobSat))
avPlot(final.model, variable = sqrt(Income))
avPlot(final.model, variable = log(Children+0.01))
avPlot(final.model, variable = sqrt(Health))


vif(final.model)

hi <- hatvalues(final.model)
ri <- rstandard(final.model)
n <- length(final.model$Happy)  
p <- length(final.model$coefficients) - 1


bad_leverage_index <- which(hi > (2*(p+1)/2361) & abs(ri) > 2)
bad_leverage_index

par(mfrow=c(1,1))

plot(happiness_lm$fitted.values, happiness$Happy, xlab = "y hat", ylab = "y")
abline(lsfit(happiness_lm$fitted.values, happiness$Happy))


plot(happiness_t$fitted.values, happiness$Happy, xlab = "y hat", ylab = "y")
abline(lsfit(happiness_t$fitted.values, happiness$Happy))

plot(final.model$fitted.values, happiness$Happy, xlab = "y hat", ylab = "y")
abline(lsfit(final.model$fitted.values, happiness$Happy))
















