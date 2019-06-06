
###################################### LAB.1 ############################################
############################ Linear regression with R ###################################

library(MASS)
data("Boston")

Boston[1:3,]
dim(Boston)

# For convenience we can assign the information about about the number of
# houses into an object n
n = nrow(Boston)

# Consider only variables
# - medve: median values of the houses (1000 $)
# - lstat: lower status of the population (percent)
#
# We want to evaluate whether and how the value medve can be predicted using lstat. Start
# with some characteristics about the value
summary(Boston$medv)

# HISTOGRAM of the distribution
##  xlab = graphical option to assign a label to the x-axis
##  main: title of the graph
hist(Boston$medv, prob=TRUE, xlab='Median value', main='Histogram')

# BOXPLOT of the distribution
boxplot(Boston$medv, xlab='Median value', main='Boxplot' )

# Graphical evaluation of the relationship between medv and lstat
# dispersion plot
## pch=19 type of point;
## cex=0.5 reducing the dimension of the points (defaulting to 1)
## ylab: analogous to xlab but relative to y-axis
plot(Boston$medv, Boston$lstat, main='Dispersion plot',
     ylab='% of lower status of the population',
     xlab='Median value', pch=19, cex=0.5)
# > The plot shows an inverse relationship between the variables.
# Correlation between the variables
cor(Boston$medv, Boston$lstat)  # = -0.7376627

# What can we conclude?
# Try to estimate a simple linear regression model
#           medv = β0 + β1*lstat + ε

# Construct it step by step:
beta1 <- cov(Boston$medv, Boston$lstat)/var(Boston$lstat)
beta1

beta0 <- mean(Boston$medv) - beta1*mean(Boston$lstat)
beta0

#### Linear Model of Boston Dataset ####

model <- lm(medv ~ lstat, data=Boston)

# The output provides an object (model) with many details.
# basic information: estimate of the coefficients
model

# Much of the information can be visualised through command summary
summary(model)

# How can we comment on the output?
# Other information in model
names(model)

# Model-based estimated median values (extracts fitted values from model)
est.values <- fitted(model)

# Observations, model-based estimated values and linear regression fit
plot(Boston$lstat, Boston$medv, pch=19, cex=0.5,
     xlab='% of lower status of the population', ylab='Median value')

# add on the estimated values
points(Boston$lstat, est.values, pch='x', col='green')
## add on the least squares regression line
## lty=2 specifies dashed line (defaulting to lty=1 solid line)
## lwd=3 specifies line width (defaulting to lwd=1)
abline(beta0, beta1, lty=2, lwd=3, col='red')

# Residuals
res <- residuals(model)

# Graphical evaluation of the residuals:
## subdivide the window into 4 parts, 2 rows and 2 columns
par(mfrow=c(2,2))

hist(res, prob=TRUE)
plot(res, pch=19, cex=0.5, ylab='Residuals')
# add on the line parallel to the x-axis
abline(h=0, lty=2, col='red')
plot(est.values, res, pch=19, cex=0.5, xlab='Estimated values',
     ylab='Residuals')
abline(h=0, lty=2, col='red')
plot(Boston$lstat, res, ylab='Residuals',
     xlab='% of lower status of the population', pch=19, cex=0.5)
abline(h=0, lty=2, col='red')

# Graphical evaluation of the standardized residuals

par(mfrow=c(2,2))
standard.res <- rstandard(model)
hist(standard.res, prob=TRUE, xlab='Standardized residuals')
plot(standard.res, pch=19, cex=0.5, ylab='Standardized residuals')
## add on the line parallel to the x-axis
abline(h=0, lty=2, col='red')
plot(est.values, standard.res, pch=19, cex=0.5,
       xlab='Estimated values', ylab='Standardized residuals')
abline(h=0, lty=2, col='red')
plot(Boston$lstat, standard.res, ylab='Standardized residuals',
       xlab='% of lower status of the population',
       pch=19, cex=0.5)
abline(h=0, lty=2, col='red')


# Graphical evaluation of the accuracy of the model provided by R
par(mfrow=c(2,2))
plot(model)
# Are there any anomalies? There are ”suspicious” values that R indicates through the corresponding row number in the dataset, but they are not anomalous on the basis of the Cook’s 
# distance (contour is zero).

# Confidence interval at level 0.95 for β1
## variance/covariance matrix associated to the parameter estimates
vcov(model)

## standard error
se <- sqrt(diag(vcov(model)))
se

## Using R functionalities
confint(model)

# Predictions on a new dataset
predict(model, newdata=data.frame(list(lstat=c(5, 10, 25))))

## Predictions with prediction interval
predict(model, newdata=data.frame(list(lstat=c(5, 10, 25))),
            interval='prediction')



#### Multiple linear regression model ####

# Consider variable crim that includes the information about per capita crime rate by town.
# Relationship between crim and medv
par(mfrow=c(1,1))
plot(Boston$crim, Boston$medv, ylab='Median value',
     xlab='Crime', pch=19, cex=0.5)

# Estimation of the model
# medv = β0 + β1lstat + β2crim + ε
model.mv <- lm(medv ~ lstat + crim, data=Boston)
summary(model.mv)
#            Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 34.31921    0.57374  59.816   <2e-16 ***
#  lstat       -0.91139    0.04339 -21.004   <2e-16 ***
#  crim        -0.07045    0.03602  -1.956   0.0511 . 

# The significance of β2 is questionable.
# How do we interpret the parameter estimates? How is medv related to lstat?



#### Model with polynomials ####

# Consider the model without crim. Given the dispersion plot between medv and lstat we
# can try to insert a quadratic term, that is, we estimate model:
#                 medv = β0 + β1lstat + β2lstat2 + ε

model2 <- lm(medv ~ lstat + I(lstat^2), data=Boston)
## or model2 <- update(model, .~.+I(lstat^2))
summary(model2)

# The new covariate has an associated coefficient significantly different from 0.
# Compare the two models, with and without the quadratic term, 
# In R we can use function anova()

anova(model, model2)
# Note that in this case statistic F corresponds to the square of statistic t for the significance
# of the coefficient associated to the square of lstat in model2.

# Residuals of the updated model
par(mfrow=c(2,2))
plot(model2)
