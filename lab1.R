
###################################### LAB.1 ############################################

library(MASS)
data("Boston")

str(Boston)
n = nrow(Boston)
summary(Boston$medv)

library(ggplot2)
myplot = ggplot(data = Boston)
myplot + geom_histogram(aes(x=medv))
myplot + geom_point(aes(x=lstat, y=medv))
myplot + geom_boxplot(aes(x=1, y=medv))

# facciamo modello lineare semplice

covXY = cov(Boston$medv, Boston$lstat)
varX = var(Boston$lstat)*(n-1)/n
beta1 = covXY/varX
beta0 = mean(Boston$medv) - beta1*mean(Boston$lstat)

myplot + geom_point(aes(x=lstat, y=medv)) + geom_smooth(aes(x=lstat, y=medv), method = 'lm')

model1 = lm(medv ~ lstat, data = Boston)
summary(model1)

