
################# LOGISTIC REGRESSION ###################

data(mtcars)
str(mtcars)

## Reduce dataset on a subset of original one
cars.data = mtcars[, c('mpg', 'vs', 'am')] # no rows, columns --> c = concatenate a vector
cars.data

# Refactor am to factor var for RStudio
cars.data$am = as.factor(cars.data$am)
is.factor(cars.data$am) # check



# Create model1 Generialize Linear Model (GLM)
m.c = glm(vs~mpg*am, data = cars.data, family = binomial) # Variabile risposta è 0/1 quindi family=binomial setta regressione logistica
summary(m.c)

# Simplify this model erasing not significative iteration
m.c2 = glm(vs ~ mpg + am, data = cars.data, family = binomial)
summary(m.c2)


# Analysis of DEVIANCE
1-pchisq(20.646, 29)  # P-value of test --> 0,08.. so I accept simpliest model

# delete am create mc3
m.c3 = glm(vs ~ mpg, data = cars.data, family = binomial)
summary(m.c3)

# Anova test to compare mc2 w/ mc3
anova(m.c3, m.c2, test = 'Chisq')

# Find quantiles

  # pchisq(x)=P
  # qchisq(P)=x

qchisq(0.95, 1) # = 3.84

#### m.c2 is the best model

# extract beta^ from mc2
estimate = coef(m.c2)
# extract standard error of beta^
se = sqrt(diag(vcov(m.c2)))

# confidence interval 90%
c(estimate[2]-qnorm(0.95)*se[2], estimate[2]+qnorm(0.95)*se[2])
confint(m.c2, level = 0.9)

# predict
ev = predict(m.c2)
ep = predict(m.c2, type = 'response')

exp(ev)/(1+exp(ev))

## Plots
plot(cars.data$mpg, cars.data$vs, pch=19, col=1+as.numeric(cars.data$am))

# Curva di regressione
curve(predict(m.c2, newdata = data.frame(mpg=x, am='0'), type='response'), add=T)
curve(predict(m.c2, newdata = data.frame(mpg=x, am='1'), type='response'), add=T, col=2)

# create prediction vector of the model
preds = rep(0, nrow(cars.data)) # vettore di 0
preds[ep>0.5] = 1 # put 1 only if ep > 0.5

# see confusion matrix
addmargins(table(preds, vs=cars.data$vs)) # add total for every colums w/ this command

n = nrow(cars.data)
set.seed(222)
sel = sample(n, 0.6*n, replace = F)

# training set
tr.s = cars.data[sel,]
# test set
te.s = cars.data[-sel,]

m.ct = glm(vs ~ mpg + am, data = tr.s, family = binomial())
summary(m.ct)

prob.test = predict(m.ct, newdata = te.s, type = 'response')
preds.test = rep(0, length(prob.test))
preds.test[prob.test>0.5] = 1
addmargins(table(preds.test, vs = te.s$vs))


###### LINEAR DISCRIMINANT ANALYSIS #####

library(MASS)

m.cl = lda(vs ~ mpg + am, data = tr.s)
m.cl

# Plots
plot(m.cl)

# predictions
preds.lda = predict(m.cl, te.s)
p.lda = rep(0, nrow(te.s))
preds.lda
p.lda[preds.lda$posterior[,2]>0.5] = 1
addmargins(table(p.lda, vs=te.s$vs))
p.lda           
           