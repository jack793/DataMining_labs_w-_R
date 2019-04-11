
###################################### LAB.4 ############################################

cement <- read.table('data_mining1819/lab4/hald.dat')

# Rename columns name
colnames(cement) = c('heat', 'ca', 'ts', 'tf', 'ds')
head(cement)

## Grafici di dispersione
pairs(cement)


## Create first model
m1 = lm(heat ~ ca, data = cement)
summary(m1)

## Try to improve m1 adding some vars = m2
m2 = lm(heat~ca+ts, data = cement)
summary(m2)

## Again..
m3 = lm(heat~ca+ts+tf+ds, data = cement)
summary(m3)
# See infact the CORRELATION MATRIX of cement (high correlation b2w X vars) ----> MULTICOLLINEARITY problem
cor(cement)



## Remove objs from Workspace and load new dataset Carseats
rm(list = ls())

# Load pckg and data
library(ISLR)
data(Carseats)
str(Carseats)

## Create lm model1 
model1 = lm(Sales~Price+US+ShelveLoc, data = Carseats)
summary(model1)

# Cambiare categoria di riferimento per le var categoriali (factors)
new.shelveloc = Carseats$ShelveLoc
contrasts(new.shelveloc) = contr.treatment(levels(new.shelveloc), base = which(levels(new.shelveloc)=='Good'))

### Metodo migliore e più veloce
new.shelveloc = relevel(Carseats$ShelveLoc, ref = 'Good')

# Riscriviamo il modello
model2 = lm(Sales~Price+US+new.shelveloc, data = Carseats)
summary(model2)


### b1^ - b1/SE(b1^) ###
set.seed(1234)
X1 = rnorm(50)
X2 = rnorm(50)
b=se=matrix(nrow = 1000, ncol = 3)
rse=rep(NA, 1000)

# Ciclo for per la creazione del vero modello
for (i in 1:1000) {
  Y = 3 + 1.5 * X1 + rnorm(50, sd=1.2)
  dati=data.frame(y=Y, x1=X1, x2=X2)
  fit = lm(y~x1+x2, data = dati)
  b[i,]=coef(fit)
  se[i,] = sqrt(diag(vcov(fit)))
  rse[i]=sd(fit$residuals)*sqrt(49/47)
}

# Creaiamo un istogramma con curva di dispersione (t-student w/ 47 degrees of fredoom)
hist((b[,2]-1.5)/se[,2], freq=F, n=25)
curve(dt(x, 47), col=2, add=T, lwd=2)
# Aggiungiamo limiti sup ed inf
linf = b[,2]+qt(0.025, 47)*se[,2]
lsup = b[,2]+qt(0.975, 47)*se[,2]
sum((linf<1.5) & (lsup>1.5))/1000

# b2=0 vs b2>0
sum((b[,3]/se[,3])>qt(0.95,47))/1000




################# LOGISTIC REGRESSION ###################

data(mtcars)
str(mtcars)

## Reduce dataset on a subset of original one
cars.data = mtcars[, c('mpg', 'vs', 'am')]
cars.data

# Refactor am to factor var for RStudio
cars.data$am = as.factor(cars.data$am)
is.factor(cars.data$am) # check

# Boxplot
boxplot(cars.data$mpg ~ cars.data$vs)
  # vs , am are binary vars => boxplot show combination of them
boxplot(cars.data$mpg ~ cars.data$vs*cars.data$am)


### Create logistic model w/ GLM (generialize linear model)
model1 = glm(vs~mpg*am, data = cars.data, family = binomial) # Variabile risposta è 0/1 quindi family=binomial setta regressione logistica
summary(model1)
