
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
