load('simulation/cars.RData')
dim(cars)

############### Analisi esplorativa dei dati  ###############

hist(cars$Price)
# distribuzione asimmettrica -> consideriamo una trasformazione logaritmica della var. risposta
hist(log(cars$Price))

boxplot(cars$Price)
# ugualmente asimmetrico (lunghezza dei baffi molto differente)
boxplot(log(cars$Price))
## queste trasformazioni in scala log hanno senso se i dati son positivi ovviamente

# é perció ragionevole trasfrmare tutti i dati di Price
cars$Price = log(cars$Price)

######## 1) Consider the dataset composed by Price, MPG.city, Horsepower, Origin, AirBags.

# MPG.city: City MPG (miles per US gallon)
# Horsepower: Horsepower
# Origin: Origin of the car? non-USA / USA  (var. dicotomica qualit.)
# AirBags: Air Bags presence (none, driver only, or driver & passenger) (var. qualitativa)

# grafico per vedere la relazione tra la risposta e le due covariate CONTINUE!!
pairs(cars[, c('Price','MPG.city','Horsepower')])

# per le cov. categoriali usiamo un boxplot:
boxplot(cars$Price ~ cars$Origin)
# linea spessa = mediana!
# la mediana delle non-USA e piu alta ma i valori dei prezzi sono molto piu variabili (ampiezza del box)

boxplot(cars$Price ~ cars$AirBags)

## Grafici di dispersione:
#  prezzo in relaz con le due var. quantitative dividendoli con i colori secondo le var. qualitative
plot(cars$Price ~ cars$MPG.city, col=cars$Origin)
plot(cars$Price ~ cars$MPG.city, col=cars$AirBags)

plot(cars$Price ~ cars$Horsepower, col=cars$Origin)
plot(cars$Price ~ cars$Horsepower, col=cars$AirBags)

############### Creazione Modello  ###############

# Parto con il modello con tutte le interazioni tra tutte le var possibili:
m = lm(Price ~ MPG.city * Origin + MPG.city * AirBags + Horsepower * Origin + Horsepower * AirBags, data = cars)
## MPG.city * Origin == MPG.city + Origin + MPG.city:Origin
summary(m)

# costruisco ora un modello solo con le covariate che sembrano essere significative (relazionate e non):
m2 = lm(Price ~ Horsepower * Origin + MPG.city * AirBags, data = cars)
summary(m2)


## Confronto i modelli:
# R2 aggiustato: piu alto in m che m2 ma il primo modello e molto piu complicato

# Anova
anova(m2,m)
# p-value: 0.1767  significativamente > 0.05 percio preferisco il modello piu piccolo, la differenza non é significativa

## Analisi dei residui: (meglio prima dividere la grafica in 2,2 con par(frow=c(2,2)))
plot(m2)
#
# dal 3o grafico Scale-Location: sospettiamo una non linearitá quindi prendiamo MTG.city e lo inseriamo come polinomio (o anche come spline in un altro modello)

library(splines)

# prima di tutto facciamo un analisi tra Price e la var che vogliamo, CON CV per utilizzare la cross-validation
sp = smooth.spline(x = cars$MPG.city, y=cars$Price, cv = TRUE)
sp
# Equivalent Degrees of Freedom (Df): 3.872711 quindi nel nuovo modello approssimiamo a 4

# con PARAMETRO DI LISCIAMENTO SI USA GAM
library(gam)
m3 = gam(Price ~ s(MPG.city, 4) + AirBags + Origin + Horsepower, data = cars)
summary(m3)

# nulla ci vieta di fare un quarto modello con le interazioni
m4 = gam(Price ~ s(MPG.city,4)*AirBags + Horsepower*Origin, data = cars)
summary(m4)
# vediamo che con spline la relazione tra MPG.city e airbgs non é piu significativa

## confrontiamo
anova(m4, m3)
# ora ha piu senso il modello pi"u complicato m4, poiché il p value é significativamente basso (< 0.05)

######## 2) Consider all the variables in the dataset. (modelli di selezione automatica)

# partiamo dalle regressioni penalizzate

library(glmnet)

# partiamo con un semplice modello lineare
model = lm(Price ~ .,data = cars) # troppo complicato ha 23 cov.

y = cars$Price
X = model.matrix(Price ~ .,data = cars) # matrice di tutte le covariate (cosí peró includamo anche l'intercetta e non NON la vogliamo)

X = model.matrix(Price ~ . ,data = cars)[,-1] # cosi eliminiamo intercetta e otteniamo la X corretta!

# modello RIDGE
m_ridge = glmnet(X, y, alpha = 0) # alpha=0 : ridge
plot(m_ridge, xvar = 'lambda')

# CV per scegliere il valore best di lambda
ridge_cv = cv.glmnet(X, y, alpha=0)
ridge_cv$lambda.min

# quindi --> MODELLO RIDGE FINALE
m_ridge = glmnet(X, y, alpha = 0, lambda = ridge_cv$lambda.min)

# per vedere i coeff. di ognuna delle covariate..
coef(m_ridge)

# modello LASSO
m_lasso = glmnet(X, y, alpha = 1)
plot(m_lasso, xvar = 'lambda')

lasso_cv = cv.glmnet(X, y, alpha=1)

# MODELLO LASSO FINALE
m_lasso = glmnet(X, y, alpha = 1, lambda = lasso_cv$lambda.min)
coef(m_lasso)

## interpretiamo (descrivendo un po i coefficenti) il modello lasso poiché e il piu semplice e quell con meno covariate considerate


# possiamo anche confrontare i mean squared err.
lasso_cv$cvm[which.min(lasso_cv$lambda)] # 0.05117485
ridge_cv$cvm[which.min(ridge_cv$lambda)] # 0.04913806

# ridge ha min sq.error un po minore quindi se il nostro obiettivo é 
# la predizione meglio ridge, altrimenti é migliore utilizzare il modello lasso perché piu semplice

