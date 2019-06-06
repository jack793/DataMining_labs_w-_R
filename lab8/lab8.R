
###################################### LAB.8 #########################################

                    ## COMPONENTI PRINCIPALI per esplorare i dati##
install.packages("pls")
install.packages("rattle.data")
library(pls)
library(rattle.data)

data(wine,package="rattle.data") # Caricamento DATASET VINI
head(wine)
dim(wine)

pairs(wine[,-1],col=wine$Type) # escludo type, grafico complesso usiamo le componenti principali

?prcomp # Pricipal Components Analysis

pr=prcomp(wine[,-1],scale. = TRUE)
names(pr)
dim(pr$x)

# Le COMPONENTI PRINCIPALI sono sempre ordinate in modo descrescente
head(pr$rotation)
head(pr$sdev)

# Grafico fatto a mano (con inserimento dei colori per il vino wine$Type)
plot(pr$x[,1:2], col=wine$Type)
biplot(pr)

# Calcolo della varianza
variance = pr$sdev^2
prop_var = variance/sum(variance)
prop_var
# sommma cumulativa
cumsum(prop_var) # Arriva sempre al 100% (1.000000)

plot(prop_var, type = 'b')
plot(cumsum(prop_var), type = 'b')


############# COMPONENTI PRINCIPALI PER EFFETTUARE REGRESSIONE #######

# Cambiamo Dataset
data("gasoline")
?gasoline
# NIR: a matrix with 401 columns. The NIR spectrum.
dim(gasoline$NIR)

# Creaiamo le nostre vars
y <- gasoline$octane # risposta
X <- gasoline$NIR # matrice covariate

head(X)

# Perform PR COMP ANALYSIS
pr_analysis = prcomp(X, scale. = TRUE)

dim(X)
length(pr_analysis$sdev) # risultano 60 componenti principali

# grafico delle prime due comp princ
plot(pr_analysis$x)

# guardo correlazione ma non sembra utile
plot(pr_analysis$x[,1], y) # con la prima comp

# Calcolo VARIANZA
varianza = pr_analysis$sdev^2

pr_analysis_var = varianza/sum(varianza)

plot(pr_analysis_var, type = 'b') # spiegato già il 60%
plot(cumsum(pr_analysis_var), type = 'b') # 90%


#-- Ora con il pacchetto 'pls' che ha una funz pcr=principal component regression --#
?pcr

# Costruisco il modello!
m_pcr = pcr(y ~ X, ncomp = 20, validation = 'CV', scale = TRUE) # Cross-Validation con 20 comp
summary(m_pcr)

# TRAINING: % variance explained
# 1 comps  2 comps  3 comps  4 comps  5 comps  6 comps  7 comps  8 comps  9 comps  10 comps
# X   71.725    88.57    93.74    97.51    98.28    98.67    99.01    99.20    99.36     99.48
# y    8.856    22.69    96.39    97.40    98.18    98.51    98.51    98.57    98.79     98.79
#
# Come vediamo servono almeno 4 comps per avere una buona % di dev spiegata sulla y risposta

#### Il grafico che rappresenta è il seguente ###
validationplot(m_pcr, val.type = 'MSEP') #minsquare error 
# Graficamente 4 o 5 componenti sembra essere la scelta migliore..
validationplot(m_pcr, val.type = 'R2')

# ma con la funx selectNcomp è auto, in particolare con il metodo 'onesigma'
?selectNcomp

selectNcomp(m_pcr, method = "onesigma")
# [1] 5 , il che ci conferma quanto analizzato prima

# Ci manca da esplorare i COEFFICIENTI, si può fare graficamente con:
coefplot(m_pcr, ncomp = 1:5, legendpos = 'bottomleft')


# Per avere il valore della nostra PREDIZIONE, 
# asse X = veri valori, asse Y=valori predetti
plot(m_pcr)
abline(0, 1) # inserisco bisettrice
# (vediamo che se il nostro obj è la predizione stiamo facendo un buon lavoro)

# Si può fare anche con LASSO
library(glmnet)

cv_lasso <- cv.glmnet(X, y, alpha = 1)

# lamba minimo
cv_lasso$lambda.min

# LASSO
m_lasso = glmnet(X, y, alpha = 1, lambda = cv_lasso$lambda.min)

# confrontiamo i due metodi e vediamo..
min(cv_lasso$cvm) # 0.05618346
MSEP(m_pcr, ncomp = 5) # 0.05269
# di poco è migliore il lasso.
