
###################################### LAB.6 #########################################

library(MASS)
data("Boston")

mod1 = lm(medv ~ lstat + I(lstat^2), data = Boston)

mod2 = lm(medv ~ poly(lstat, 3, raw = TRUE), data = Boston)
summary(model)

# Se vogliamo usare l'Adjusted R-squared allora bisogna inserire polinomio di grado 3
# nel modello

# Per capire se inserire polinomio di grado 3 invece di 2 è giustificato si usa
# la stat F, in particolare per confrontare i modelli usiamo il comando ANOVA

anova(mod1,mod2)

# Troviamo i valori di logLik per i due modelli per verificare che effettivamente
# il mod2 è preferibile da mod1
logLik(mod1)
mod1_aic = 2*2 - 2*logLik(mod1)
mod2_aic = 2*3 - 2*logLik(mod2)
# RICORDIAMO CHE: più basso il valore di Lik è, MEGLIO è!

# Per il BIC invece
mod1_bic = 2*log(nrow(Boston)) - 2*logLik(mod1)
mod2_bic = 3*log(nrow(Boston)) - 2*logLik(mod2)

# Un altro metodo di confronto è la
########### CROSS-VALIDATION ###########

library(boot)
?cv.glm  # Cross-validation HELP (modello deve essere in forma glm)

mod1_glm = glm(medv ~ lstat + I(lstat^2), data = Boston)
mod2_glm = glm(medv ~ poly(lstat, 3, raw = TRUE), data = Boston)

# Cross-validation USAGE:   cv.glm(data, glmfit, cost, K)
cv_mod1 = cv.glm(Boston, mod1_glm, K = 10)
cv_mod2 = cv.glm(Boston, mod2_glm, K = 10)

names(cv_mod1)

# ci interessa DELTA = stima dell errore del modello
cv_mod1$delta
cv_mod2$delta


################# METODI AUTOMATICI DI SCELTA DEL MODELLO #################

library(ISLR)
data("Hitters") # Baseball players dataset

boxplot(Hitters$Salary) # BOXPLOT fornisce: mediana, 1 e 3 quartile, min e max, outliers

# Usiamo il log per rendere il grafico più SIMMETRICO!
boxplot(log(Hitters$Salary))

# quindi convertiamo per semplicità...
Hitters$Salary = log(Hitters$Salary)
# ed eliminiamo tutti i giocatori che hanno valore N/A su salario
Hitters = na.omit(Hitters)

# Installiamo paccheto leaps che contiene tutti i metodi di confronto modelli automatici
# nel sottopacchetto  --> regsubsets
install.packages('leaps')
library(leaps)

# Creiamo il modello con questo pacchetto
m_forward <- regsubsets(Salary ~ ., data = Hitters, method = 'forward', nvmax = 19) # nv specifica n di covariate

summary(m_forward)
# Nel sommario notiamo (via *) quali var. e con che ordine sono state inserite nel modello!

# non sappiamo ancora qual è il modello migliore...
names(summary(m_forward))
# alcuni VALORI UTILI del summary del modello sono:
summary(m_forward)$rss
summary(m_forward)$adjr2
summary(m_forward)$bic

# TROVIAMO IL MODELLO: cercando il minimo dei valori bic del modello per definirlo completamente
# Questo è il miglior modello CON 4 COVARIATE, sta a noi ora decidere quante tenerne...

# selezione con BIC: modelli più semplici
# selezione con R quadro aggiunstato: modelli più complicati

which.min(summary(m_forward)$bic) #->  [1] 4
coef(m_forward, 4) # troviamo i coefficenti delle var del modello. Risultato:
#
# (Intercept)        Hits       Years       CRuns     PutOuts 
# 4.432357335 0.006780439 0.053285280 0.000753155 0.000351109 
#

# GRAFICAMENTE
plot(m_forward)
plot(m_forward, scale = 'adjr2') # solo su un criterio

# evidenziamo valore minimo, che sappiamo essere il 4o
plot(summary(m_forward)$rss, type = 'l')
points(4, summary(m_forward)$rss[4], col=2, pch=19)

plot(summary(m_forward)$bic, type = 'l')
points(4, summary(m_forward)$bic[4], col=2, pch=19)

plot(summary(m_forward)$adjr2, type = 'l')
points(4, summary(m_forward)$adjr2[4], col=2, pch=19)
points(8, summary(m_forward)$adjr2[8], col=4, pch=19) # vediamo che il 4o è basso quindi prendiamo l'8o

# L' altro modello con l'algor. di selez è simile..
m_back <- regsubsets(Salary ~ ., data = Hitters, method = 'backward', nvmax = 19)


# CAPITO IL MODELLO da utilizzare facciamo il FIT del modello selezionato, sempre con lm()
fit4 = lm(Salary ~ Hits + Years + CRuns + PutOuts, data = Hitters)

summary(fit4)
plot(fit4)

# Come ultima cosa vediamo che BIC dice che bastano 3 covariate per un modello ottimo:
which.min(summary(m_back)$bic)
#  [1] 3
coef(m_back, 3)
#  (Intercept)        Hits       Walks       Years 
#  4.231360675 0.006811902 0.006582034 0.094467566 
coef(m_forward, 4)
#  (Intercept)        Hits       Years       CRuns     PutOuts 
#  4.432357335 0.006780439 0.053285280 0.000753155 0.000351109 

