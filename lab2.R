
###################################### LAB.2 ############################################

model1$coefficients[2]/0.03873

# Creo plot ed inserisco punto
curve(dt(x, df=n-2), -25,25)
points(-24.53,0, col=2)

integrate(dt, -3,3, df=n-2)  # 0.9971666 with absolute error < 1.1e-06

# Ipotesi nulla H0: beta1 = -1

(model1$coefficients[2]+1)/0.03873
points(1.2897,0, col=4)

# p-value del test poco sopra
pt(1.2897, df=n-2, lower.tail = FALSE)*2  # [1] 0.1977463 (~20%) >> Sto erroneamente rifiutando H0

# Residui del nostro model1
res <- residuals(model1)  # model1$residuls (alternative mode)

# Divido il plot frame in 4 quadranti, 2 righe e 2 colonne
par(mfrow=c(2,2))

# Inserisco i miei 4 grafici
hist(res, prob=TRUE, xlab = "Residui")
plot(res, ylab = "Residui", main = "Distribuzione residui", cex=0.5)
plot(model1$fitted.values, res, xlab = "Valori predetti", ylab = "Residui", cex=0.5)
plot(Boston$lstat, res, xlab = "lstat", ylab = "Residui", cex=0.5)

# Resetto plot frame ed inserisco grafico a dispersione con linea mediana
par(mfrow=c(1,1)); plot(Boston$lstat, Boston$medv)
abline(coefficients(model1), col=4, lwd=2) # lwd: set line width (def=1)

# Automatic plot summary for model1
par(mfrow=c(2,2))
plot(model1)

# standardizzo i residui e lavoro sul minimo
res.sd = rstandard(model1) 
min(res.sd)
qnorm(0.5)
median(res.sd)
# Nel Q-Q plot (quartile-quartile) osservo chiaramente che la curva di destra
# e' piu' pesante (come mostrava l'istogramma)

# Analizziamo i punti estremi del 4o plot (215, 375)
par(mfrow=c(1,1)); plot(Boston$lstat, Boston$medv)
points(Boston[375, 'lstat'], Boston[375, 'medv'], col=2, pch=19)  # pch: specify point shapes
points(Boston[215, 'lstat'], Boston[215, 'medv'], col=2, pch=19)


# Vista l'analisi dei residui..
############### Facciamo un nuovo modello in cui lstat compare anche al quadrato ################

model2 <- lm(medv ~ lstat + I(lstat^2), data = Boston)  # I: create a new var
summary(model2)

# Aggiungiamo al grafico Boston precedente un altra retta
par(mfrow=c(1,1)); plot(Boston$lstat, Boston$medv)
abline(coefficients(model1), col=4, lwd=2)
coeff2 = coefficients(model2)
regr2 = function(x) 42.86 -2.33*x + 0.0435*(x^2)
curve(regr2, lwd=2, col=2, add = TRUE)

# Summary plot model2
par(mfrow=c(2,2))
plot(model2)


############### Modello di regressione multivariato ################

pairs(Boston[,c(14,1:5)])

model3 = lm(medv ~ ., data = Boston)

X = as.matrix(Boston[,1:13])
X = cbind(rep(1,n), X)  # cbind: Take a matrix arguments and combine by columns or rows, respectively.

betahat = solve(t(X) %*% X) %*% t(X) %*% as.double(Boston$medv)
cbind(betahat, coefficients(model3))

summary(model3)

