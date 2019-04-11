
###################################### LAB.3 ############################################

# Load dataset from csv file
dati = read.csv("lab3/Gender_Discrimination.csv", sep = ',')
dim(dati)
summary(dati)

# comandi utili
is.factor(dati$Gender) # é una variabile CATEGORIALE(QUALITATIVA)??
levels(dati$Gender)
table(dati$Gender)

# Boxplot
boxplot(dati$Salary, las=2) # las=2: raddrizza le labels
# la scatola da il primo mediana e terzo quartile

# Grafico a torta
pie(table(dati$Gender), labels = c('F','M')) # necessita di table() per le categorie


## Boxplot utile che mette in relazione Gender con relativo Salary
boxplot(dati$Salary~dati$Gender, col=c('pink', 'blue'), las=2)

## Esperienza con genere
boxplot(dati$Experience~dati$Gender, col=c('pink', 'blue'), las=2)


## Per i dati quantitativi esploriamo con
## Grafico di dispersione
plot(dati$Experience, dati$Salary, 
     las=2, main="Sal vs Exp", xlab = "Exp", ylab = "Sal", cex.axis=0.7)
# cex.axis: ingrandire il carattere sugli assi

# Aggiungiamo i colori per inserire la 3a var (Gender) categoriale
plot(dati$Experience, dati$Salary, 
     las=2, main="Sal vs Exp", xlab = "Exp", ylab = "Sal", cex.axis=0.7, pch=19, col=as.numeric(dati$Gender))
# pch: parametro grafico (19=pallino pieno) as.numeric converte in 1/2 i dati qualitativi binari di Gender

# Aggiungiamo la legenda al grafico sopradefinito
legend('topleft', pch=c(19,19), c('Female', 'Male'), col=c(1,2), bty='n') # bty: box intorno la legenda



## Creiamo modello lineare
model <- lm(Salary~Gender+Experience, data = dati)
summary(model)

#  --> sal^=53260+17020*1(g=m)+1744*Exp
#   --> (gender=F) sal^=53260+1744*Exp

# Aggiungiamo una retta al grafico (una x Female una per Male)
beta = coef(model) # estraggo i beta cappello
abline(beta[1], beta[3], col=1) # 1arg: intercetta, 2arg: coeff.angolare
abline(beta[1]+beta[2], beta[3], col=2) # PER I MASCHI AGGIUNGO BETA 2 all'intercetta

model2 = lm(Salary~Gender*Experience, data = dati)
summary(model2)

# (gender=M) sal^= 66333-8034+(666+2086)*Exp
# (gender+F) sal^= 66333 + 666*Exp
## Teniamo GenderMale per PRINCIPIO DI GERARCHIA


## Anova test per confrontare i due modelli (normale vs interazione)
anova(model,model2)

## Plot del modello 2
plot(dati$Experience, dati$Salary, 
     las=2, main="Sal vs Exp", xlab = "Exp", ylab = "Sal", cex.axis=0.7, pch=19, col=as.numeric(dati$Gender))

legend('topleft', pch=c(19,19), c('Female', 'Male'), col=c(1,2), bty='n') # bty: box intorno la legenda

beta2 = coef(model2) # calcolo tutti i beta per il secondo modello
abline(beta2[1], beta2[3], col=1)
abline(beta2[1]+beta2[2], beta2[3]+beta2[4], col=2)


## Creiamo un 3o modello POLINOMIALE per vedere se esiste questo tipo di relazione

model3 = lm(Salary~Gender+Experience+I(Experience^2)+Gender:Experience, data = dati) # gender:exp inserisce il termine interazione tra le 2 var
summary(model3)

# Confrontiamo med2 con mod3
anova(model2,model3)

## --> é migliore il modello 2!!! 

## Analiziamo infine i RESIDUI del migliore modello raggiunto: ovvero il model2!
plot(model2)
# ...visualizza tutti plot di seguito premendo ENTER in console..

## Come fare le PREVISIONI!!
predict(model2, newdata = data.frame(list(Experience=20, Gender="Male")))
beta2[1]+beta2[2]+(beta2[3]+beta2[4])*20 # Fatta manualmente

predict(model2, newdata = data.frame(list(Experience=20, Gender="Female")))
beta2[1]+beta2[2]+(beta2[3])*20




