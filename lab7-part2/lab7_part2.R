
###################################### LAB.7 - Part II ######################################

# Situazione: più covariate rispetto alle osservazioni
# p > m
# e.g: genetica, genomica

library(glmnet)

# Dataset genomico
load("lab7-part2/Leukemia.RData")
str(Leukemia)
# List of 2
# $ x: num [1:72, 1:3571] 0.562 -0.623 -0.815 0.229 -0.706 ...
# ..- attr(*, "scaled:center")= num [1:3571] -1.048 -0.867 -1.121 -0.875 0.208 ...
# ..- attr(*, "scaled:scale")= num [1:3571] 0.462 0.514 0.461 0.451 0.487 ...
# $ y: num [1:72] 0 0 0 0 0 0 0 0 0 0 ...

table(Leukemia$y)

# !!!-> Quando abbiamo dati BINARI è corretto utilizzare un modello di REGRESS. LOGISTICA
# Da un punto di vista matematico, ci saranno più soluioni

fit = glm(Leukemia$y ~ Leukemia$x, family = 'binomial')
# Avendo più parametri che osservazioni un semplice modello glm non è in grado di stimare
# tutte le osservazioni. Ecco perchà esistono i metodi con ridge (glmnet) e lasso

############### Modello RIDGE (CROSS-VALIDATION) ###############

# m_ridge = glmnet(Leukemia$x, Leukemia$y, alpha = 0)   NON È NECESSARIO
m_ridge_cv = cv.glmnet(Leukemia$x, Leukemia$y, alpha = 0, family='binomial') # BINOMIAL SE HO 0,1
m_ridge_cv

m_ridge_cv$lambda.min # Lambda MINIMO
m_ridge_cv$lambda.1se # un po di errore in più ma coefficenti più 'schiacciati'

# Plot informativo
plot(m_ridge_cv)

# Fit del miglior modello scelto
m_ridge_best = glmnet(Leukemia$x, Leukemia$y, alpha = 0, family='binomial', lambda = m_ridge_cv$lambda.min)
m_ridge_best # Spiega il 90% circa della devianza quindi è un buon modello

head(m_ridge_best$beta) # Stime dei coeff. del nostro modello


############### Modello RIDGE (CROSS-VALIDATION) ###############
m_lasso <- glmnet(Leukemia$x, Leukemia$y, alpha = 1, family = 'binomial')
plot(m_lasso, xvar='lambda')

# Lasso Cross-validation
m_lasso_cv = cv.glmnet(Leukemia$x, Leukemia$y, alpha = 1, family = 'binomial')
plot(m_lasso_cv)

# Trovato il valore di lambda desiderato fittiamo il miglior modello GLM
m_lasso_best = glmnet(Leukemia$x, Leukemia$y, alpha = 1, family = 'binomial', lambda = m_lasso_cv$lambda.min)
m_lasso_best

#      Df   %Dev   Lambda
# [1,] 23 0.9888 0.004492

# Vediamo che spiega una > % di devianza e fitta meglio, in qst caso Lasso risulta 
# essere il metodo migliore per questo tipo di dati.

########### Predictions ###########

# Aggiungo 3 pazienti random
newx <- Leukemia$x[1:3,sample(1:ncol(Leukemia$x))] # sample: esegue uno shuffle delle prime 3 colonne
head(newx[,1:3])

?predict.glmnet # Funzione per le predizioni in glm

predict.glmnet(m_lasso_best, newx, type = 'coefficient')
# tipo di predizione: coefficent -> ritorna i coefficenti
                    # response   -> ritorna la risposta

# Probabilità che appartenga alla classe n
library(gtools)
gtools::inv.logit(predict.glmnet(m_lasso_best, newx, type = 'response'))
