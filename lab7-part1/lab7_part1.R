
###################################### LAB.7 - Part I #######################################

# Lasso e Regolarizzazione in R

install.packages("glmnet")
library(glmnet) # implementa anche elastic-net

# glmnet() usage:

  # alpha=1 -> lasso
  #      =0 -> ridge

class(Salary ~ .)

# Load Hitters dataset
library(ISLR)
data("Hitters")

Hitters = na.omit(Hitters)

# Plot
boxplot(Hitters$Salary)
hist(Hitters$Salary, breaks = 30)
hist(log(Hitters$Salary), breaks = 30)

Hitters$Salary = log(Hitters$Salary)

y <- Hitters$Salary
# Togliamo INTERCETTA, ovvero prendiamo tutto TRANNE LA PRIMA colonna
X <- model.matrix(Salary ~ ., data = Hitters)[,-1]

m_ridge = glmnet(X, y, alpha = 0) # alpha=0 -> ridge

names(m_ridge)
# Plot utili
plot(m_ridge, xvar = 'lambda')
plot(m_ridge, xvar = 'dev')

# Per decidere il VALORE OTTIMALE di lambda usiamo CROSS-VALIDATION
m_ridge_cv = cv.glmnet(X, y, alpha = 0, nfolds = 10)

m_ridge_cv$lambda.min
m_ridge_cv$lambda.1se

m_ridge_best = glmnet(X, y, alpha = 0, lambda = m_ridge_cv$lambda.min)
m_ridge_best$beta


######################### REGRESSIONE LASSO #########################

m_lasso <- glmnet(X, y, alpha = 1)

plot(m_lasso)
plot(m_ridge)

# Lasso Cross-validation
m_lasso_cv = cv.glmnet(X, y, alpha = 1)
plot(m_lasso_cv)

# Val min di lambda che ci porta al min valore di min squared error
m_lasso_cv$lambda.min

m_lasso_best = glmnet(X, y, alpha = 1, lambda = m_lasso_cv$lambda.min)

# lambda entro lo s.e
m_lasso_cv$lambda.1se

# Quindi uso questo
m_lasso_best2 = glmnet(X, y, alpha = 1, lambda = m_lasso_cv$lambda.1se)
# Seleziono il modello con il min squared error che è più adatto


# Modello ibrido ridge-lasso
m_net = glmnet(X, y, alpha = 0.5)
plot(m_net, xvar = 'lambda')

m_net_cv = cv.glmnet(X, y, alpha = 0.5)

m_net = glmnet(X, y, alpha = 0.5, lambda = m_net_cv$lambda.min)
