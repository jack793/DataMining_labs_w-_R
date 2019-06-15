
load('icua.RData')
summary(icua)
dim(icua)

is.factor(icua$status)
icua$status = as.factor(icua$status)

icua.set = icua[,c('status', 'age', 'conscious', 'service', 'previous')]

boxplot(icua$age ~ icua$status)
# note that died person are older than youger


mosaicplot(table(icua$status,icua$conscious))
mosaicplot(table(icua$status,icua$service))
mosaicplot(table(icua$status,icua$previous))

# models

m1 = glm(status ~ ., data = icua.set, family = 'binomial')
summary(m1)


m2 = glm(status ~ . - previous, data = icua.set, family = 'binomial')
summary(m2)

m3 = glm(status ~ . + age*service - previous, data = icua.set, family = 'binomial')
summary(m3)

anova(m3,m2)

par(mfrow=c(1,1))
plot(m2)
plot(m3)

# Parte 2

library(glmnet)

y = icua$status
X = model.matrix(status ~ ., data = icua)[,-1]

m_ridge = glmnet(X, y, alpha = 0, family = 'binomial') # alpha=0 : ridge
plot(m_ridge, xvar = 'lambda')

set.seed(1234)
ridge_cv = cv.glmnet(X, y, alpha=0, family='binomial')
ridge_cv$lambda.min

m_ridge.best = glmnet(X, y, alpha = 0, family = 'binomial', lambda = ridge_cv$lambda.min
)
m_ridge.best
par(mfrow=c(1,1))
plot(ridge_cv)


m_lasso = glmnet(X, y, family = 'binomial')
plot(m_lasso, xvar = 'lambda')

set.seed(1234)
lasso_cv = cv.glmnet(X, y, family='binomial')
lasso_cv$lambda.min

m_lasso.best = glmnet(X, y, family = 'binomial', lambda = lasso_cv$lambda.min
)
m_lasso.best
par(mfrow=c(1,1))
plot(lasso_cv)

coef(m_lasso.best)

# find minimum MSE
min(lasso_cv$cvm) # 0.8403224
min(ridge_cv$cvm) # 0.839384

# vista la minima diff. per la semplicità del modello è migliore quello del lasso



