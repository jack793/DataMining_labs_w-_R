

load('movie.RData')

summary(movie)

movie.sub = movie[, c('box', 'budget', 'animated', 'cmngsoon')] # parte 1

movie.sub[1:3,] # prima 3 righe per vedere come è costruito il sub set di analisi

# analisi prelim dei dati
hist(movie.sub$box)
hist(log(movie.sub$box))

movie.sub$box = log(movie.sub$box)
movie.sub$budget = log(movie.sub$budget)
movie.sub$cmngsoon = log(movie.sub$cmngsoon)

boxplot(box ~ animated, data = movie.sub)

pairs(box ~ budget + cmngsoon, data = movie.sub)
cor(movie.sub[,c('box','budget','cmngsoon')])

plot(movie.sub$box ~ movie$cmngsoon, col=movie$animated)
legend('bottomright', pch=c(1,1), col=c(1,2),
       legend=c('animatedN', 'animatedY'), bty='y')

plot(movie.sub$box ~ movie$budget, col=movie$animated)
legend('bottomright', pch=c(1,1), col=c(1,2),
       legend=c('animatedN', 'animatedY'), bty='y')

# creazione modelli
m1 = lm(box ~ .,data = movie.sub)
summary(m1)

m2 = lm(box ~ . -animated, data = movie.sub)
summary(m2)

anova(m2,m1)
# passiamo ad m2

m3 = lm(box ~ budget*animated + cmngsoon, data = movie.sub)
summary(m3)

anova(m3,m2)
# passiamo ad m3

m4 = lm(box ~ budget*animated + cmngsoon*animated, data = movie.sub)
summary(m4)

anova(m4,m3)
# passiamo ad m4!

m5 = lm(box ~ budget + cmngsoon*animated, data = movie.sub)
summary(m5)

anova(m5,m4)
# passiamo ad m5

plot

library(splines)

m.ns = lm(box ~ budget + ns(cmngsoon, 3), data = movie.sub)
summary(m.ns)

m.ns2 = lm(box ~ budget + ns(cmngsoon, 3)*animated,  data = movie.sub)
summary(m.ns2)

anova(m.ns2,m.ns)
#resto in m.ns

plot(m.ns)

aic = rep(0, 10)

for (i in 1:10) {
  m.glm = glm(box ~ poly(cmngsoon, i) + budget, data = movie.sub)
  aic[i] = summary(m.glm)$aic
}
which.min(aic)

m.glm = lm(box ~ poly(cmngsoon,4) + budget, data = movie.sub)
summary(m.glm)

par(mfrow=c(2,2))
plot(m.glm)

library(splines)

sp.cmn = smooth.spline(x = movie.sub$cmngsoon, y = movie.sub$box, cv = TRUE)
sp.cmn # ~2

sp.bud = smooth.spline(x = movie.sub$budget, y = movie.sub$box, cv = TRUE)
sp.bud # ~5

library(gam)

m.gam = gam(box ~ s(cmngsoon, 2) + budget, data = movie.sub)
summary(m.gam)

m.gam2 = gam(box ~ s(cmngsoon, 2) + s(budget,5), data = movie.sub)
summary(m.gam2)

anova(m.gam2,m.gam)
# meglio m.gam2

plot(m.gam2)
