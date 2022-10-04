#MLM
library(lme4)
library(lmerTest)
library(sjstats)

m1 = lmer(sim ~ agree + (1 + agree | ID), data=long, 
          control=lmerControl(optimizer="bobyqa",
                              optCtrl=list(maxfun=2e5)))
m2 = lmer(lik ~ agree + (1 + agree | ID), data=long, 
          control=lmerControl(optimizer="bobyqa",
                              optCtrl=list(maxfun=2e5)))
m3 = lmer(clo ~ agree + (1 + agree | ID), data=long, 
          control=lmerControl(optimizer="bobyqa",
                              optCtrl=list(maxfun=2e5)))

icc(m1)
icc(m2)
icc(m3)

summary(m1, cor=FALSE)
summary(m2, cor=FALSE)
summary(m3, cor=FALSE)