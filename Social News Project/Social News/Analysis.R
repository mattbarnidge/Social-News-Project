#Load Data and Libraries
setwd("~/Documents/GitHub/Social-News-Project/Social News Project/Social News")
load("socnews.Rdata")
load("sub.Rdata")
load("long.Rdata")

library(lme4)
library(lmerTest)
library(sjstats)
library(lavaan)

#Part 1: Regular outcomes

#Correlations among outcomes
with(x, cor(cbind(soccon, combel, soctrust), 
            use="complete.obs", method="pearson"))

#Residualize for SEM
r1 = lm(fb.freq ~ age + gender + poc + edu + inc + netsize + netdiv2, data=x, na.action=na.exclude)
x$x1 <- residuals(r1)

#Path analysis with correlated outcomes
sem1 <- '
soccon ~ .10*x1
combel ~ x1
soctrust ~ x1
'

fit1 <- sem(sem1, data = x)
fitMeasures(fit1, c("chisq", "df", "pvalue", "cfi",  "gfi", "rmsea", "rmsea.pvalue", "srmr"))
standardizedSolution(fit1)
#B1 = .28, B2 = .18, B3 = .10

sem2 <- '
soccon ~ x1
combel ~ .10*x1
soctrust ~ x1
'

fit2 <- sem(sem2, data = x)
fitMeasures(fit2, c("chisq", "df", "pvalue", "cfi",  "gfi", "rmsea", "rmsea.pvalue", "srmr"))
standardizedSolution(fit2)
#B1 = .22, B2 = .15, B3 = .09

sem3 <- '
soccon ~ x1
combel ~ x1
soctrust ~ .05*x1
'

fit3 <- sem(sem3, data = x)
fitMeasures(fit3, c("chisq", "df", "pvalue", "cfi",  "gfi", "rmsea", "rmsea.pvalue", "srmr"))
standardizedSolution(fit3)
#B1 = .21, B2 = .14, B3 = .08

#Average estimates: B1 = .21, B2 = .16, B3 = .09

#Layer on agreement and engagement
r2 = lm(ng.agree ~ age + gender + poc + edu + inc + netsize + netdiv2, data=x, na.action=na.exclude)
r3 = lm(ng.engage ~ age + gender + poc + edu + inc + netsize + netdiv2, data=x, na.action=na.exclude)
x$x2 <- residuals(r2)
x$x3 <- residuals(r3)

sem4 <- '
soccon ~ .06*x1 + x2 + x3
combel ~ .07*x1 + x2 + x3
soctrust ~ .03*x1 + x2 + x3
'

fit4 <- sem(sem4, data = x)
fitMeasures(fit4, c("chisq", "df", "pvalue", "cfi",  "gfi", "rmsea", "rmsea.pvalue", "srmr"))
standardizedSolution(fit4)

#Part 2: repeated measures outcomes

#Correlations among outcomes
with(long, cor(cbind(sim, lik, clo), 
            use="complete.obs", method="pearson"))

#combine
long$affect = with(long, rowMeans(cbind(sim,lik,clo), na.rm=T))

#MLM
mlm = lmer(affect ~ agree + engage.x + fb.freq + 
             age + gender + poc + edu + inc + netsize + netdiv2 + (1 | ID), 
           data=long, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(mlm, cor=F)
icc(mlm)




######################


#Baseline relationships
m1 = lm(soccon ~ fb.freq + age + gender + poc + edu + inc + netsize + netdiv2, data=x)
m2 = lm(combel ~ fb.freq + age + gender + poc + edu + inc + netsize + netdiv2, data=x)
m3 = lm(soctrust ~ fb.freq + age + gender + poc + edu + inc + netsize + netdiv2, data=x)
m4 = lm(ng.sim ~ fb.freq + age + gender + poc + edu + inc + netsize + netdiv2, data=x)
m5 = lm(ng.lik ~ fb.freq + age + gender + poc + edu + inc + netsize + netdiv2, data=x)
m6 = lm(ng.clo ~ fb.freq + age + gender + poc + edu + inc + netsize + netdiv2, data=x)

summary(m1) #B = .08, SE = .02, p < .001
summary(m2) #B = .10, SE = .03, p < .001
summary(m3) #B = .05, SE = .03, p < .05
summary(m4) #B = .03, SE = .03, p = .356
summary(m5) #B = .01, SE = .03, p = .643
summary(m6) #B = .06, SE = .06, p = .293

#Layering on Engagement and Agreement
m7 = lm(soccon ~ ng.agree + ng.engage + fb.freq + age + gender + poc + edu + inc + netsize + netdiv2, data=x)
m8 = lm(combel ~ ng.agree + ng.engage + fb.freq + age + gender + poc + edu + inc + netsize + netdiv2, data=x)
m9 = lm(soctrust ~ ng.agree + ng.engage + fb.freq + age + gender + poc + edu + inc + netsize + netdiv2, data=x)
m10 = lmer(sim ~ agree + engage.x + fb.freq + age + gender + poc + edu + inc + netsize + netdiv2 + (1 | ID), data=long, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
m11 = lmer(lik ~ agree + engage.x + fb.freq + age + gender + poc + edu + inc + netsize + netdiv2 + (1 | ID), data=long, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
m12 = lmer(clo ~ agree + engage.x + fb.freq + age + gender + poc + edu + inc + netsize + netdiv2 + (1 | ID), data=long, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))

summary(m7)
summary(m8)
summary(m9)
summary(m10, cor=F)
summary(m11, cor=F)
summary(m12, cor=F)

#Testing Interactions
i1 = lm(soccon ~ ng.agree*ng.engage + fb.freq + age + gender + poc + edu + inc + netsize + netdiv2, data=x)
i2 = lm(combel ~ ng.agree*ng.engage + fb.freq + age + gender + poc + edu + inc + netsize + netdiv2, data=x)
i3 = lm(soctrust ~ ng.agree*ng.engage + fb.freq + age + gender + poc + edu + inc + netsize + netdiv2, data=x)
i4 = lmer(sim ~ agree*engage.x + fb.freq + age + gender + poc + edu + inc + netsize + netdiv2 + (1 | ID), data=long, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
i5 = lmer(lik ~ agree*engage.x + fb.freq + age + gender + poc + edu + inc + netsize + netdiv2 + (1 | ID), data=long, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
i6 = lmer(clo ~ agree*engage.x + fb.freq + age + gender + poc + edu + inc + netsize + netdiv2 + (1 | ID), data=long, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))


summary(i1)
summary(i2)
summary(i3)
summary(i4, cor=F)
summary(i5, cor=F)
summary(i6, cor=F)

#Viz
visreg::visreg(i4, "engage.x", by="agree")
visreg::visreg(i5, "engage.x", by="agree")
visreg::visreg(i6, "engage.x", by="agree")


