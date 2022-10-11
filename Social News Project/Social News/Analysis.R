#Load Data
setwd("~/Documents/GitHub/Social-News-Project/Social News Project/Social News")
load("socnews.Rdata")
load("long.Rdata")

#Mean Differences
with(subset(d, fb.acct==1), 
     cor(cbind(d$fb.freq, d$soccon, d$combel, 
               d$soctrust, d$commtrust, d$newstrust, d$newstrust2, 
               d$bustrust, d$govtrust, d$scitrust), 
         use="complete.obs", method="pearson"))

with(subset(d, fb.acct==1), 
     cor(cbind(d$fbnews, d$soccon, d$combel), 
         use="complete.obs", method="pearson"))

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

m4 = lmer(commtrust ~ agree + (1 + agree | ID), data=long, 
          control=lmerControl(optimizer="bobyqa",
                              optCtrl=list(maxfun=2e5)))
summary(m4)

