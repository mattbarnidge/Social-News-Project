#Create ID var
d$ID = row.names(d)

#Filter
d$ng1.agree[d$name1 == 0] <- NA
d$ng2.agree[d$name2 == 0] <- NA
d$ng3.agree[d$name3 == 0] <- NA

d$ng1.sim[d$name1 == 0] <- NA
d$ng2.sim[d$name2 == 0] <- NA
d$ng3.sim[d$name3 == 0] <- NA

d$ng1.lik[d$name1 == 0] <- NA
d$ng2.lik[d$name2 == 0] <- NA
d$ng3.lik[d$name3 == 0] <- NA

d$ng1.clo[d$name1 == 0] <- NA
d$ng2.clo[d$name2 == 0] <- NA
d$ng3.clo[d$name3 == 0] <- NA

#Filter again and select variables
x <- subset(d, names > 0)
x.agree <- x %>% select(ID, ng1.agree, ng2.agree, ng3.agree)
x.agree <- x.agree %>% rename(ng1 = ng1.agree, ng2 = ng2.agree, ng3 = ng3.agree)
x.sim <- x %>% select(ID, ng1.sim, ng2.sim, ng3.sim)
x.sim <- x.sim %>% rename(ng1 = ng1.sim, ng2 = ng2.sim, ng3 = ng3.sim)
x.lik <- x %>% select(ID, ng1.lik, ng2.lik, ng3.lik)
x.lik <- x.lik %>% rename(ng1 = ng1.lik, ng2 = ng2.lik, ng3 = ng3.lik)
x.clo <- x %>% select(ID, ng1.clo, ng2.clo, ng3.clo)
x.clo <- x.clo %>% rename(ng1 = ng1.clo, ng2 = ng2.clo, ng3 = ng3.clo)

#Put data into long form
long.agree <- gather(x.agree, ng, agree, ng1:ng3, factor_key=TRUE)
long.sim <- gather(x.sim, ng, sim, ng1:ng3, factor_key=TRUE)
long.lik <- gather(x.lik, ng, lik, ng1:ng3, factor_key=TRUE)
long.clo <- gather(x.clo, ng, clo, ng1:ng3, factor_key=TRUE)
long.t1 <- merge(long.agree, long.sim, by=c("ID", "ng"))
long.t2 <- merge(long.t1, long.lik, by=c("ID", "ng"))
long <- merge(long.t2, long.clo, by=c("ID", "ng"))

#Clean up environment
rm(long.agree, long.clo, long.sim, long.lik, long.t1, long.t2)
rm(x, x.agree, x.clo, x.lik, x.sim)

#Correlation
with(long, (cor(agree, sim, use="complete.obs")))

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





