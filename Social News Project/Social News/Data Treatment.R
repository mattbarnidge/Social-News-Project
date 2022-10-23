#Load Data
setwd("~/Documents/GitHub/Social-News-Project/Social News Project/Social News")
load("socnews.Rdata")

#Create tie strength variable
d$ng1.st <- NA
d$ng1.st = ifelse(d$ng1.fam == 1 | d$ng1.fri == 1, 1, d$ng1.st)
d$ng1.st = ifelse(d$ng1.cow == 1 | d$ng1.acq == 1 | d$ng1.nei == 1, 0, d$ng1.st)

d$ng2.st <- NA
d$ng2.st = ifelse(d$ng2.fam == 1 | d$ng2.fri == 1, 1, d$ng2.st)
d$ng2.st = ifelse(d$ng2.cow == 1 | d$ng2.acq == 1 | d$ng2.nei == 1, 0, d$ng2.st)

d$ng3.st <- NA
d$ng3.st = ifelse(d$ng3.fam == 1 | d$ng3.fri == 1, 1, d$ng3.st)
d$ng3.st = ifelse(d$ng3.cow == 1 | d$ng3.acq == 1 | d$ng3.nei == 1, 0, d$ng3.st)

#Create ID var
d$ID = row.names(d)

#Filter
d$ng1.st[d$name1 == 0] <- NA
d$ng2.st[d$name2 == 0] <- NA
d$ng3.st[d$name3 == 0] <- NA

d$ng1.engage[d$name1 == 0] <- NA
d$ng2.engage[d$name2 == 0] <- NA
d$ng3.engage[d$name3 == 0] <- NA

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

#Filter again 
x <- subset(d, names > 0)

#Create dataframe for non RM vars
reg.vars <- x %>% select(age, gender, poc, edu, inc, fb.freq, 
                         netsize, newssize, netdiv1, netdiv2, netdiv3, 
                         fbnews, fbnews.dis, fbnews.agr, follow, nfbnews, engage, avoid, unfriend, 
                         fbtalk.freq, fbtalk.size, fbtalk.dis, fbtalk.agr, express, 
                         eff.i, eff.e, int, ideo, pid, know, 
                         soccon, combel, soctrust, commtrust, newstrust, newstrust2, 
                         govtrust, bustrust, scitrust)

#Create dataframes for RM vars
x.st <- x %>% select(ID, ng1.st, ng2.st, ng3.st)
x.st <- x.st %>% rename(ng1 = ng1.st, ng2 = ng2.st, ng3 = ng3.st)

x.engage <- x %>% select(ID, ng1.engage, ng2.engage, ng3.engage)
x.engage <- x.engage %>% rename(ng1 = ng1.engage, ng2 = ng2.engage, ng3 = ng3.engage)

x.agree <- x %>% select(ID, ng1.agree, ng2.agree, ng3.agree)
x.agree <- x.agree %>% rename(ng1 = ng1.agree, ng2 = ng2.agree, ng3 = ng3.agree)

x.sim <- x %>% select(ID, ng1.sim, ng2.sim, ng3.sim)
x.sim <- x.sim %>% rename(ng1 = ng1.sim, ng2 = ng2.sim, ng3 = ng3.sim)

x.lik <- x %>% select(ID, ng1.lik, ng2.lik, ng3.lik)
x.lik <- x.lik %>% rename(ng1 = ng1.lik, ng2 = ng2.lik, ng3 = ng3.lik)

x.clo <- x %>% select(ID, ng1.clo, ng2.clo, ng3.clo)
x.clo <- x.clo %>% rename(ng1 = ng1.clo, ng2 = ng2.clo, ng3 = ng3.clo)

#Cbind reg vars onto last dataframe
x.clo.plus <- cbind(x.clo, reg.vars)

#Put data into long form
long.st <- gather(x.st, ng, st, ng1:ng3, factor_key = TRUE)
long.engage <- gather(x.engage, ng, engage, ng1:ng3, factor_key = TRUE)
long.agree <- gather(x.agree, ng, agree, ng1:ng3, factor_key=TRUE)
long.sim <- gather(x.sim, ng, sim, ng1:ng3, factor_key=TRUE)
long.lik <- gather(x.lik, ng, lik, ng1:ng3, factor_key=TRUE)
long.clo.plus <- gather(x.clo.plus, ng, clo, ng1:ng3, factor_key=TRUE)

#Clean up
rm(reg.vars, x.agree, x.clo, x.clo.plus, x.engage, x.lik, x.sim, x.st)

#Merge
long.t1 <- merge(long.st, long.engage, by=c("ID", "ng"))
long.t2 <- merge(long.t1, long.agree, by=c("ID", "ng"))
long.t3 <- merge(long.t2, long.sim, by=c("ID", "ng"))
long.t4 <- merge(long.t3, long.lik, by=c("ID", "ng"))
long <- merge(long.t4, long.clo.plus, by=c("ID", "ng"))

#Clean up environment
rm(long.agree, long.clo.plus, long.engage, long.sim, long.lik, long.st, 
   long.t1, long.t2, long.t3, long.t4)

#Correlation
with(long, (cor(cbind(st, engage.x, agree, sim, lik, clo), 
                use="complete.obs")))

#Save Data
save(x, file="sub.Rdata")
save(long, file="long.Rdata")
rm(long, x, d)







