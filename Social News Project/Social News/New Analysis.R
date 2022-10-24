#Load Data and Libraries
setwd("~/Documents/GitHub/Social-News-Project/Social News Project/Social News")

load("socnews.Rdata")
load("sub.Rdata")
load("long.Rdata")

library(lme4)
library(lmerTest)
library(ggplot2)
library(dotwhisker)
library(gridExtra)
library(sjPlot)

#Make CCE vars
x$cce <- x$fbnews.dis-x$fbnews.agr
long$cce <- abs(long$agree - 6) - 3

long$agr[long$agree > 3] <- 1
long$agr[long$agree == 3] <- 0
long$agr[long$agree < 3] <- -1

#RQ1: How much CCE in news?
summary(x$cce); sd(x$cce, na.rm=T)
summary(long$cce); sd(long$cce, na.rm=T)
table(long$agr)

t.test(x$cce, mu=0)
t.test(long$cce, mu=0)
prop.test(634, 998)

#Visualizations
p1 <- ggplot(x, aes(x = cce)) + 
  geom_histogram(colour = 1, fill = "white", bins = 5)

p2 <- ggplot(long, aes(x = cce)) + 
  geom_histogram(colour = 1, fill = "white", bins = 5)

#RQ2: Relationship with Y
m1 = lm(soccon ~ age + gender + poc + edu + inc + fb.freq + netsize + netdiv2 + cce, data=x)
m2 = lm(combel ~ age + gender + poc + edu + inc + fb.freq + netsize + netdiv2 + cce, data=x)
m3 = lm(soctrust ~ age + gender + poc + edu + inc + fb.freq + netsize + netdiv2 + cce, data=x)
m4 = lmer(sim ~ age + gender + poc + edu + inc + fb.freq + netsize + netdiv2 + cce + (1 | ID), data=long, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
m5 = lmer(lik ~ age + gender + poc + edu + inc + fb.freq + netsize + netdiv2 + cce + (1 | ID), data=long, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
m6 = lmer(clo ~ age + gender + poc + edu + inc + fb.freq + netsize + netdiv2 + cce + (1 | ID), data=long, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))

summary(m1)
summary(m2)
summary(m3)
summary(m4, cor=F)
summary(m5, cor=F)
summary(m6, cor=F)

#Visualizations
#Overlay Plot
dwplot(list(wm.fb, wm.wa), 
       show_intercept = TRUE, 
       vline = geom_vline(
         xintercept = 0, 
         colour = "grey60", 
         linetype = 2), 
       vars_order = c("fb.freq.c", "wa.freq.c", 
                      "age.c", "gen.c", "edu.c", "inc.c", "wmz.c", 
                      "pol.int.c", "liber.c", "eff.int.c", "news.c",
                      "sec.c", "trust.c", "eff.ext.c", 
                      "com.index.c", "pol.index.c", 
                      "com.g", "pol.g", "fb.g", "wa.g", 
                      "SD (Intercept)", 
                      "SD (Observations)"),
       model_order = c("Model 1", "Model 2")
) %>% 
  relabel_predictors(
    c(fb.freq.c = "Facebook Frequency", wa.freq.c = "WhatsApp Frequency",
      age.c = "Age", gen.c = "Gender", edu.c = "Education", inc.c = "Income", wmz.c = "Race",
      pol.int.c = "Political Interest", liber.c = "Liberalism", eff.int.c = "Internal Efficacy", news.c = "News use",
      sec.c = "Perceived Security", trust.c = "Trust in Institutions", eff.ext.c = "External Efficacy", 
      com.index.c = "Community Engagement", pol.index.c = "Political Participation", 
      com.g = "Contextual Community", pol.g = "Contextual Political", 
      fb.g = "Contextual Facebook", wa.g = "Contextual WhatsApp"
    )
  ) + 
  theme(legend.title = element_blank()) +
  scale_colour_grey(labels = c("WhatsApp", "Facebook"),
                    start = 0.25,
                    end = 0.50) + 
  xlab("Effect Size") + 
  ylab("Predictor")

#RQ3: Relationship with engagement?
with(x, cor(cce, engage, use="complete.obs")) #weak negative correlation
with(long, cor(cce, engage.x, use="complete.obs")) #moderate negative correlation

aggregate(long$engage.x, by=list(long$agr), mean)
fit = aov(engage.x ~ as.factor(agr), data=long)
summary(fit)
TukeyHSD(fit)

#Visualization
ggplot(long, aes(as.factor(agr), engage.x)) + 
  geom_boxplot()

#RQ4: Interaction with engagement?
i1 = lm(soccon ~ age + gender + poc + edu + inc + fb.freq + netsize + netdiv2 + cce*engage, data=x)
i2 = lm(combel ~ age + gender + poc + edu + inc + fb.freq + netsize + netdiv2 + cce*engage, data=x)
i3 = lm(soctrust ~ age + gender + poc + edu + inc + fb.freq + netsize + netdiv2 + cce*engage, data=x)
i4 = lmer(sim ~ age + gender + poc + edu + inc + fb.freq + netsize + netdiv2 + cce*engage.x + (1 | ID), data=long, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
i5 = lmer(lik ~ age + gender + poc + edu + inc + fb.freq + netsize + netdiv2 + cce*engage.x + (1 | ID), data=long, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
i6 = lmer(clo ~ age + gender + poc + edu + inc + fb.freq + netsize + netdiv2 + cce*engage.x + (1 | ID), data=long, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))

summary(i1)
summary(i2)
summary(i3)
summary(i4, cor=F)
summary(i5, cor=F)
summary(i6, cor=F)

#Visualization
vissreg::visreg(i3, "cce", by="engage")
vissreg::visreg(i4, "cce", by="engage.x")


