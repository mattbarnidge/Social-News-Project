#Load Data and Libraries
setwd("~/Documents/GitHub/Social-News-Project/Social News Project/Social News")

load("socnews.Rdata")
load("sub.Rdata")
load("long.Rdata")

library(lme4)
library(lmerTest)
library(ggplot2)
library(dotwhisker)
library(grid)
library(gridExtra)
library(sjPlot)
library(dplyr)

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

#Visualization
p1 <- ggplot(x, aes(x = cce)) + 
  geom_histogram(aes(y = ..density..), 
                 colour = 1, fill = "white", bins = 5) + 
  geom_density(adjust=5) + 
  ylim(0,.5) +
  xlab("General Measure") + 
  ylab("Density")
p1 + geom_vline(aes(xintercept=mean(cce)),
                color="black", linetype="dashed", size=.5)

p2 <- ggplot(long, aes(x = cce)) + 
  geom_histogram(aes(y = ..density..), 
                 colour = 1, fill = "white", bins = 5) + 
  geom_density(adjust=5) + 
  ylim(0,.5) +
  xlab("Name Generator Measure") + 
  ylab("")
p2 + geom_vline(aes(xintercept=mean(cce, na.rm=TRUE)),
                color="black", linetype="dashed", size=.5)

grid.arrange(p1,p2, ncol=2, nrow=1, 
             top=textGrob("Density Histograms of Cross-Cutting Exposure Measures"))

#Second visualization
df <- data.frame(x = c("LME", "NEU", "CCE"), y = c(319, 161, 634), sd=c(17.86057, 12.68858, 25.17936)) 
df$x <- factor(df$x, levels = c("LME", "NEU", "CCE"))
p3 <- ggplot(data=df, aes(x=x, y=y)) + 
  geom_bar(stat="identity", width=.5) + 
  geom_errorbar(aes(ymin=y-sd,
                    ymax=y+sd),
                width=.2) + 
  xlab("Exposure Type") + 
  ylab("No. of Cases") 
p3 + coord_flip()

geom_errorbar(aes(ymin=value-standard_error,
                  ymax=value+standard_error),
              width=.2)

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
p4 <- dwplot(list(m1, m2, m3, m4, m5, m6), 
             show_intercept = TRUE, 
             vline = geom_vline(
               xintercept = 0, 
               colour = "grey60", 
               linetype = 2), 
             vars_order = c("age", "gender", "poc", "edu", "inc", 
                            "fb.freq", "netsize", "netdiv2", 
                            "cce", 
                            "SD (Intercept)"), 
             model_order = c("Model 1", "Model 2", "Model 3", 
                             "Model 4", "Model 5", "Model 6")
) %>% 
  relabel_predictors(
    c(age = "Age", gender = "Gender", poc = "Race", edu = "Education", inc = "Income",
      fb.freq = "Facebook Frequency", netsize = "Network Size", netdiv2 = "Network Diversity", 
      cce = "Cross-Cutting Exposure")
  ) 
p4 + labs(x = "Effect Size", colour = "Model")
p4 + theme_light() + 
  theme(legend.title = element_blank()) +
  scale_colour_grey(labels = c("Closeness", 
                               "Liking", 
                               "Perceived Similarity",
                               "Social Trust", 
                               "Community Belongingness", 
                               "Social Connectedness"), 
                    start = 0.2,
                    end = 0.8, 
                    aesthetics = "colour") + 
  xlab("Effect Size") 

#RQ3: Relationship with engagement?
with(x, cor(cce, engage, use="complete.obs")) #weak negative correlation
with(long, cor(cce, engage.x, use="complete.obs")) #moderate negative correlation

aggregate(long$engage.x, by=list(long$agr), mean)
fit = aov(engage.x ~ as.factor(agr), data=long)
summary(fit)
TukeyHSD(fit)

#Visualization
p5 <- ggplot(data=subset(long, !is.na(agr)), 
             aes(as.factor(agr), engage.x)) + 
  geom_boxplot(notch=TRUE, fill='#A4A4A4', color="black") + 
  labs(x="Exposure Type", y = "Engagement") + 
  scale_x_discrete(labels = c('CCE','NEU','LME'))
p5 + coord_flip()
p5 + stat_summary(fun=mean, geom="point", shape=23, size=4)

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

#Visualizations
x$engage <- cut(x$engage, 3, labels=c("Low", "Med", "High"))
i3.f = lm(soctrust ~ age + gender + poc + edu + inc + fb.freq + netsize + netdiv2 + cce*engage, data=x)
visreg::visreg(i3.f, "cce", by="engage", jitter=TRUE, line=list(col="black"), 
               xlab="Cross-Cutting Exposure", ylab="Social Trust")

long$engage.x <- cut(long$engage.x, 3, labels=c("Low", "Med", "High"))
i4.f = lmer(sim ~ age + gender + poc + edu + inc + fb.freq + netsize + netdiv2 + cce*engage.x + (1 | ID), data=long, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
visreg::visreg(i4.f, "cce", by="engage.x", jitter=TRUE, line=list(col="black"), 
               xlab="Cross-Cutting Exposure", ylab="Perceived Similarity")

