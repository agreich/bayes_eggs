#Alexandra Reich
#Bayes project: egg diameter
#last updated: 11/03/22


#####Proposal stuff######
#first, let's load some data. I'll use the cleaned dataset from my other analyses
coho_eggs <- read.csv("Data/coho_clean_for_641_project")
coho.clean <- coho_eggs

#Let's get some exploratory graphs for the project proposal
hist(coho_eggs$Diameter..mm., prob=TRUE, main="")
lines(density(coho_eggs$Diameter..mm.))

library(ggplot2)
ggplot(coho_eggs) + aes(x=Diameter..mm., fill=Wild.or.Hatch) +# geom_histogram() + 
  geom_density(alpha=0.5) + theme_bw() +
  scale_fill_manual(values=c("orange", "purple"))+
  labs(y="Density", x="Egg diameter (mm)")+
  theme(legend.title = element_blank())

ggplot(coho_eggs) + aes(x=Diameter..mm., y= Wild.or.Hatch, fill=Wild.or.Hatch) +# geom_histogram() + 
  geom_boxplot(alpha=0.5) + 
  #geom_jitter(alpha=0.9) +
  theme_bw() +
  scale_fill_manual(values=c("orange", "purple"))+
  theme(legend.title = element_blank())+
  labs(y="Fish origin", x="Egg diameter (mm)")


##I'll copy-paste the frequentist analysis 
##frequentist analysis below:
library(nlme)
library(lmerTest)
#conceptual model (mixed model with interaction effects too)
fit.c.A.INT <- lm(Diameter..mm. ~ Length..mm. + Wild.or.Hatch + Length..mm.:Wild.or.Hatch, data=coho.clean)

#reml to select mixed effects
fit.c.GLOBAL <- lmer(Diameter..mm. ~ Length..mm. + Wild.or.Hatch+Length..mm.:Wild.or.Hatch +(1|ID), data=coho.clean, REML=T)
#fit.c.B.INT <- lmer(Diameter..mm. ~ Length..mm. + Wild.or.Hatch + Length..mm.:Wild.or.Hatch +(1|ID), data=coho.clean, REML=T)
AIC(fit.c.GLOBAL, fit.c.A.INT) #mixed effect model wins!

#ml to select fixed effects
fit.c.GLOBAL.ml <- lmer(Diameter..mm. ~ Length..mm. + Wild.or.Hatch+Length..mm.:Wild.or.Hatch +(1|ID), data=coho.clean, REML=F)

fit.c.C <- lmer(Diameter..mm. ~ Length..mm. + Wild.or.Hatch +(1|ID), data=coho.clean, REML = F)

fit.c.C1 <- lmer(Diameter..mm. ~ Wild.or.Hatch +(1|ID), data=coho.clean, REML = F)
fit.c.C2 <- lmer(Diameter..mm. ~ 1 + (1|ID), data=coho.clean, REML = F)
fit.c.C3 <- lmer(Diameter..mm. ~ Length..mm. +(1|ID), data=coho.clean, REML = F)
AIC(fit.c.C, fit.c.C1, fit.c.C2, fit.c.C3, fit.c.GLOBAL.ml) #C1 is winner.



#model that was selected
##(and fit to model without intercept for the standard errors)
fit.c.C1.REML <- lmer(Diameter..mm. ~ Wild.or.Hatch +(1|ID), data=coho.clean, REML = T)
summary(fit.c.C1.REML)

fit.lme <- lme(fixed = Diameter..mm. ~ Wild.or.Hatch, random = ~1|ID, data=coho.clean, method="REML")
summary(fit.lme)

fit.lme.no.int <- lme(fixed = Diameter..mm. ~ Wild.or.Hatch-1, random = ~1|ID, data=coho.clean, method="REML")
summary(fit.lme.no.int)


#parameter estimates
ranef(fit.lme.no.int) #the random effects
fixef(fit.lme.no.int) #the means
summary(fit.lme)
summary(fit.lme.no.int)

#graph that
library(cowplot)
#graph that
coho.egg.graph <- ggplot(coho.clean) + aes(x=Length..mm., y=Diameter..mm., color=Wild.or.Hatch) +
    geom_point(alpha=0.5) + scale_color_manual(values=c("purple", "orange"), name=element_blank(), labels=c("Wild origin", "Hatchery origin"), breaks=c("wild", "hatchery"))+
    labs(y="Egg diameter (mm)", x="MEHP length (mm)")+
    theme_cowplot() +
    theme(legend.position = c(0.03, 0.94))+
    coord_cartesian(ylim=c(5,8.5)) +
    scale_y_continuous(breaks=c(5,6,7,8), expand=c(0,0))
