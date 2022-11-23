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


#11/22/22
###Nice. LEt's make a stan model
names(coho_eggs)
range(coho_eggs$Length..mm.)

#ok I made one, but may have included the fish origin as a random intercept (in addition to fish ID)
##and I think that is wrong, only fish ID should be a random intercept?

#anyway, let's try running this model.
##referencing "Mixed_model_stan" aka Sorensen et al. 2016 for example LMM rstan code
###listing 5
library(rstan)
# 1. Compile and fit model
stantest_1 <- stan(file="Stan_mod_eggs_figureout.stan", data = coho_eggs,
                          iter = 2000, chains = 4)
# posterior probability of beta 1 being less than 0:
beta1 <- unlist(extract(ranIntSlpNoCorFit, pars = "beta[2]"))
print(quantile(beta1, probs = c(0.025, 0.5, 0.975)))
mean(beta1 < 0)

#surprise surprise, we have an error
#I think it's because  "Fish.ID" is composed of characters, not numbers
##Fish origin is also composed of characters, not numbers. How to tell stan to count each character as a number?
##OR do I have to data wrangle?


###Well, I don;t know how to do this the stan way
##but I do know how to data wrangle.
##It's going to be a lot worse to re-do the indexing for the Fish.ID than for Wild.or.Hatch
###TO DO: re-do indexing for Wild.or.Hatch and Fish.ID to be numbers


#10/23/22
##Margaret has confirmed, I need to change my covariates into indexes
##easy for wild/hatch
##not so easy for fish ID
##I'll look into how I originally created that dataset

###Fish ID data wrangling
Fish_ID_Index <- c(1:55)
length(unique(coho_eggs$Fish.ID))
#Fish_ID_order <- order(unique(coho_eggs$Fish.ID))
Fish.ID <- as.character(unique(coho_eggs$Fish.ID))
#data.frame(Fish_ID, Fish_ID_order)
#can take the time to order, but I dont think I need to for my purposes today
##but would need to do this in excel, I think
#anyway, let's give each fish a number
#wild/hatch as 1 or 2. 1 if wild, 2 if hatch.

df_fishID <- data.frame(Fish.ID, Fish_ID_Index)
remove(Fish.ID)
#df_fishID_WH <- df_fishID %>% 
 # mutate(
  #  ifelse(Wild.or.Hatch == "wild", Wild_or_Hatch_ID = 1, Wild_or_Hatch_ID = 2))
##each fish Id had a number
library(dplyr)
coho_eggs_2 <- left_join(coho_eggs, df_fishID, by=c("Fish.ID"="Fish.ID"))
names(coho_eggs_2)

#add wild/hatch
coho_eggs_3 <- coho_eggs_2 %>%
  mutate(
  Wild_or_Hatch_ID = ifelse(Wild.or.Hatch == "wild", 1, 2)
    )
names(coho_eggs_3)
head(coho_eggs_3)

coho_eggs_4 <- coho_eggs_3 %>%
  mutate(
  Wild_or_Hatch_ID = as.integer(Wild_or_Hatch_ID)
  )

#coho_eggs_4

#coho_eggs_3 <- coho_eggs_3 %>% dplyr::select(Wild_or_Hatch_ID,)


#ok set up stan stuff

#now I can run rstan with the coho_eggs_3 dataset
stantest_2 <- stan(file="Stan_mod_eggs_figureout.stan", data = coho_eggs_4,
                   iter = 2000, chains = 4)

