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
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

# 1. Compile and fit model
#stantest_1 <- stan(file="Stan_mod_eggs_figureout_2.stan", data = coho_eggs,
        #                  iter = 2000, chains = 4)
# posterior probability of beta 1 being less than 0:
#beta1 <- unlist(extract(ranIntSlpNoCorFit, pars = "beta[2]"))
#print(quantile(beta1, probs = c(0.025, 0.5, 0.975)))
#mean(beta1 < 0)

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


#ok set up stan dat stuff
stanDat <- list(Fish_ID_Index = as.integer(coho_eggs_4$Fish_ID_Index), #change to be egg spec
                Wild_or_Hatch_ID = as.integer(coho_eggs_4$Wild_or_Hatch_ID),
                Egg_diam = coho_eggs_4$Diameter..mm.,
                Length = coho_eggs_4$Length..mm.,
                N = nrow(coho_eggs_4),
                J = nlevels(as.factor(coho_eggs_4$Fish_ID_Index)),
                K = nlevels(as.factor(coho_eggs_4$Wild_or_Hatch_ID)))


#now I can run rstan with the coho_eggs_3 dataset
stantest_2 <- stan(file="Stan_mod_eggs_figureout.stan", data = stanDat,
                   iter = 2000, chains = 4)
#saveRDS(stantest_2, "stantest_2.rds")

#successfully ran stan mod! Now let's look at what happened:
plot(stantest_2)
print(stantest_2, pars = c("beta", "sigma_e", "sigma_u", "sigma_w"),
      probs = c(0.025, 0.5, 0.975)) #werid...
#maybe lppk at how marg does results??
print(stantest_2, pars = c("beta", "u", "w", "sigma_e", "sigma_u", "sigma_w"),
      probs = c(0.025, 0.5, 0.975))
#how to interpret though?
summary(stantest_2)

#more plots
stan_trace(stantest_2, pars=c("beta", "sigma_e", "sigma_u", "sigma_w"))
##beta 0 is super wandering. Do for more iterations? Alow for a different prior??
##beta 1 is pretty wandery too. 

stan_hist(stantest_2)


#I think try running stantest 3 longer. try 10,000 iterations. Yes, it will take forever.
##oh. Any mayve center the eggs before running. And center the length too??
coho_eggs_5 <- coho_eggs_4 %>% 
  mutate(c_egg_diam = Diameter..mm. - mean(Diameter..mm.)) %>%
  mutate(c_length = Length..mm. - mean(Length..mm.))
names(coho_eggs_5)

write.csv(coho_eggs_5, "coho_eggs_5.csv")


stanDat_3 <- list(Fish_ID_Index = as.integer(coho_eggs_5$Fish_ID_Index), #change to be egg spec
                Wild_or_Hatch_ID = as.integer(coho_eggs_5$Wild_or_Hatch_ID),
                Egg_diam = coho_eggs_5$c_egg_diam,
                Length = coho_eggs_5$c_length,
                N = nrow(coho_eggs_5),
                J = nlevels(as.factor(coho_eggs_5$Fish_ID_Index)),
                K = nlevels(as.factor(coho_eggs_5$Wild_or_Hatch_ID)))
#stantest_3 <- stan(file="Stan_mod_eggs_figureout.stan", data = stanDat_3,
                   iter = 2000, chains = 4)



stantest_3 <- readRDS("stantest_3.rds")
class(stantest_3)

summary(stantest_3)
plot(stantest_3)
stan_trace(stantest_3)
stan_hist(stantest_3)



#12/06/22
##centered model with more iterations
stantest_4 <- stan(file="Stan_mod_eggs_figureout.stan", data = stanDat_3,
                   iter = 5000, chains = 4)
#saveRDS(stantest_4, "stantest_4.rds")
plot(stantest_4)
stan_trace(stantest_4)

#uncentered model with more interations


##what about making this one-sided?


stanDat_4.5 <- list(Fish_ID_Index = as.integer(coho_eggs_5$Fish_ID_Index), #change to be egg spec
                  Wild_or_Hatch_ID = as.integer(coho_eggs_5$Wild_or_Hatch_ID),
                  Egg_diam = coho_eggs_5$c_egg_diam,
                  Length = coho_eggs_5$c_length,
                  N = nrow(coho_eggs_5),
                  J = nlevels(as.factor(coho_eggs_5$Fish_ID_Index)),
                  K = nlevels(as.factor(coho_eggs_5$Wild_or_Hatch_ID)))
stantest_4.5 <- stan(file="Stan_mod_eggs_figureout_aaronhelp.stan", data = stanDat_4.5,
                   iter = 2000, chains = 4)

plot(stantest_4.5)
stan_trace(stantest_4.5, pars = c("Beta_0", "Beta_1", "sigma_e", "sigma_u"))
stan_hist(stantest_4.5, pars = c("Beta_0", "Beta_1", "sigma_e", "sigma_u"))
#saveRDS(stantest_4.5, "stantest_4.5.rds")
## add integers? 
##n_fish_ID

#not-centered:
stanDat_4.6 <- list(Fish_ID_Index = as.integer(coho_eggs_5$Fish_ID_Index), #change to be egg spec
                    Wild_or_Hatch_ID = as.integer(coho_eggs_5$Wild_or_Hatch_ID),
                    Egg_diam = coho_eggs_5$Diameter..mm.,
                    Length = coho_eggs_5$Length..mm.,
                    N = nrow(coho_eggs_5),
                    J = nlevels(as.factor(coho_eggs_5$Fish_ID_Index)),
                    K = nlevels(as.factor(coho_eggs_5$Wild_or_Hatch_ID)))
stantest_4.6 <- stan(file="Stan_mod_eggs_figureout_aaronhelp.stan", data = stanDat_4.5,
                     iter = 10000, chains = 4)

traceplot(stantest_4.6, pars=c("Beta_0", "Beta_1", "sigma_e", "sigma_u"))



##so many attempts.
##Try just the one random effect, without the factor of wild/hatch or the covariate first
##using the centered data
pulpdat_Alex <- list(N=nrow(coho_eggs_5),
                J=length(unique(coho_eggs_5$Fish_ID_Index)),
                response=coho_eggs_5$c_egg_diam,
                predictor=as.numeric(coho_eggs_5$Fish_ID_Index))
#run the model
Just_random_effect_model_c <- stan(file="Just_random_effect.stan",
                                   data=pulpdat_Alex,
                                   iter=5000,
                                   chains=4)
#suppressMessages(sm <- stan_model(stanc_ret = rt, verbose=FALSE))
#system.time(fit <- sampling(sm, data=pulpdat))
#saveRDS(Just_random_effect_model_c, "random_effects_moded.RDS")
Just_random_effect_model_c <- readRDS("random_effects_moded.RDS")
stan_trace(Just_random_effect_model_c, pars = c("mu", "sigmaepsilon", "sigmaalpha"))
#might need to thin
summary(Just_random_effect_model_c, pars = c("mu", "sigmaepsilon", "sigmaalpha"))

#pname <- "mu"
#muc <- rstan::extract(fit, pars=pname,  permuted=FALSE, inc_warmup=FALSE)
#mdf <- reshape2::melt(muc)
#mdf %>% dplyr::filter(iterations %% 100 == 0) %>% 
 # ggplot(aes(x=iterations,y=value,color=chains)) + geom_line() + ylab(mdf$parameters[1])

Just_random_effect_model_c_2 <- stan(file="Just_random_effect_2.stan",
                                   data=pulpdat_Alex,
                                   iter=5000,
                                   chains=4)
stan_trace(Just_random_effect_model_c_2, pars = c("mu", "sigmaepsilon", "sigmaalpha"))
#how to thin tho?
?stan_trace

summary(Just_random_effect_model_c_2, pars=c("mu", "sigmaalpha", "sigmaepsilon"))
##that is centered, which is why it is werid. MAybe don't center
##NOT CENTERED:
pulpdat_Alex <- list(N=nrow(coho_eggs_5),
                     J=length(unique(coho_eggs_5$Fish_ID_Index)),
                     response=coho_eggs_5$Diameter..mm.,
                     predictor=as.numeric(coho_eggs_5$Fish_ID_Index))
#run the model
Just_random_effect_model_3 <- stan(file="Just_random_effect.stan",
                                   data=pulpdat_Alex,
                                   iter=5000,
                                   chains=4)
#suppressMessages(sm <- stan_model(stanc_ret = rt, verbose=FALSE))
#system.time(fit <- sampling(sm, data=pulpdat))
#saveRDS(Just_random_effect_model_c, "random_effects_moded.RDS")
stan_trace(Just_random_effect_model_3, pars = c("mu", "sigmaepsilon", "sigmaalpha"))
stan_hist(Just_random_effect_model_3, pars = c("mu", "sigmaepsilon", "sigmaalpha"))
#might need to thin
summary(Just_random_effect_model_3, pars = c("mu", "sigmaepsilon", "sigmaalpha"))
saveRDS(Just_random_effect_model_3, "Just_random_effect_model_3.RDS")
##what are sigma alpha and epsilon interps?
###J is the number of coho id, which is 55
###response is the coho Egg diamters (the y-values)
### the predictor is the coho ID, which has a really long length


##diagnostics on Just_random_effect_model_3

print(Just_random_effect_model_3, pars=c("mu","sigmaalpha","sigmaepsilon","a"))
print(Just_random_effect_model_3, pars=c("mu","sigmaalpha","sigmaepsilon"))
#write.csv()

###now do a fixed effect stan model


##now do a linear mixed effect model


##BRMS experiment
#fit.c.C1 <- lmer(Diameter..mm. ~ Wild.or.Hatch +(1|ID), data=coho.clean, REML = F)
library(tidybayes)
#install.packages()
library(brms)

Stan_fit_random_only <- tidybrms(Diameter..mm. ~ 1 + (1|ID), data=coho.clean)
fit.c.C1_STAN <- brm(Diameter..mm. ~ Wild.or.Hatch +(1|ID), data=coho.clean, REML = F)


