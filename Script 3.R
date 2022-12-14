##script 2
###based off of this example: https://github.com/julianfaraway/rexamples/blob/main/mixed/penicillin.md
###I should also reference this one: https://github.com/julianfaraway/rexamples/blob/main/mixed/abrasion.md
##trying the linear mixed effect stan model via randomized block design

library(ggplot2)
library(rstan)
library(rstan)
library(faraway)
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)
coho_eggs_5 <- read.csv("coho_eggs_5.csv")

#plot the things
#ggplot(coho_eggs_5, aes(x=Wild.or.Hatch ,y=Diameter..mm., group=Fish.ID,color=Fish.ID))+geom_line()
hist(coho_eggs_5$Diameter..mm.)

#the frequentist way
REML_fit <- lmer(Diameter..mm. ~ Wild.or.Hatch +(1|ID), data=coho_eggs_5, REML = F)
sumary(REML_fit)
##parallel to tutorial: mmod <- lmer(yield ~ treat + (1|blend), penicillin)
ranef(REML_fit)$ID
anova(REML_fit)

#the bayesian way
##STAN TIME
set.seed(15432)
dat_stan_script_2 <- list(N=nrow(coho_eggs_5), 
                          K=length(unique(coho_eggs_5$Wild_or_Hatch_ID)), 
                          J=length(unique(coho_eggs_5$Fish_ID_Index)), 
                          origin=coho_eggs_5$Wild_or_Hatch_ID, 
                          FishID=coho_eggs_5$Fish_ID_Index, 
                          y=coho_eggs_5$Diameter..mm.)
Mod_stan_script_2 <- stan(file="Stan_for_script2.stan",
                                   data=dat_stan_script_2,
                                   iter=5000,
                                   chains=4)
saveRDS(Mod_stan_script_2, "Mod_stan_script2.RDS")
##graph the STAN
stan_trace(Mod_stan_script_2, pars = c("sigmaepsilon", "sigmaID"))
stan_trace(Mod_stan_script_2, pars = c("trt"))
stan_hist(Mod_stan_script_2, pars = c("sigmaepsilon", "sigmaID"))
stan_hist(Mod_stan_script_2, pars = c("trt"))

#STAN tables:
print(Mod_stan_script_2, pars=c("trt","sigmaID","sigmaepsilon","bld"))


#look like it works!! Yay! And the values make sense! Let's run this with 10,000 samples 
##for the project purposes!

#graphs needed:
##thinned traceplot
##density plot or histogram

##Q's for margaret office hours:
##0) I don't understand my model statement
##1) calculate posterior dist as part of 
##2) add more informative priors
##3)a way to make this calculation one-sided?

#12/11/22. Right nowjust stick with script for stan_
set.seed(15432)
dat_stan_script_2 <- list(N=nrow(coho_eggs_5), 
                          K=length(unique(coho_eggs_5$Wild_or_Hatch_ID)), 
                          J=length(unique(coho_eggs_5$Fish_ID_Index)), 
                          origin=coho_eggs_5$Wild_or_Hatch_ID, 
                          FishID=coho_eggs_5$Fish_ID_Index, 
                          y=coho_eggs_5$Diameter..mm.)
Mod_stan_script_2 <- stan(file="Script_for_stan_2_simplifed.stan",
                          data=dat_stan_script_2,
                          iter=5000,
                          chains=4)
print(Mod_stan_script_2, pars=c("wildhatch","sigmaID","sigmaepsilon","bld"))


#12/12/22 attemp corresponding to "Stan_4_basedon_brms.stan"
set.seed(15432)
dat_stan_script_4 <- list(N=nrow(coho_eggs_5), 
                          K=length(unique(coho_eggs_5$Wild_or_Hatch_ID)), 
                          J=length(unique(coho_eggs_5$Fish_ID_Index)), 
                          origin=coho_eggs_5$Wild_or_Hatch_ID, 
                          FishID=coho_eggs_5$Fish_ID_Index, 
                          y=coho_eggs_5$Diameter..mm.)
Mod_stan_script_4 <- stan(file="Stan_4_basedon_brms.stan",
                          data=dat_stan_script_4,
                          iter=5000,
                          chains=4)
print(Mod_stan_script_4, pars=c("wh","sigmaID","sigmaepsilon","bld"))
stan_trace(Mod_stan_script_4, pars = c("sigmaepsilon", "sigmaID"))
#Fuck yeah it works
#let's run with 10,000 samples!

Mod_stan_script_4_10000_fullmod <- stan(file="Stan_4_basedon_brms.stan",
                          data=dat_stan_script_4,
                          iter=10000,
                          chains=4)
print(Mod_stan_script_4_10000_fullmod, pars=c("wh","sigmaID","sigmaepsilon","bld"))
stan_trace(Mod_stan_script_4_10000_fullmod, pars = c("sigmaepsilon", "sigmaID"))#can I thin?
saveRDS(Mod_stan_script_4_10000_fullmod, "stan_full_egg_model.RDS")

summary(coho_eggs_5$Diameter..mm.)

#save that full mod

#now let's do the mod without the wild/hatch covariate. This will requrie building a new model (without wh)
dat_stan_script_4_nowh <- list(N=nrow(coho_eggs_5), 
                          K=length(unique(coho_eggs_5$Wild_or_Hatch_ID)), 
                          J=length(unique(coho_eggs_5$Fish_ID_Index)), 
                          #origin=coho_eggs_5$Wild_or_Hatch_ID, 
                          FishID=coho_eggs_5$Fish_ID_Index, 
                          y=coho_eggs_5$Diameter..mm.)
Mod_stan_script_4_10000_nowh <- stan(file="Stan_4_basedon_bmrs_nowh.stan",
                                        data=dat_stan_script_4_nowh,
                                        iter=10000,
                                        chains=4)
print(Mod_stan_script_4_10000_nowh, pars=c("wh","sigmaID","sigmaepsilon","bld"))

#compare using dic.samples or something
##waic would work

########################################
#let's do this with 10 as a prior
Mod_stan_script_4_10000_fullmod_10prior <- stan(file="Stan_4_basedon_brms_10.stan",
                                        data=dat_stan_script_4,
                                        iter=10000,
                                        chains=4)
print(Mod_stan_script_4_10000_fullmod_10prior, pars=c("wh","sigmaID","sigmaepsilon","bld"))
stan_trace(Mod_stan_script_4_10000_fullmod_10prior, pars = c("sigmaepsilon", "sigmaID"))#can I thin?
saveRDS(Mod_stan_script_4_10000_fullmod_10prior, "stan_full_egg_model_10.RDS")

summary(coho_eggs_5$Diameter..mm.)
###########################################
#let's do this with 100 as a prior
Mod_stan_script_4_10000_fullmod_100prior <- stan(file="Stan_4_basedon_brms_100.stan",
                                        data=dat_stan_script_4,
                                        iter=10000,
                                        chains=4)
print(Mod_stan_script_4_10000_fullmod_100prior, pars=c("wh","sigmaID","sigmaepsilon","bld"))
stan_trace(Mod_stan_script_4_10000_fullmod_100prior, pars = c("sigmaepsilon", "sigmaID"))#can I thin?
saveRDS(Mod_stan_script_4_10000_fullmod_100prior, "stan_full_egg_model_100.RDS")
##in this mod, stan gets sigma ID stuck at 0, is this right though?

plot(coho_eggs_5$Diameter..mm.)

#########################
##let's change the prior to 6
##more realistic
Mod_stan_script_5_10000_fullmod <- stan(file="Stan_4_basedon_brms_2.stan",
                                        data=dat_stan_script_4,
                                        iter=10000,
                                        chains=4)
print(Mod_stan_script_5_10000_fullmod, pars=c("wh","sigmaID","sigmaepsilon","bld"))
stan_trace(Mod_stan_script_5_10000_fullmod, pars = c("sigmaepsilon", "sigmaID"))#can I thin?
#saveRDS(Mod_stan_script_4_10000_fullmod, "stan_full_egg_model.RDS")
summary(Mod_stan_script_5_10000_fullmod)

summary(coho_eggs_5$Diameter..mm.)

###################
######
#KEEPER MODELS
#Mod_stan_script_fixedwh_fullmod <- stan(file="Stan_4_basedon_brms.stan",
 #                                       data=dat_stan_script_4,
  #                                      iter=10000,
   #                                     chains=4)
#print(Mod_stan_script_fixedwh_fullmod , pars=c("wh","sigmaID","sigmaepsilon","bld"))
#saveRDS(Mod_stan_script_fixedwh_fullmod, "Stanmodfinal_1.RDS")


dat_stan_script_4_nowh <- list(N=nrow(coho_eggs_5), 
                               K=length(unique(coho_eggs_5$Wild_or_Hatch_ID)), 
                               J=length(unique(coho_eggs_5$Fish_ID_Index)), 
                               #origin=coho_eggs_5$Wild_or_Hatch_ID, 
                               FishID=coho_eggs_5$Fish_ID_Index, 
                               y=coho_eggs_5$Diameter..mm.)
Mod_lesser <- stan(file="Stan_lesser.stan",
                   data=dat_stan_script_4_nowh,
                   iter=10000,
                   chains=4)

print(Mod_lesser , pars=c("sigmaID","sigmaepsilon","bld"))
#stan_trace(Mod_lesser, pars=c
#saveRDS(Mod_lesser,"Mod_lesser.RDS")

##let's compare models, with the sd=1 model
library(loo)
?loo::waic

#waic_1 <- get_waic(Mod_stan_script_fixedwh_fullmod)
#get_waic(Mod_stan_script_fixedwh_fullmod)
names(summary(Mod_stan_script_fixedwh_fullmod))
summary(Mod_stan_script_fixedwh_fullmod)$summary
#ok, probs just compare the sd one version, what i have so far because it's quitting time, as they say

extract_log_lik(Mod_stan_script_fixedwh_fullmod)
##FUCKthis log likelihood stuff
Mod_stan_script_fixedwh_fullmod_loglik <- stan(file="Stan_4_basedon_brms.stan",
                                        data=dat_stan_script_4,
                                        iter=10000,
                                        chains=4)
print(Mod_stan_script_fixedwh_fullmod_loglik , pars=c("wh","sigmaID","sigmaepsilon","bld"))
saveRDS(Mod_stan_script_fixedwh_fullmod_loglik, "Stanmodfinal_1_loglik.RDS")
model1_trace<-stan_trace(Mod_stan_script_fixedwh_fullmod_loglik , pars=c("wh","sigmaID","sigmaepsilon"))

model1_trace + xlab("Iteration") + ylab("Value")
#bayesplot sounds cool, but the download isn't working for me right now. I have an issue downloading packages taht I'm not dealing with right now
stan_hist(Mod_stan_script_fixedwh_fullmod_loglik, pars=c("wh","sigmaID","sigmaepsilon"))
stan_diag(Mod_stan_script_fixedwh_fullmod_loglik)
#stan_par(Mod_stan_script_fixedwh_fullmod_loglik, par=c("wh"))

#LOO TIME
?compare_models
?loo
loo1<- loo(Mod_stan_script_fixedwh_fullmod_loglik)
loo2 <- loo(Mod_lesser)
loo_compare(loo1,loo2) #model 2 is worse
#save this table output!!

# use the moment matching for loo with a stanfit object
#loo_mm <- loo(fit1, pars = "log_lik", moment_match = TRUE)
#print(loo_mm)

#log_lik_1=extract_log_lik(loo1_sample, merge_chains = F)
#log_lik_2=extract_log_lik(loo2_sample, merge_chains = F)
#r_eff_1=relative_eff(log_lik_1)
#r_eff_2=relative_eff(log_lik_2)

################3
#trash test
Mod_stan_script_5_5000_fullmod_100 <- stan(file="Stan_4_basedon_brms_2.stan",
                                        data=dat_stan_script_4,
                                        iter=5000,
                                        chains=4)
print(Mod_stan_script_5_5000_fullmod_100, pars=c("wh","sigmaID","sigmaepsilon","bld"))
#stan_trace(Mod_stan_script_5_10000_fullmod, pars = c("sigmaepsilon", "sigmaID"))#can I thin?
#saveRDS(Mod_stan_script_4_10000_fullmod, "stan_full_egg_model.RDS")
#summary(Mod_stan_script_5_10000_fullmod)

summary(coho_eggs_5$Diameter..mm.)

####trash trash
Mod_stan_script_fixedwh_fullmod <- stan(file="Stan_4_basedon_brms.stan",
                                           data=dat_stan_script_4,
                                           iter=5000,
                                           chains=4)
print(Mod_stan_script_fixedwh_fullmod , pars=c("wh","sigmaID","sigmaepsilon","bld"))
#stan_trace(Mod_stan_script_5_10000_fullmod, pars = c("sigmaepsilon", "sigmaID"))#can I thin?
#saveRDS(Mod_stan_script_4_10000_fullmod, "stan_full_egg_model.RDS")
#summary(Mod_stan_script_5_10000_fullmod)

summary(coho_eggs_5$Diameter..mm.)
