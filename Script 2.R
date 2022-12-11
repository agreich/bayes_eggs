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

