//
// This Stan program defines a simple model, with a
// vector of values 'y' modeled as normally distributed
// with mean 'mu' and standard deviation 'sigma'.
//
// Learn more about model development with Stan at:
//
//    http://mc-stan.org/users/interfaces/rstan.html
//    https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started
//

//just the random intercept model
data {
  int<lower=1> N; //Number of samples (2000 something)
  int<lower=1> K; //number of wild/hatchery groups K (2)
  int<lower=1> J; //number of FISH ID's J (55)
  //int<lower=1,upper=K> origin[N]; //wild or hatch: coefficient per grouping level
  int<lower=1,upper=J> FishID[N]; // Fish_ID grouping levels
  vector[N] y; //I think this is the response variable, egg diameter
}
parameters {
  vector[J] eta; //Fish_ID grouping levels effects
  //vector[K] wh; // wild_hatch coeff effects
  real<lower=0> sigmaID; //corresponds to error between fish (the random error)
  real<lower=0> sigmaepsilon; //the within-fish error, I beleive (not random effect error)
}
transformed parameters {
  vector[J] bld; //actual group level (fishID) effects
  vector[N] mu; //the response variable

  bld = sigmaID * eta;

  for (i in 1:N)
    mu[i] = bld[FishID[i]];//part of likelihood

}
model {
  //priors
  eta ~ normal(0, 1); //try different priors later
  //wh ~ normal(0,1);
  sigmaID ~ normal (0,1);
  sigmaepsilon ~normal(0,1);

//likelihood
  y ~ normal(mu, sigmaepsilon); 
}

