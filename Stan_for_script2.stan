data {
  int<lower=0> N; //Number of samples (2000 something)
  int<lower=0> K; //number of wild/hatchery groups K (2)
  int<lower=0> J; //number of FISH ID's J (55)
  int<lower=1,upper=K> origin[N]; //wild or hatch
  int<lower=1,upper=J> FishID[N]; // FishID
  vector[N] y; //I think this is the response variable, egg diameter
}
parameters {
  vector[J] eta; //what is this? Corresponds to fish ID
  vector[K] trt; // Dont know this either. Corresponds to Wild or Hatch
  real<lower=0> sigmaID; //corresponds to error between fish (the random error)
  real<lower=0> sigmaepsilon; //the within-fish error, I beleive (not random effect error)
}
transformed parameters {
  vector[J] bld; //corresponds to fish ID
  vector[N] yhat; //the response variable, I beleive

  bld = sigmaID * eta;

  for (i in 1:N)
    yhat[i] = trt[origin[i]]+bld[FishID[i]];//fitted value

}
model {
  eta ~ normal(0, 1); //is this too small for variance? Probably. Try with larger var
//trt can set prior here
//trt~ normal(5, 100)
//check if results are robust against different priors
  y ~ normal(yhat, sigmaepsilon); 
}
