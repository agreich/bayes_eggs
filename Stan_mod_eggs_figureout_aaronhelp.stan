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


//below is wrong. need to adjust data to say my data specific things
// The input data is a vector 'y' of length 'N'.
data {
  int<lower=1> N;                  //should lower= 0 or 1??
real Egg_diam[N]; //Diameter..mm. is the response variable
real Length[N]; //<lower=477, upper=631>//put fish length as predictor? Are lower and upper the min and max?
int<lower=1> J; // fish ID
int<lower=1> K; // fish origin, wild or hatch (this should be a factor)
int<lower=1, upper=J> Fish_ID_Index[N]; //N could be 60 here
int<lower=1, upper=K> Wild_or_Hatch_ID[N]; // N could be replaced with 2? Make sure I'm doing this right
//find paper with your written notes from office hours (probs at desk at work)
}


// The parameters accepted by the model. Our model
// accepts two parameters 'mu' and 'sigma'.
parameters {
  //vector[2] beta; //fixed intercept and slope. // why is 2 here??. Oh, becasuse there are 2 betas?
  real Beta_0;
  real Beta_1;
  vector[J] u;  //ID intercepts. What is u?
 // vector[K] w; //Wild or hatch intercepts. What is w?
  real<lower=0> sigma_e; // error sd.
  real<lower=0> sigma_u; // subj sd
//  real<lower=0> sigma_w; // item sd
  //SOOO WHERE ARE THESE SIGMA'S FROM?
}

// The model to be estimated. We model the output
// 'y' to be normally distributed with mean 'mu'
// and standard deviation 'sigma'.
model {
  real mu;
//priors
Beta_0 ~ normal(0,1000);
Beta_1 ~ normal(0,1000);
sigma_u ~ normal(0,100);
//sigma_w ~ normal(0,100);
sigma_e ~ normal(0,100);
u ~ normal(0, sigma_u); //ID random effects
//w ~ normal(0, sigma_w); // wild or hatch random effects (DO WE WANT RANDOM EFFECTS HERE? I'M NOT SURE IF WE DO.)
//I think w (wild or hatch) should just be a factor? HELP!
// likelihood
for (i in 1:N){
mu = Beta_0 + u[Fish_ID_Index[i]] + Wild_or_Hatch_ID[i] + Beta_1 * Length[i];
    Egg_diam[i] ~ normal(mu, sigma_e);
  }
}

