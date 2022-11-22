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

// The input data is a vector 'y' of length 'N'.
data {
  int<lower=1> N;                  //should lower= 0 or 1??
real Diameter..mm.[N]; //Diameter..mm. is the response variable
real<lower=477, upper=631> so[N]; //put fish length as predictor? Are lower and upper the min and max?
int<lower=1> J; // fish ID
int<lower=1> K; // fish origin, wild or hatch (this should be a factor)
int<lower=1, upper=J> subj[N]; //I guess this belongs here too?
int<lower=1, upper=K> item[N]; // Make sure I'm doing this right
//find paper with your written notes from office hours (probs at desk at work)
}


// The parameters accepted by the model. Our model
// accepts two parameters 'mu' and 'sigma'.
parameters {
  vector[2] beta; //fixed intercept and slope. // why is 2 here??. Oh, becasuse there are 2 betas?
  vector[J] u;  //ID intercepts. What is u?
  vector[K] w; //Wild or hatch intercepts. What is w?
  real<lower=0> sigma_e; // error sd
  real<lower=0> sigma_u; // subj sd
  real<lower=0> sigma_w; // item sd
  //so I think I use different errors for all of these. Check with Margaret.
}

// The model to be estimated. We model the output
// 'y' to be normally distributed with mean 'mu'
// and standard deviation 'sigma'.
model {
  real mu;
//priors
u ~ normal(0, sigma_u); //ID random effects
w ~ normal(0, sigma_w); // wild or hatch random effects (DO WE WANT RANDOM EFFECTS HERE? I'M NOT SURE IF WE DO.)
//I think w (wild or hatch) should just be a factor? HELP!
// likelihood
for (i in 1:N){
mu = beta[1] + u[Fish.ID[i]] + w[Wild.or.Hatch[i]] + beta[2] * Length..mm.[i];
    Diameter..mm.[i] ~ normal(mu, sigma_e);
  }
}

//draft model statement complete 11/22/22
//Questions: is this right? Should the Origin component not have a random intercept?
////It does right now, and I'm not sure that is right.

