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
  int<lower=0> N;
  real y[N];
  real x[N];
  real xNew; //for predicting a new obs
}

// The parameters accepted by the model. Our model
// accepts two parameters 'mu' and 'sigma'.
parameters {
  real beta0;
  real beta1;
  real<lower=0, upper=5> sigma;
  real yNew;
}

// The model to be estimated. We model the output
// 'y' to be normally distributed with mean 'mu'
// and standard deviation 'sigma'.
model {
  for (i in 1:N)
   y[i] ~ normal(beta0+beta1*x[i], sigma);
   
  yNew ~ normal( beta0+beta1*xNew, sigma);
}

generated quantities{
  real yNew2;
  yNew2=normal_rng(beta0+beta1*xNew, sigma);
}

