##Alex experiments with STAN##

library("rstan") 
#options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE) #use this or no?
  

#stan github example
schools_dat <- list(J = 8, 
                    y = c(28,  8, -3,  7, -1,  1, 18, 12),
                    sigma = c(15, 10, 16, 11,  9, 11, 10, 18))

fit <- stan(file = 'Stan_mod_1.stan', data = schools_dat)



print(fit)
plot(fit)
pairs(fit, pars = c("mu", "tau", "lp__"))

la <- extract(fit, permuted = TRUE) # return a list of arrays 
mu <- la$mu 

### return an array of three dimensions: iterations, chains, parameters 
a <- extract(fit, permuted = FALSE) 

### use S3 functions on stanfit objects
a2 <- as.array(fit)
m <- as.matrix(fit)
d <- as.data.frame(fit)

#######################3
#Margaret example from packet 30
summary(cars)

xNew <- 4.5
N <- nrow(mtcars);N

mydata <- list(y=mtcars$mpg, x= mtcars$wt, N=N, xNew=xNew)
myinits <- list(beta0 =1.0, beta1=2.0)
myfit <- stan(file="Marg.stan.packet30.stan",
              data=mydata,
              init="random",
              seed=1234543,
              thin=1,
              iter=5000,
              chains=3
              )

print(myfit)
plot(myfit)

