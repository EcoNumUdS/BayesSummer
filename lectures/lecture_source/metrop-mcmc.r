library(mcmc)


logposterior <- function(params, dat)
{
	if(params[2] <= 0) 
		return(-Inf)

	mu <- params[1]
	sig <- params[2]

	lp <- sum(dnorm(dat, mu, sig, log=TRUE)) + dnorm(mu, 16, 0.4, log = TRUE) + dgamma(sig, 1, 0.1, log = TRUE)
	return(lp)
}

X <- c(15, 19.59, 15.06, 15.71, 14.65, 21.4, 17.64, 18.31, 15.12, 14.40)
inits <- c(5, 2)
tuning <- c(1.5, 0.5)


model <- metrop(logposterior, initial = inits, nbatch = 10000, dat = X, scale = tuning)

model$accept
colnames(model$batch) = c('mu', 'sigma')
colMeans(model$batch)
library(bayesplot)
mcmc_dens(model$batch)
mcmc_intervals()
mcmc_trace(model$batch)