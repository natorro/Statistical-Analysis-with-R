# Let's create a function that calculates the sigmas and the proportion of each normal 
# in the mixture of two gaussians centered at zero.

#mi_normal <- function(x, mu=0, sigma=1) {
#	value <- (1/sigma*sqrt(2*pi)) * exp((-1/2)*(((x-mu)/sigma)^2))
#}




#rate_of_returns <- (closing_values[2:length(closing_values)] - closing_values[1:(length(closing_values)-1)]) / closing_values[1:(length(closing_values)-1)]
#log_rate_of_returns <- log(closing_values[2:length(closing_values)] / closing_values[1:(length(closing_values)-1)])


#ipc_values <- em(rate_of_returns, c(mean(rate_of_returns), sd(rate_of_returns), 4*sd(rate_of_returns)))
#ipc_values 

# This line uses a density estimator, by default is a gaussian kernel
# and we try to adjust a normal with mean and sd from the data
#lines(density(log_rate_of_returns))
#desviacion <- sd(log_rate_of_returns)
#media <- mean(log_rate_of_returns)
#normalvalues <- rnorm(1000000, mean=media, sd=desviacion)
#lines(density(normalita), col="red")

# ------------ LA CURVA ESTIMADA DE LA MIXTURA ----------------------
#curve(ipc_values_EM$lambda[1] * dnorm(x, ipc_values_EM$mu[1], ipc_values_EM$sigma[1]) 
#      +
#      ipc_values_EM$lambda[2] * dnorm(x, ipc_values_EM$mu[2], ipc_values_EM$sigma[2]),
#      from=-0.2, to=0.2, add=TRUE, col="blue")


#plot(ipc_values_EM, density=TRUE, add=TRUE)

#Ecdf(log_rate_of_returns)
#plot(ecdf(log_rate_of_returns))
#mibinom <- rbinom(1000, 100, prob=0.1)
#Ecdf(mibinom)


Ecdf(log_rate_of_returns)
plot(ipc_values_EM, density=TRUE)
plot(rate_of_returns, type="l", main="^MXX", add=TRUE)
plot(log_rate_of_returns, type="l", main="^MXX", add=TRUE)
hist(rate_of_returns, main="^MXX", freq=FALSE)
hist(log_rate_of_returns, main="log rate ", freq=FALSE)
curve( (ipc_values$alfa/ipc_values$sigma_zero) * mi_normal(x/ipc_values$sigma_zero) + ((1-ipc_values$alfa)/ipc_values$sigma_one) * mi_normal(x/ipc_values$sigma_one))
plot(density(rate_of_returns), main="^MXX", col="red", add=TRUE)

#a <- rnorm(10000, 0, ipc_values$sigma_zero)
#b <- rnorm(10000, 0, ipc_values$sigma_one)
#d <- c(sample(a, ceiling(ipc_values$alfa*10000)), sample(b, floor(10000 - ipc_values$alfa*10000)))

#lines(density(d), col="purple")


summary(hola)
hola	
# Esto para hacer ECDF con grupos

#g <- c(rep(1, length(log_rate_of_returns)), rep(2, length(mynormal))) 
#Ecdf(c(log_rate_of_returns, mynormal), group=g, col=c('blue', 'orange'))

# Recreate a normal vector with the same number of elements as the log-returns
# using the same mean and sd from the data

mynormal <- rnorm(length(log_rate_of_returns), mean(log_rate_of_returns), sd(log_rate_of_returns))

# Generate random numbers from the mixture
mymixturedata <- rnormmix(length(log_rate_of_returns), lambda=ipc_values_EM$lambda, 
                          mu=ipc_values_EM$mu, sigma=ipc_values_EM$sigma)

mymixturedata2 <- rnormmix(length(log_rate_of_returns), lambda=c(values$alfa, 1-values$alfa), 
                          mu=c(0,0), sigma=c(values$sigma_zero, values$sigma_one))


# Graph both empirical data
# This is used to plot the empirical distribution of simulated data from the mixtures
#Ecdf(mymixturedata, lty=2, lwd=2, col="blue", add=TRUE)
#Ecdf(mymixturedata2, lty=2, lwd=2, col="red", add=TRUE)
# tests  to check if the data from upside is the same
ks.test(log_rate_of_returns, rnorm(10000, mean(log_rate_of_returns), sd(log_rate_of_returns)))
ks.test(log_rate_of_returns, mymixturedata)
ks.test(log_rate_of_returns, mymixturedata2)
ks.test(mymixturedata, mymixturedata2)

# Graph 05 
# The histogram with the two components of the mixture and the mixture this plot comes from the package
#plot(ipc_values_EM, which=2, breaks=25)
# This plots a empirical density
#lines(density(log_rate_of_returns), lty=2, lwd=2)
#curve(ipc_values_EM$lambda[1] * dnorm(x, ipc_values_EM$mu[1], ipc_values_EM$sigma[1]) +
#      ipc_values_EM$lambda[2] * dnorm(x, ipc_values_EM$mu[2], ipc_values_EM$sigma[2]), 
#      from=-0.10, to=0.07, add=TRUE, col="blue")

