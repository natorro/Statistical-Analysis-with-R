# Let's create a function that calculates the sigmas and the proportion of each normal 
# in the mixture of two gaussians centered at zero.

#mi_normal <- function(x, mu=0, sigma=1) {
#	value <- (1/sigma*sqrt(2*pi)) * exp((-1/2)*(((x-mu)/sigma)^2))
#}

#em <- function(datos, params)
#{
#  paramsP <- c(-9,-9,-9)
#  while(sum(paramsP-params)^2 > (0.001^2))
#  {
#    paramsP <- params
    #E
#    p=params[1] * dnorm(datos, sd=params[2])/
#      (params[1] * dnorm(datos, sd=params[2]) + 
#      (1 - params[1]) * dnorm(datos, sd=params[3]))
    #M
#    params[1] = mean(p)
#    params[2] = sqrt(sum(p * datos^2) / sum(p))
#    params[3] = sqrt(sum((1 - p) * datos^2) / sum(1 - p))
#  }
#  params_list <- list("alfa"=params[1], "sigma_zero"=params[2], "sigma_one"=params[3])
#  return(params_list)
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

