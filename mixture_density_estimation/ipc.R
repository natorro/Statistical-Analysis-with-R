set.seed(100)
library(fImport)
library(nlme)
library(mixtools)
library(Hmisc)
library(tseries)
library(nortest)

# The EM function that assumes that both normal densities are centered on 0
em <- function(datos, params)
{
  paramsP <- c(-9,-9,-9)
  while(sum(paramsP-params)^2 > (0.001^2))
  {
    paramsP <- params
    #E
    p=params[1] * dnorm(datos, sd=params[2])/
      (params[1] * dnorm(datos, sd=params[2]) + 
      (1 - params[1]) * dnorm(datos, sd=params[3]))
    #M
    params[1] = mean(p)
    params[2] = sqrt(sum(p * datos^2) / sum(p))
    params[3] = sqrt(sum((1 - p) * datos^2) / sum(1 - p))
  }
  params_list <- list("alfa"=params[1], "sigma_zero"=params[2], "sigma_one"=params[3])
  return(params_list)
}

# 5 years
ipc <- yahooSeries("^MXX", from="2007-05-21", to="2012-05-21")

names(ipc) <- c("MXX.Open", "MXX.High", "MXX.Low", "MXX.Close", "MXX.Volume", "MXX.Adj.Close")

closing_values <- ipc$MXX.Close

log_rate_of_returns <- log(closing_values[2:length(closing_values)] / closing_values[1:(length(closing_values)-1)])

length(log_rate_of_returns)

# TODO: Check the autocorrelation structure 
# ACF(as.ts(rate_of_returns))

# Calculate the values for the model
ipc_values_EM <- normalmixEM(log_rate_of_returns, arbvar = TRUE, arbmean=FALSE, epsilon = 1e-03, mu=c(0,0), sigma=c(sd(log_rate_of_returns), sd(log_rate_of_returns)+0.4))
ipc_values_EM2 <- normalmixEM2comp(log_rate_of_returns, lambda=0.5, mu=c(mean(log_rate_of_returns), mean(log_rate_of_returns)), sigsqrd=c(sd(log_rate_of_returns), sd(log_rate_of_returns)+4))

summary(log_rate_of_returns)

summary(ipc_values_EM)
summary(ipc_values_EM2)

# Calculate values of mean and standard deviation
mydeviation <- sd(log_rate_of_returns)
mymean <- mean(log_rate_of_returns)

# Graph 01
# Plot the histogram with the curve of the normal distribution using the mean and standar deviation from the data
title <- paste("Histograma de log-retornos y normal con \nmedia= ", mymean, "\ndesviación estándar= ", mydeviation)
hist(log_rate_of_returns, prob=TRUE, breaks=25, main=title,
     xlab="log-retornos", ylab="Densidad")
curve(dnorm(x, mymean, mydeviation), from=-0.2, to=0.2, add=TRUE, col="red")

# Jarque Bera Normality Test
jarque.bera.test(log_rate_of_returns)

# Shapiro-Wilk Normality Test 
shapiro.test(log_rate_of_returns)

# Anderson-Darling Normality Test
ad.test(log_rate_of_returns)

# Kolmogorov-Smirnoff Goodness of Fit Test 
ks.test(log_rate_of_returns, pnorm)

# Graph 02
# Let's plot the qqplot supposing a normal distribution and a line 
qqnorm(log_rate_of_returns, type="p", main="q-q plot normal", 
       xlab="Cuantiles teóricos", ylab="Cuantiles muestrales")
qqline(log_rate_of_returns, col="red")

# Graph 03
# Let's plot the ecdf and ecdf of the normal distribution with same mean and sd from data
Ecdf(log_rate_of_returns, lty=1, lwd=2, col="blue", main="Tasa de retornos")
curve(pnorm(x, mean(log_rate_of_returns), sd(log_rate_of_returns)),
      from=-0.10, to=0.07, add=TRUE, col="red")


# This is done only if I need to plot the ecdf from the simulated sample
#Ecdf(mynormal, lty=2, lwd=2, col="red", add=TRUE)

# Calculate values for EM assuming centered in zero
values <- em(log_rate_of_returns, 
             c(0.5, 
               sd(log_rate_of_returns), 
               4*sd(log_rate_of_returns)))

values

# Graph 04
Ecdf(log_rate_of_returns, lty=1, lwd=2, col="blue", main="Tasa de retornos")

curve(values$alfa * pnorm(x, 0, values$sigma_zero) +
      (1-values$alfa) * pnorm(x, 0, values$sigma_one),
      from=-0.10, to=0.07, add=TRUE, col="red")

curve(ipc_values_EM$lambda[1] * pnorm(x, ipc_values_EM$mu[1], ipc_values_EM$sigma[1]) 
  + ipc_values_EM$lambda[2] * pnorm(x, ipc_values_EM$mu[2], ipc_values_EM$sigma[2]), 
      from=-0.10, to=0.07, add=TRUE, col="green")



# Graph 05 
title <- paste("Histograma de log-retornos y  \ndos componentes de la mezcla y la mezcla")
hist(log_rate_of_returns, prob=TRUE, breaks=25, main=title,
     xlab="log-retornos", ylab="Densidad", ylim=c(0,45))
curve(ipc_values_EM$lambda[1]*dnorm(x, ipc_values_EM$mu[1], ipc_values_EM$sigma[1]), 
      from=-0.10, to=0.07, add=TRUE, col="blue", ylim=c(0,45))
curve(ipc_values_EM$lambda[2]*dnorm(x, ipc_values_EM$mu[2], ipc_values_EM$sigma[2]), 
      from=-0.10, to=0.07, add=TRUE, col="blue", ylim=c(0,45))
curve(ipc_values_EM$lambda[1] * dnorm(x, ipc_values_EM$mu[1], ipc_values_EM$sigma[1]) +
      ipc_values_EM$lambda[2] * dnorm(x, ipc_values_EM$mu[2], ipc_values_EM$sigma[2]), 
      from=-0.10, to=0.07, add=TRUE, col="red", ylim=c(0,45))

#Graph 06 
title <- paste("Histograma de log-retornos y  \ndos componentes de la mezcla\ny la mezcla con mu=0")
hist(log_rate_of_returns, prob=TRUE, breaks=25, main=title,
     xlab="log-retornos", ylab="Densidad", ylim=c(0,45))

curve(values$alfa*dnorm(x, 0, values$sigma_zero), 
      from=-0.10, to=0.07, add=TRUE, col="blue", ylim=c(0,45))

curve((1-values$alfa)*dnorm(x, 0, values$sigma_one), 
      from=-0.10, to=0.07, add=TRUE, col="blue", ylim=c(0,45))

curve(values$alfa * dnorm(x, 0, values$sigma_zero) +
      (1-values$alfa) * dnorm(x, 0, values$sigma_one),
      from=-0.10, to=0.07, add=TRUE, col="red", ylim=c(0,45))

# Hypothesis testing for this
pmnorm <- function(x, mu, sigma, alfa) {
  alfa[1]*pnorm(x,mu[1],sigma[1]) + (1-alfa[1])*pnorm(x,mu[2],sigma[2])
}

ks.test(log_rate_of_returns, pmnorm, ipc_values_EM$mu, ipc_values_EM$sigma, ipc_values_EM$lambda)
ks.test(log_rate_of_returns, pmnorm, c(0, 0), c(values$sigma_zero, values$sigma_one), c(values$alfa, 1-values$alfa))


