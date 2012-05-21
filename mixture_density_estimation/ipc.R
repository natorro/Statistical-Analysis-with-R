set.seed(100)
library(fImport)
library(nlme)
library(mixtools)
library(Hmisc)
library(tseries)
library(nortest)

# 5 years
ipc <- yahooSeries("^MXX", from="2007-05-18", to="2012-05-18")

names(ipc) <- c("MXX.Open", "MXX.High", "MXX.Low", "MXX.Close", "MXX.Volume", "MXX.Adj.Close")

closing_values <- ipc$MXX.Close

log_rate_of_returns <- log(closing_values[2:length(closing_values)] / closing_values[1:(length(closing_values)-1)])

length(log_rate_of_returns)

# TODO: Check the autocorrelation structure 
# ACF(as.ts(rate_of_returns))

ipc_values_EM <- normalmixEM(log_rate_of_returns, arbvar = TRUE, arbmean=FALSE, epsilon = 1e-03, mu=c(0, 0), sigma=c(sd(log_rate_of_returns), sd(log_rate_of_returns)+0.4))
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
hist(log_rate_of_returns, prob=TRUE, breaks=50, main=title,
     xlab="log-retornos", ylab="Densidad")
curve(dnorm(x, mymean, mydeviation), from=-0.2, to=0.2, add=TRUE, col="red")


# Recreate a normal vector with the same number of elements as the log-returns
# using the same mean and sd from the data

mynormal <- rnorm(length(log_rate_of_returns), mean(log_rate_of_returns), sd(log_rate_of_returns))

# Jarque Bera Normality Test
jarque.bera.test(log_rate_of_returns)

# Shapiro-Wilk Normality Test 
shapiro.test(log_rate_of_returns)

# Anderson-Darling Normality Test
ad.test(log_rate_of_returns)

# Kolmogorov-Smirnoff Goodness of Fit Test 
ks.test(log_rate_of_returns, mynormal)

# Graph 02
# Let's plot the qqplot supposing a normal distribution and a line 
qqnorm(log_rate_of_returns, type="p", main="q-q plot normal", 
       xlab="Cuantiles teóricos", ylab="Cuantiles muestrales")
qqline(log_rate_of_returns, col="red")

# Graph 03
# Let's plot the ecdf and ecdf of the normal distribution with same mean and sd from data
Ecdf(log_rate_of_returns, lty=1, lwd=2, col="green", main="Tasa de retornos")
Ecdf(mynormal, lty=2, lwd=2, col="red", add=TRUE)

# Generate random numbers from the mixture
mymixturedata <- rnormmix(length(log_rate_of_returns), lambda=ipc_values_EM$lambda, 
                          mu=ipc_values_EM$mu, sigma=ipc_values_EM$sigma)

Ecdf(log_rate_of_returns, lty=1, lwd=2, col="green", main="Tasa de retornos")
Ecdf(mymixturedata, lty=2, lwd=2, col="red", add=TRUE)

