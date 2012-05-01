library(fImport)

# Let's create a function that calculates the sigmas and the proportion of each normal 
# in the mixture of two gaussians centered at zero.
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
  return(params)
}

# PULL DATA FOR TELMEXL.MX, AMXL.MX AND CEMEXCPO.MX
# We pull data from yahoo finance for the symbol TELMEXL.MX from 2007-01-02 to 2011-12-30

ipc <- yahooSeries("^MXX", from="2007-01-02", to="2011-12-30")
names(ipc) <- c("MXX.Open", "MXX.High", "MXX.Low", "MXX.Close", "MXX.Volume", "MXX.Adj.Close")
closing_values <- ipc$MXX.Close
rate_of_returns <- (closing_values[2:length(closing_values)] - closing_values[1:(length(closing_values)-1)]) / closing_values[1:(length(closing_values)-1)]

plot(rate_of_returns, type="l", main="^MXX")
plot(density(rate_of_returns), main="^MXX", col="red")
hist(rate_of_returns, main="^MXX", freq=FALSE, add=TRUE, breaks=100)
#plot(log(rate_of_returns), type="l", main="^MXX")

valores <- em(rate_of_returns, c(mean(rate_of_returns), sd(rate_of_returns), 4*sd(rate_of_returns)))
valores 


# We pull the data from yahoo finance for the symbol AMXL.MX from 2010-04-15 to 2011-09-27

amxl.mx <- yahooSeries("AMXL.MX", from="2007-01-02", to="2011-12-30")
closing_values <- amxl.mx$AMXL.MX.Close
rate_of_returns <- (closing_values[2:length(closing_values)] - closing_values[1:(length(closing_values)-1)]) / closing_values[1:(length(closing_values)-1)]
rate_of_returns <- rate_of_returns[!(rate_of_returns > 2)]

plot(rate_of_returns, type="l", main="AMXL MX")
plot(density(rate_of_returns), main="AMXL MX", col="red")
hist(rate_of_returns, breaks=100, main="AMXL MX", freq=FALSE, add=TRUE)
#plot(log(rate_of_returns), type="l", main="AMXL MX")

valores <- em(rate_of_returns, c(mean(rate_of_returns), sd(rate_of_returns), 4*sd(rate_of_returns)))
valores 


# We pull the data from yahoo finance for the symbol CEMEXCPO.MX from 2010-04-15 to 2011-09-27

cemexcpo.mx <- yahooSeries("CEMEXCPO.MX", from="2007-01-02", to="2011-12-30")
closing_values <- cemexcpo.mx$CEMEXCPO.MX.Close
rate_of_returns <- (closing_values[2:length(closing_values)] - closing_values[1:(length(closing_values)-1)]) / closing_values[1:(length(closing_values)-1)]

#Use par in case of multiple graphs in one sheet
# par(mfrow=c(3, 1))
plot(rate_of_returns, type="l", main="CEMEX CPO")
plot(density(rate_of_returns), main="CEMEX CPO", col="red")
hist(rate_of_returns, breaks=100, main="CEMEX CPO", freq=FALSE, add=TRUE)
#plot(log(rate_of_returns), type="l", main="CEMEX CPO")

valores <- em(rate_of_returns, c(mean(rate_of_returns), sd(rate_of_returns), 4*sd(rate_of_returns)))
valores 

