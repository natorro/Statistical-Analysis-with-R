# We pull the data from yahoo finance for the symbol AMXL.MX from 2007-01-02 to 2011-12-30

tlevisacpo.mx <- yahooSeries("TLEVISACPO.MX", from="2000-01-02", to="2011-12-30")
closing_values <- tlevisacpo.mx$TLEVISACPO.MX.Close
rate_of_returns <- (closing_values[2:length(closing_values)] - closing_values[1:(length(closing_values)-1)]) / closing_values[1:(length(closing_values)-1)]
#rate_of_returns <- rate_of_returns[!(rate_of_returns > 2)]
#rate_of_returns <- rate_of_returns[!(rate_of_returns > 0.6)]

televisa_values <- em(rate_of_returns, c(mean(rate_of_returns), sd(rate_of_returns), 4*sd(rate_of_returns)))
televisa_values 


plot(rate_of_returns, type="l", main="Televisa", add=TRUE)
#plot(density(rate_of_returns), main="^MXX", col="red", add=TRUE)
hist(rate_of_returns, main="Televisa", freq=TRUE, breaks=100)
curve( (televisa_values$alfa/televisa_values$sigma_zero) * mi_normal(x/televisa_values$sigma_zero) + ((1-televisa_values$alfa)/televisa_values$sigma_one) * mi_normal(x/televisa_values$sigma_one), from=-0.1, to=0.1, add=TRUE)


# We pull the data from yahoo finance for the symbol CEMEXCPO.MX from 2007-01-02 to 2011-12-30

cemexcpo.mx <- yahooSeries("CEMEXCPO.MX", from="2000-01-02", to="2011-12-30")
closing_values <- cemexcpo.mx$CEMEXCPO.MX.Close
rate_of_returns <- (closing_values[2:length(closing_values)] - closing_values[1:(length(closing_values)-1)]) / closing_values[1:(length(closing_values)-1)]

#Use par in case of multiple graphs in one sheet
# par(mfrow=c(3, 1))
plot(rate_of_returns, type="l", main="CEMEX CPO")
plot(density(rate_of_returns), main="CEMEX CPO", col="red")
hist(rate_of_returns, breaks=1000, main="CEMEX CPO", freq=FALSE, add=TRUE)
#plot(log(rate_of_returns), type="l", main="CEMEX CPO")

cemexcpo.mx_values <- em(rate_of_returns, c(mean(rate_of_returns), sd(rate_of_returns), 4*sd(rate_of_returns)))
cemexcpo.mx_values

a <- rnorm(10000, 0, cemexcpo.mx_values$sigma_zero)
b <- rnorm(10000, 0, cemexcpo.mx_values$sigma_one)
d <- c(sample(a, ceiling(cemexcpo.mx_values$alfa*10000)), sample(b, floor(10000 - cemexcpo.mx_values$alfa*10000)))

lines(density(d), col="purple")

