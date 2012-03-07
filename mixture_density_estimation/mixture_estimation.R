library(fImport)
# We pull the data from yahoo finance for the symbol TELMEXL.MX from 2010-04-15 to 2011-09-27

#telmex <- yahooSeries("TELMEXL.MX", from="2010-04-15", to="2011-09-27")
#closing_values <- telmex$TELMEXL.MX.Close
# rate_of_returns <- (closing_values[2:length(closing_values)] - closing_values[1:(length(closing_values)-1)]) / closing_values[1:(length(closing_values)-1)]

#Use par in case of multiple graphs in one sheet
# par(mfrow=c(3, 1))
# plot(rate_of_returns, type="l")
# hist(rate_of_returns, breaks=100)
# plot(density(rate_of_returns))
#plot(log(rate_of_returns), type="l")

# Simulated data to test the calculate_mixture function, alpha=0.8, 1-alpha=0.2, sigmazero=0.2, sigmaone=1
a <- rnorm(1000, 0, 0.3)
b <- rnorm(1000, 0, 1)
d <- c(sample(a, 200), sample(b, 800))

plot(density(a), xlim=c(-2.5, 2.5), ylim=c(0, 2.3), col="red")
lines(density(b), xlim=c(-2.5, 2.5), ylim=c(0, 2.3), col="red")
lines(density(d), xlim=c(-2.5, 2.5), ylim=c(0, 2.3), col="blue")

calculate_mixture <- function(data, alpha, sigmazero, sigmaone, epsilon=0.001){
	n <- length(data)
	a <- c(1, 1, 1)
	
	while(all(a > epsilon)){
		p_omega_one_given_x <- ( alpha * dnorm(data, 0, sigmaone) ) / ( (alpha * dnorm(data, 0, sigmaone) ) + ( (1-alpha) * dnorm(data, 0, sigmaone) ) )
		p_omega_zero_given_x <- 1 - p_omega_one_given_x

		sum_of_ps <- sum(p_omega_one_given_x)
		sum_of_ps2 <- sum(p_omega_zero_given_x)

		w_is <- p_omega_zero_given_x/sum_of_ps2
		w_is_asterisk <- 1 - p_omega_zero_given_x/(n - sum_of_ps2)
		x_square <- data^2

		alpha_new <- (1/n)*sum_of_ps
		sigmazero_new <- sum(w_is * x_square)
		sigmaone_new <- sum(w_is_asterisk * x_square)

		a[1] <- abs((sigmazero_new - sigmazero)/sigmazero)
		a[2] <- abs((sigmaone_new - sigmaone)/sigmaone)
		a[3] <- abs((alpha_new - alpha) / alpha)

		sigmazero <- sigmazero_new
		sigmaone <- sigmaone_new
		alpha <- alpha_new
		
	}
		
	return(list(alpha=alpha, sigmazero=sigmazero, sigmaone=sigmaone))
	
}

calculate_mixture(d, 0.7, sd(d), sd(d)*4)
