# This is just to make a simulation and try the results.

set.seed(34356)
a <- rnorm(1000, 0, 0.5)
b <- rnorm(1000, 0, 4)
d <- c(sample(a, 350), sample(b, 650))

plot(density(a), xlim=c(-2.5, 2.5), ylim=c(0, 2.3), col="green")
lines(density(b), xlim=c(-2.5, 2.5), ylim=c(0, 2.3), col="green")
lines(density(d), xlim=c(-2.5, 2.5), ylim=c(0, 2.3), col="blue")

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

valores <- em(d, c(mean(d), sd(d), 4*sd(d)))
valores 

a <- rnorm(1000, 0, valores[2])
b <- rnorm(1000, 0, valores[3])
d <- c(sample(a, floor(valores[1]*1000)), sample(b, floor(valores[1]*1000)))

lines(density(d), xlim=c(-2.5, 2.5), ylim=c(0, 2.3), col="red")
