# This is just to make a simulation and try the results.

set.seed(34356)
a <- rnorm(1000, 0, 3)
b <- rnorm(1000, 0, 10)
d <- c(sample(a, 350), sample(b, 650))

plot(density(a), col="green")
lines(density(b), col="green")
lines(density(d), col="blue")

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


valores <- em(d, c(mean(d), sd(d), 4*sd(d)))
valores 

a <- rnorm(1000, 0, valores$sigma_zero)
b <- rnorm(1000, 0, valores$sigma_one)
d <- c(sample(a, floor(valores$alfa*1000)), sample(b, floor(1000 - valores$alfa*1000)))

lines(density(d), col="red")
