mi_normal <- function(x, mu=0, sigma=1) {
	value <- (1/sigma*sqrt(2*pi)) * exp((-1/2)*(((x-mu)/sigma)^2))
}

curve(mi_normal, from=-3.5, to=3.5)