source("phenologyInference.R")

getOverlap = function(alpha_s, beta_s, alpha_d, beta_d) {


	f <- function(x) dstart_time(x, alpha_s=alpha_s, beta_s=beta_s)
	g <- function(x) dend_time(x, alpha_s=alpha_s, beta_s=beta_s, alpha_d=alpha_d, beta_d=beta_d)
	
	overlap <- integrate(function(x) pmin(f(x), g(x)), 0.00001, 0.9999)$value
	return(overlap)
	}

