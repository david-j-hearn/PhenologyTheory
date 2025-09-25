source("helperFunctions.R")

#_____________________________________For GP
dstart_time_GP = function(t, mu_O, sigma) {
	return(dnorm(t, mu_O, sigma) )
}

pstart_time_GP = function(q, mu_O, sigma) {
	return(pnorm(q, mu_O, sigma) )
}

dfirst_start_time_GP = function(t, N, mu_O, sigma) {
  N * (1 - pstart_time_GP(t, mu_O, sigma))^(N - 1) * dstart_time_GP(t, mu_O, sigma)
}

qfirst_start_time_GP = function(p, N, mu_O, sigma) {
	q_fst = qnorm(1 - (1-p)^(1/N), mu_O, sigma)
	return(q_fst)
}


dend_time_GP = function(t, mu_C, sigma) {
	return(dnorm(t, mu_C, sigma))
}

pend_time_GP = function(q, mu_C, sigma) {
	return(pnorm(q, mu_C, sigma))
}

dlast_end_time_GP = function(t, N, mu_C, sigma) {
  N * (pend_time_GP(t,mu_C, sigma))^(N - 1) * dend_time_GP(t,mu_C, sigma)
}

qlast_end_time_GP = function(p, N, mu_C, sigma) {
	return(qnorm(p^(1 / N), mean = mu_C, sd = sigma))
}

drange_GP = function(r, N, mu_O, mu_C, sigma, min=0, max=365) {

return(sapply(r, function(Tr) {
  integrate(function(y) dlast_end_time_GP(Tr + y, N, mu_C, sigma) * dfirst_start_time_GP(y,N,mu_O, sigma), lower = min, upper = max-Tr)$value
}))

}

dobserved_GP = function(x, mean_s, mean_e, sigma, max=1, min=0, normalize=T) {
	if(max<=min) {
		stop("The maximum response time must be greater than the minimum response time.")
	}
	if(mean_e<=mean_s) {
		stop("The mean start time must be before the mean end time.")
	}

	if(mean_e > max) {
		stop("The mean end time must be earlier than the maximum response time.")
		}

	if(mean_s < min) {
		stop("The mean start time must be later than the minimum response time.")
		}
	#mean_s = (mean_s-min)/(max-min)
	#mean_e = (mean_e-min)/(max-min)
	#sigma = sigma/(max-min)
	#x = (x-min) / (max-min)
	prob = pnorm(x,mean_s,sigma)*(1-pnorm(x,mean_e,sigma))
	if(normalize) { 
		integrand <- function(x) {
			pnorm(x, mean_s, sigma) * (1 - pnorm(x, mean_e, sigma))
		}
		nc = integrate(integrand, lower = min, upper = max)$value
		#print(paste(nc, " ", mean_e - mean_s))
	}
	else {
		nc = 1
	}
	return(prob / nc)
}

#provides the pmf of the proportion of individuals in a population that are in the phenophase at a give time, t
dproportionInPhenophase_GP = function(x, t, mean_s, mean_e, sigma, minT=0, maxT=365) {
	if(sum(ifelse(x<0,1,0))>0) {
		stop("The input proportion of the population in the phenophase, x, should be above 0.")
		}
	if(sum(ifelse(x>1,1,0))>0) {
		stop("The input proportion of the population in the phenophase, x, should be below 1.")
		}

	if(t<minT || t>maxT) {
		stop("The provided time must be in the input range of times specified by minT and maxT.")
	}

	t = (t-minT) / (maxT - minT)


	N = 1000
	n = round(x * 1000)

	Pt = dobserved_GP(t, mean_s, mean_e, sigma, min=minT, max=maxT, normalize=F)
	return( dbinom(x=n, size=N, prob=Pt) )

	}


#____________________________________

dstart_time = function(t, alpha_s, beta_s) {
        return(dbeta_safe(t, alpha_s, beta_s) )
}

pstart_time = function(q, alpha_s, beta_s) {
        return(pbeta_safe(q, alpha_s, beta_s) )
}

dfirst_start_time = function(t, N, alpha_s, beta_s) {
  N * (1 - pstart_time(t, alpha_s, beta_s))^(N - 1) * dstart_time(t, alpha_s, beta_s)
}

qfirst_start_time = function(p, N, alpha_s, beta_s) {
        q_fst = qbeta(1 - (1-p)^(1/N), alpha_s, beta_s)
        return(q_fst)
}

dlast_end_time = function(t, N, alpha_s, beta_s, alpha_d, beta_d) {
  N * (pend_time(t,alpha_s,beta_s,alpha_d,beta_d))^(N - 1) * dend_time(t,alpha_s,beta_s,alpha_d,beta_d)
}


qlast_end_time = function(p, N, alpha_s, beta_s, alpha_d, beta_d) {
        qfunc = make_quantile_function_from_pdf(ddist= function(x){ dlast_end_time(x,N,alpha_s,beta_s,alpha_d,beta_d) }, support = c(0, 1), n = 100)
        return(qfunc(p))
}

#the normalization is not correct!
drange = function(r, N, alpha_s, beta_s, alpha_d, beta_d, min=0, max=365) {

r = r / (max-min)



densities = sapply(r, function(Tr) {
  print(Tr)
  integrate(function(y) dlast_end_time(Tr + y, N, alpha_s, beta_s, alpha_d, beta_d) * dfirst_start_time(y,N,alpha_s, beta_s), lower = 0, upper = 1-Tr)$value
})

return(densities / (max-min))

}

dend_time <- function(t, alpha_s, beta_s, alpha_d, beta_d) {
  sapply(t, function(te) {
    #if (te <= 0 || te >= 1) return(0)

    integrand <- function(s) {
      #if (s <= 0 || s >= te || s >= 1) return(0)
      u <- (te - s) / (1 - s)
      #if (u <= 0 || u >= 1) return(0)

      f_ts <- dbeta(s, alpha_s, beta_s)
      f_u  <- dbeta(u, alpha_d, beta_d)
      return(f_ts * f_u / (1 - s))
    }

    out <- tryCatch(
      integrate(integrand, lower = 0, upper = te, rel.tol = 1e-6)$value,
      error = function(e) 1e-12
    )

    return(out)
  })
}

pend_time_raw = function(t, alpha_s, beta_s, alpha_d, beta_d) {
  #if (t <= 0) return(0) 	#should work for vectorized version
  #if (t >= 1) return(1) 	#should work for vectorized version

  integrand <- function(u) {
    dend_time(u, alpha_s, beta_s, alpha_d, beta_d)
  }

  # Numerically integrate f_Te from 0 to t
  integrate(integrand, lower = 0, upper = t, rel.tol = 1e-6)$value
}

pend_time <- Vectorize(pend_time_raw)



# --- 2. Density function for observed ---
dobserved <- function(x, alpha_s, beta_s, alpha_d, beta_d, normalize=T) {
  sapply(x, function(to) {		#'to' here is the observed time, not the onset. 's' is the start or onset
    integrand <- function(s) {
      #if (s >= to) return(0)			# this check should be here, but doesn't work vectorized
      f_ts <- dbeta_safe(s, alpha_s, beta_s)
      scaled_tail <- (to - s) / (1 - s)
      tail_prob <- 1 - pbeta_safe(scaled_tail, alpha_d, beta_d)
      mu_s = alpha_s/(alpha_s+beta_s)
      mu_d = alpha_d/(alpha_d+beta_d)
	if(normalize) {
      nc = mu_d * (1 - mu_s) + 1e-12
	}
	else {
	nc = 1
	}
      f_ts * tail_prob / nc
    }
    tryCatch(
      integrate(integrand, lower = 0, upper = to, rel.tol = 1e-6)$value,
      error = function(e) 1e-12
    )
  })
}



pobserved <- function(q, alpha_s, beta_s, alpha_d, beta_d) {
  sapply(q, function(tq) {
    tryCatch(
      integrate(
        function(u) dobserved(u, alpha_s, beta_s, alpha_d, beta_d),
        lower = 0,
        upper = tq,
        rel.tol = 1e-6
      )$value,
      error = function(e) 1e-12
    )
  })
}

qobserved <- function(p, alpha_s, beta_s, alpha_d, beta_d) {
  sapply(p, function(prob) {
    if (prob <= 0) return(0)
    if (prob >= 1) return(1)

    # Root-finding: Find q such that pobserved(q) = prob
    uniroot(
      function(q) pobserved(q, alpha_s, beta_s, alpha_d, beta_d) - prob,
      lower = 0,
      upper = 1,
      tol = 1e-6
    )$root
  })
}

#NEED TO CHECK IF DURATION IS PRESCALED
robserved = function(n, alpha_s, beta_s, alpha_d, beta_d) {
  # Simulate onset times
  t_start <- rbeta(n, alpha_s, beta_s)

  # Simulate durations scaled to (0, 1 - onset)
  raw_duration <- rbeta(n, alpha_d, beta_d)
  duration <- raw_duration * (1 - t_start)

  t_end <- t_start + duration

  # Uniform sample between start and end for each individual
  observed <- runif(n, min = t_start, max = t_end)
  return(observed)
}

#provides the pmf of the proportion of individuals in a population that are in the phenophase at a give time, t
dproportionInPhenophase = function(x, t, alpha_s, beta_s, alpha_d, beta_d, minT=0, maxT=365) {
	if(sum(ifelse(x<0,1,0))>0) {
		stop("The input proportion of the population in the phenophase, x, should be above 0.")
		}
	if(sum(ifelse(x>1,1,0))>0) {
		stop("The input proportion of the population in the phenophase, x, should be below 1.")
		}

	if(t<minT || t>maxT) {
		stop("The provided time must be in the input range of times specified by minT and maxT.")
	}

	t = (t-minT) / (maxT - minT)


	N = 1000
	n = round(x * 1000)

	Pt = dobserved(t, alpha_s, beta_s, alpha_d, beta_d, normalize=F)
	return( dbinom(x=n, size=N, prob=Pt) )

	}
