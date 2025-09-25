library(fitdistrplus)

scale_affine_model_mean_anchor <- function(y_mean, betas, x_means, x_mins, x_maxs, y_min, y_max) {
  if (!all(names(betas) == names(x_means)) ||
      !all(names(betas) == names(x_mins)) ||
      !all(names(betas) == names(x_maxs))) {
    stop("Names of betas, x_means, x_mins, and x_maxs must all match.")
  }

  y_range <- y_max - y_min
  x_ranges <- x_maxs - x_mins

  # Scaled coefficients
  beta_tilde <- betas * x_ranges / y_range

  # Intercept in scaled space when x_k = mean(x_k)
  alpha_tilde <- (y_mean - y_min - sum(betas * x_means)) / y_range + sum(beta_tilde * (x_means - x_mins) / x_ranges)

  return(list(
    alpha_tilde = alpha_tilde,
    beta_tilde = beta_tilde
  ))
}

unscale_affine_model_mean_anchor <- function(y_mean_tilde,  alpha_tilde, beta_tilde, x_means, x_mins, x_maxs, y_min, y_max) {
  if (!all(names(beta_tilde) == names(x_means)) ||
      !all(names(beta_tilde) == names(x_mins)) ||
      !all(names(beta_tilde) == names(x_maxs))) {
    stop("Names of beta_tilde, x_means, x_mins, and x_maxs must all match.")
  }

if(y_min !=0 ) {
   stop("The mimimum response variable needs to be 0")
}


  y_range <- y_max - y_min
  x_ranges <- x_maxs - x_mins

  #Recover original y_mean
  y_mean = y_min + y_mean_tilde * y_range

  # Recover original betas
  betas <- beta_tilde * y_range / x_ranges

  # Recover original alpha using mean anchor
  alpha <- y_mean - sum(betas * x_means)

  return(list(
    y_mean = y_mean,
    alpha = alpha,
    betas = betas
  ))
}

#Obtains the unscaled hyperparameters from a file associated with a provided file name
getHyperparameters = function(hyperparameterFile) {
        hyperparameters = read.table(hyperparameterFile, header=T, sep='\t')
        return(hyperparameters)
}

#Obtains min max scaled covariate data from a file associated with a provided file name
getCovariates = function(covariatesFile) {
        covariates = read.table(covariatesFile, header=T, sep='\t')
        return(processCovariates(covariates))
}

processCovariates = function(covariates) {

        mins = apply(covariates,2,min)
        maxs = apply(covariates,2,max)

        scaledCovariates <- as.data.frame(lapply(names(covariates), function(col_name) {
                (covariates[[col_name]] - mins[col_name]) / (maxs[col_name] - mins[col_name])
                }))

        colnames(scaledCovariates) <- names(covariates)


        output = list(
                mins = mins,
                maxs = maxs,
                scaledCovariates = scaledCovariates,
                covariates = covariates,
		K = ncol(covariates)
                )

        return(output)
}


#expected value of the response variable if sampling of the covariate is biased such that sampling effort y = a + bx and response = alpha + beta * x
expected_y_biased <- function(alpha, beta, a, b, x_min, x_max) {
  Z <- a * (x_max - x_min) + 0.5 * b * (x_max^2 - x_min^2)
  
  E_x_num <- 0.5 * a * (x_max^2 - x_min^2) + (1/3) * b * (x_max^3 - x_min^3)
  E_x <- E_x_num / Z
  
  return(alpha + beta * E_x)
}

#if sampling intensity increases as a + bx +Z, where x is the covariate value, the following function will sample the covariate values accordingly. This is useful if there is attentuation in the sampling rate of the covariate as we go backwards in time, for example
sample_affine_inverse <- function(n, a, b, x_min, x_max) {
  A <- a
  B <- b
  x0 <- x_min
  x1 <- x_max

  # Normalization constant Z
  Z <- A * (x1 - x0) + 0.5 * B * (x1^2 - x0^2)

  # Sample u ~ Uniform(0, 1)
  u <- runif(n)

  samples <- numeric(n)

  for (i in 1:n) {
    u_i <- u[i]
    
    if (abs(B) < 1e-10) {
      # If b ≈ 0, it's uniform
      samples[i] <- x0 + (x1 - x0) * u_i
    } else {
      # Solve: u = [A(x - x0) + B/2(x^2 - x0^2)] / Z
      
      # Rewrite as quadratic in x: (B/2)x^2 + A x - C = 0
      C <- Z * u_i - A * x0 - 0.5 * B * x0^2
      
      # Quadratic formula: ax^2 + bx + c = 0
      a_q <- 0.5 * B
      b_q <- A
      c_q <- -C
      
      disc <- b_q^2 - 4 * a_q * c_q
      
      if (disc < 0) stop("Negative discriminant — check parameters")
      
      x_pos <- (-b_q + sqrt(disc)) / (2 * a_q)
      x_neg <- (-b_q - sqrt(disc)) / (2 * a_q)
      
      # Choose root in [x_min, x_max]
      x <- if (x0 <= x_pos && x_pos <= x1) x_pos else x_neg
      samples[i] <- x
    }
  }

  return(samples)
}

#x is covariate like date as decimal and y is response, like day of year of collection
#provide the range of covariate values (e.g, dates 1850 to 2025) over which response variables were observed
#provide the average response over that period (y_mean, e.g. April 10 or day of year 100)
#provide the range in observed responses (e.g., March 10 through May 10, or day 69 through day 130)
#This function will provide a range of slopes and intercepts that will give rise to observations within the range of observed values
feasible_beta_alpha_range <- function(x_min, x_max, y_min, y_max, y_mean) {
  if (x_min >= x_max) {
    stop("x_min and x_max must be different to define a covariate range.")
  }

  delta_x <- x_max - x_min
  x_avg <- (x_min + x_max) / 2

  # Feasible beta range
  beta_min <- (y_min - y_max) / delta_x
  beta_max <- (y_max - y_min) / delta_x

  # Corresponding alpha values to ensure mean response is y_mean
  alpha_from_beta <- function(beta) {
    return(y_mean - beta * x_avg)
  }

  alpha_min <- alpha_from_beta(beta_max)
  alpha_max <- alpha_from_beta(beta_min)

  list(
    beta_range = c(min = beta_min, max = beta_max),
    alpha_range = c(min = alpha_min, max = alpha_max),
    x_avg = x_avg
  )
}

feasible_beta_range <- function(x_min, x_max, y_min, y_max) {
  #if (x_min >= x_max) {
    #stop("x_min and x_max must be different to define a covariate range.")
  #}

  delta_x <- x_max - x_min

  # Feasible beta range
  beta_min <- (y_min - y_max) / delta_x
  beta_max <- (y_max - y_min) / delta_x

  beta_range = c(min = beta_min, max = beta_max)
  return(beta_range)
}

#for a given range in the covariate x, a given range of the response, y, within the range of the covariate, a given slope of the linear response, and a given mean response over the range of the covariate, the below function returns the corresponding intercept for the linear response
calculate_intercept <- function(x_min, x_max, y_min, y_max, y_mean, beta) {
  # Ensure valid input
  if (x_min >= x_max) {
    stop("x_min and x_max must be different to define a covariate range.")
  }
  
  # Calculate the average of the covariate range
  x_avg <- (x_min + x_max) / 2
  
  # Calculate the intercept (alpha)
  alpha <- y_mean - beta * x_avg
  
  return(alpha)
}

#for a given mean, variance, and skewness, calculates the parameters for a skew normal distribution
skewnorm_params <- function(mean, variance, skewness) {
  # Invert skewness formula numerically to find delta
  objective <- function(delta) {
    gamma1_hat <- ((4 - pi)/2) * ((delta * sqrt(2/pi)) / ((1 - (2 * delta^2)/pi)^(3/2)))
    return((gamma1_hat - skewness)^2)
  }

#print("defined obj")

  # Optimize over delta ∈ (-1, 1)
  opt <- optimize(objective, c(-0.99, 0.99))
  delta <- opt$minimum

#print("opt obj")

  # Back out alpha
  alpha <- delta / sqrt(1 - delta^2)

#print("alpha")

  # Compute omega
  omega <- sqrt(variance / (1 - (2 * delta^2) / pi))

#print("omega")

  # Compute xi
  xi <- mean - omega * delta * sqrt(2 / pi)

#print("xi")

  return(list(xi = xi, omega = omega, alpha = alpha))
}

library(sn)

# Function to calculate theoretical skewness
skewness_sn <- function(alpha) {
  delta <- alpha / sqrt(1 + alpha^2)
  gamma1 <- ((4 - pi) / 2) * ( (delta * sqrt(2 / pi))^3 / ((1 - (2 * delta^2) / pi)^(3/2)) )
  return(gamma1)
}

# Function to estimate mode numerically from parameters
mode_sn <- function(xi, omega, alpha) {
  f <- function(x) -dsn(x, xi, omega, alpha)
  optimize(f, c(xi - 5 * omega, xi + 5 * omega))$minimum
}

# Objective function to minimize
objective <- function(params, target_mode, target_var, target_skew) {
  xi <- params[1]
  omega <- params[2]
  alpha <- params[3]

  if (omega <= 0) return(1e6)  # enforce omega > 0

  delta <- alpha / sqrt(1 + alpha^2)
  mean_sn <- xi + omega * delta * sqrt(2 / pi)
  var_sn <- omega^2 * (1 - (2 * delta^2) / pi)
  skew_sn <- skewness_sn(alpha)
  mode_est <- mode_sn(xi, omega, alpha)

  # Weighted sum of squared differences
  (mode_est - target_mode)^2 + 
  (var_sn - target_var)^2 + 
  (skew_sn - target_skew)^2
}

# Main routine
estimate_sn_params <- function(target_mode, target_var, target_skew, init = c(0, 1, 1)) {
  res <- optim(
    par = init,
    fn = objective,
    target_mode = target_mode,
    target_var = target_var,
    target_skew = target_skew,
    method = "L-BFGS-B",
    lower = c(-Inf, 1e-6, -20),
    upper = c(Inf, Inf, 20)
  )
  names(res$par) <- c("xi", "omega", "alpha")
  return(res$par)
}


createZerosDataFrame = function(nrow,ncol,colNamePrefix)  {
# Create a list of columns filled with zeros 
columns <- lapply(1:ncol, function(i) rep(0, nrow))

# Assign column names like V1, V2, ..., Vn
names(columns) <- paste0(colNamePrefix, 1:ncol)

# Convert to a data frame
df <- data.frame(columns)
return(df)
}


make_quantile_function_from_pdf <- function(ddist, support = c(-10, 10), n = 100) {
  # Grid of x values
  x <- seq(support[1], support[2], length.out = n)
  dx <- diff(x)[1]

  # Evaluate PDF
  pdf_vals <- ddist(x)

  # Normalize the PDF to ensure it integrates to 1
  pdf_vals <- pdf_vals / sum(pdf_vals * dx)

  # Compute the CDF
  cdf_vals <- cumsum(pdf_vals) * dx

  # Construct the quantile function (inverse CDF)
  qfun <- approxfun(cdf_vals, x, method = "linear", ties = "ordered")

  # Return a named function that accepts probability p
  quantile_func <- function(p) {
    if (any(p < 0 | p > 1)) stop("Probabilities must be between 0 and 1.")
    qfun(p)
  }

  return(quantile_func)
}

isBetaFeasible = function(alpha, beta, minS=1, maxS=3000) {
	if(alpha<minS || alpha>maxS || beta<minS || beta>maxS) {
		return(F)
	}
	return(T)
}

getObservations_standardized = function(responseFile) {
	        observed = read.table(responseFile, header=T)
        if(ncol(observed)>1) {
                print("The response file should contain only one column with the time of the observed response for each specimen. Each row represents a different specimen. There should be a header in the file.")
		return(
			list(error = T,
                	error_m = "The response file should contain only one column with the time of the observed response for each specimen. Each row represents a different specimen. There should be a header in the file."
			)
		);
        }

        #scale data between max and min
        observed =  observed[[1]]
	responseMean = mean(observed)
	responseSD = sd(observed)
	
        observed = (observed - responseMean) / responseSD
	out = list(
		error = F,
		error_m = "No errors detected.",
		observed = observed,
		mean = responseMean,
		sd = responseSD
		)
	return(out)
}

getObservations = function(responseFile,min=0,max=365) {
	        observed = read.table(responseFile, header=T)
        if(ncol(observed)>1) {
                print("The response file should contain only one column with the time of the observed response for each specimen. Each row represents a different specimen. There should be a header in the file.")
                return();
        }

        #scale data between max and min
        observed =  observed[[1]]
        observed = (observed - min) / (max - min)
	return(observed)
}

beta_mean = function(alpha, beta) {
	return(alpha/(alpha+beta))
}

beta_sd = function(alpha, beta) {
	var = alpha*beta / ((alpha+beta)*(alpha+beta)*(alpha+beta+1))
	sd = sqrt(var)
	return(sd)
}

beta_alpha <- function(mean, sd) {
  ((1 - mean) / sd^2 - 1 / mean) * mean^2
}

beta_beta <- function(mean, sd) {
  ((1 - mean) / sd^2 - 1 / mean) * mean * (1 - mean)
}

get_phenologyShapeParams = function(mu_o, sd_o, mu_d, sd_d) {
	a_s = beta_alpha(mu_o, sd_o)
	b_s = beta_beta(mu_o, sd_o)
	a_d = beta_alpha(mu_d, sd_d)
	b_d = beta_beta(mu_d, sd_d)

	return(c(a_s,b_s,a_d,b_d))
}

# Compute Cohen's d for start and stop time distributions
# Cohen's d is a measurement of how far two distributions are, scaled by their standard deviation
# My prediction is that the error will decrease as Cohen's d increases
# The below function is approximate
cohen_d <- function(mu_s, sd_s, mu_d, sd_d) {
  numerator <- mu_d * (1 - mu_s)
  denominator <- sqrt(sd_s^2 + 0.5 * sd_d^2 * (1 - mu_s)^2)
  d <- numerator / denominator
  return(d)
}


#compute the standard deviations for the mean and sd parameterization of a beta distribution based on the SD of the alpha and beta shape parameters (from the Bayesian posterior or MLE analysis)
compute_se <- function(alpha, beta, se_alpha, se_beta) {
  # SE of mean
  se_mean <- sqrt((1 / (alpha + beta))^2 * se_alpha^2 + (-alpha / (alpha + beta)^2)^2 * se_beta^2)

  # SE of SD
  se_sd <- sqrt(
    (beta / (alpha + beta)^2 / (alpha + beta + 1))^2 * se_alpha^2 +
    (alpha / (alpha + beta)^2 / (alpha + beta + 1))^2 * se_beta^2
  )
  
  return(list(se_mean = se_mean, se_sd = se_sd))
}

# --- 1. Safe beta helpers ---
dbeta_safe <- function(x, shape1, shape2) {
  ifelse(x <= 0 | x >= 1, 0, dbeta(x, shape1, shape2))
}

pbeta_safe <- function(q, shape1, shape2) {
  ifelse(q <= 0, 0,
         ifelse(q >= 1, 1, pbeta(q, shape1, shape2)))
}

