functions {
  // Implemented in C++ in gp.hpp
  real log_nc(real mu_O, real mu_C, real sigma, int debug);
  real log_nc(real mu_O, real mu_C, real sigma);

  // The likelihood exluding the normalisation constant (log_nc)
  real obs_gp_lpdf(real t, real mu_O, real mu_C, real sigma, int debug) {
    if(t > mu_O) {
      if(debug == 2) {
        print("T: ", t, ", ", [normal_lcdf(mu_C | t, sigma), normal_lcdf(mu_O | t, sigma)]);
      }
      // observed tiem after onset, CDF is going to be small (and thus precise).
      // straightforward computation
      return(log_diff_exp(normal_lcdf(mu_C | t, sigma), normal_lcdf(mu_O | t, sigma)));
    } else {
      if(debug == 2) {
        print("T - flip: ", t, ", ", [normal_lcdf(2 * t - mu_O | t, sigma), normal_lcdf(2 * t - mu_C | t, sigma),
        log_diff_exp(normal_lcdf(2 * t - mu_O | t, sigma), normal_lcdf(2 * t - mu_C | t, sigma))]);
      }
      // observed tiem before onset, CDF might be imprecise.
      // flipping around t to get small values again
      return(log_diff_exp(normal_lcdf(2 * t - mu_O | t, sigma), normal_lcdf(2 * t - mu_C | t, sigma)));
    }
  }
}

data {
  int<lower=0, upper=10> debug;

//For prior distribution checks (drop_ll = drop likelihood = ignore data, in which, the normalization constant, drop_nc, is ignored as well!)
  int<lower=0, upper=1> drop_nc;
  int<lower=0, upper=1> drop_ll;

//Sample size
  int<lower=1> N;

//Response variable data (observed time, t); these should be min max scaled
  vector<lower=0, upper=1>[N] t;

//Covariate data; these should be min max scaled
  vector<lower=0, upper=1>[N] X;

//Hyperparameters

  real meanSlopeO;
  real<lower=0> sdSlopeO;
  real<lower=0, upper=1> meanAnchorO;
  real<lower=0> sdAnchorO;

  real meanSlopeD;
  real<lower=0> sdSlopeD;
  real<lower=0, upper=1> meanAnchorD;
  real<lower=0> sdAnchorD;

  real<lower=0> meanSigma;
  real<lower=0> sdSigma;
}

parameters {
//alpha and beta intercept and coefficients to determine means of onset and duration
  real betaO; 
  real betaD; 
  real<lower=0, upper=1> anchorO;	//mean of onset across the range - used to calculate intercept alpha for onset
  real<lower=0, upper=1> anchorD;	//mean of duration across the range - used to calculate intercept alpha for duration
  real<lower=0> sigma;
}


transformed parameters {
  vector[N] mu_O;
  vector[N] mu_C;
  vector[N] mu_D;

  real alphaO;
  real alphaD;

  alphaO = anchorO - 0.5 * betaO;	//calculates the intercept based on the midpoint, 0.5, of the range of the covariate (since covariates are min max scaled)
  alphaD = anchorD - 0.5 * betaD;	//calculates the intercept based on the midpoint, 0.5, of the range of the covariate (since covariates are min max scaled)

  mu_O =  alphaO + X * betaO ; 	
  mu_D =  alphaD + X * betaD ; 	
  mu_C = mu_O + mu_D;

//The for loop is made explicit for testing purposes
  //for(i in 1:N) {
//process the model for mu_O
    //mu_O[i] = alphaO + betaO * X[i]; //generate the mean of the onset from the linear model that includes the factors of interest
//process the model for mu_D
    //mu_D[i] = alphaD + betaD * X[i]; //generate the mean of the onset from the linear model that includes the factors of interest
//Calculate the cessation, whose expected value is mu_O + mu_D
    //mu_C[i] = mu_O[i] + mu_D[i];

}

//Calculate the posterior with the likelihood and priors
model {
  //if(debug) {
    //print("Mus: ", [mu_O, mu_C], ", sigma:", sigma);
  //}

//Calculate the likelihood
  if(!drop_ll) {
    for(n in 1:N) {
      target += obs_gp_lpdf(t[n] | mu_O[n], mu_C[n], sigma, debug);
      if(!drop_nc) {
        target += -log_nc(mu_O[n], mu_C[n], sigma, debug); //p(state = 1 | all but t)
      }
    }
  }

//Define priors

betaO ~ normal(meanSlopeO,sdSlopeO);
betaD ~ normal(meanSlopeD,sdSlopeD);
anchorO ~ normal(meanAnchorO,sdAnchorO);
anchorD ~ normal(meanAnchorD,sdAnchorD);
sigma ~ normal(meanSigma,sdSigma);

}
