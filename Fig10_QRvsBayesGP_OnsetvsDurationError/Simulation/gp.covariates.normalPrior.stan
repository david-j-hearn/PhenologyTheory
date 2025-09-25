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

//Number of covariates for onset (K_O) and for duration (K_D)
  int<lower=1> K_O;
  int<lower=1> K_D;

//Response variable data (observed time, t)
  vector<lower=0, upper=1>[N] t;

//Covariate data
  matrix[N,K_O] X_O;
  matrix[N,K_D] X_D;

//Hyperparameters: means and SDs for each parameter
  vector[K_O] betaOnsetMeans;
  vector<lower=0>[K_O] betaOnsetSDs;

  real anchorOnsetMean;
  real<lower=0> anchorOnsetSD;

  vector[K_D] betaDurationMeans;
  vector<lower=0>[K_D] betaDurationSDs;

  real anchorDurationMean;
  real<lower=0> anchorDurationSD;

//Hyperparameters mean and sd of sigma
  real<lower=0> sigmaMean;
  real<lower=0> sigmaSD;
}

parameters {
 //slope parameters, beta, and mean response at midpoint, anchor, and onset and cessation distribution SD, sigma
  vector[K_O] betaO;
  real anchorO;
  vector[K_D] betaD;
  real anchorD;
  real<lower=0> sigma;
}


transformed parameters {
  vector[N] mu_O;
  vector[N] mu_C;
  vector[N] mu_D;

  real alphaO;
  real alphaD;

  //vector[N] rco = rep_vector(0.5, K_O);
  //vector[N] rcd = rep_vector(0.5, K_D);

  alphaO = anchorO - dot_product(rep_vector(0.5, K_O) , betaO);
  alphaD = anchorD - dot_product(rep_vector(0.5, K_D) , betaD);

  mu_O =  alphaO + X_O * betaO;
  mu_D =  alphaD + X_D * betaD;
  mu_C = mu_O + mu_D;
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

//Define hyperparameters

  betaO ~ normal( betaOnsetMeans, betaOnsetSDs);
  betaD ~ normal( betaDurationMeans, betaDurationSDs);

  anchorO ~ normal( anchorOnsetMean, anchorOnsetSD);
  anchorD ~ normal( anchorDurationMean, anchorDurationSD);

  sigma ~ normal( sigmaMean, sigmaSD);
}
