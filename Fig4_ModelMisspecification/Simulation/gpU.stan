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
      // observed time after onset, CDF is going to be small (and thus precise).
      // straightforward computation
      return(log_diff_exp(normal_lcdf(mu_C | t, sigma), normal_lcdf(mu_O | t, sigma)));
    } else {
      if(debug == 2) {
        print("T - flip: ", t, ", ", [normal_lcdf(2 * t - mu_O | t, sigma), normal_lcdf(2 * t - mu_C | t, sigma),
        log_diff_exp(normal_lcdf(2 * t - mu_O | t, sigma), normal_lcdf(2 * t - mu_C | t, sigma))]);
      }
      // observed time before onset, CDF might be imprecise.
      // flipping around t to get small values again
      return(log_diff_exp(normal_lcdf(2 * t - mu_O | t, sigma), normal_lcdf(2 * t - mu_C | t, sigma)));
    }
  }
}

data {
  //sample size
  int<lower=0> N;
  //observations
  vector<lower=0, upper=1>[N] t;
  //hyperparameters
  real<lower=0,upper=1> mean_mean_onset;
  real<lower=0,upper=0.3> sd_mean_onset;
  real<lower=0,upper=1> mean_mean_duration;
  real<lower=0,upper=0.3> sd_mean_duration;
  real<lower=0,upper=0.3> mean_sd;
  real<lower=0,upper=0.3> sd_sd;

  int<lower=0, upper=10> debug;
  int<lower=0, upper=1> drop_nc;
  int<lower=0, upper=1> drop_ll;
  //real<lower=0> sigma;
}


parameters {
  real<lower=0, upper=1> mu_O;
  real<lower=0, upper=1-mu_O> mu_D;
  real<lower=0> sigma;
}

transformed parameters {
  real<lower=mu_O, upper=1> mu_C;
  mu_C = mu_O+mu_D;
  if(mu_C>0.99) { 		//atom should be negligible when model is well fit
	mu_C=0.99;
  }
}

model {
  if(debug) {
    print("Mus: ", [mu_O, mu_C], ", sigma:", sigma);
  }
  if(!drop_nc) {
    target += -N * log_nc(mu_O, mu_C, sigma, debug); //p(state = 1 | all but t)
  }
  if(!drop_ll) {
    for(n in 1:N) {
      target += obs_gp_lpdf(t[n] | mu_O, mu_C, sigma, debug);
    }
  }
  //mu_O ~ beta(3,7);
  //mu_C ~ beta(7,3);
  //sigma ~ inv_gamma(11,2);
  //mu_O ~ beta(1250,1250);
  //mu_C ~ beta(1430, 960);
  mu_O ~ normal(mean_mean_onset, sd_mean_onset);
  mu_D ~ normal(mean_mean_duration, sd_mean_duration);
  sigma ~ normal(mean_sd, sd_sd);
}
