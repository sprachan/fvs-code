//
// This Stan program defines a simple subset of the diameter growth model,
// where the scale factor, beta, is not hierarchical.
//

// The input data are vectors 'dg_fvs' and 'dg_fia' of length 'N'.

// Stan doesn't allow 0 values in the gamma. Following the "integrating out"
// method from https://mc-stan.org/docs/stan-users-guide/truncation-censoring.html#censored.section

functions{
  real gamma_zeroes_lpdf(vector y, vector shape, vector rate){
    vector [num_elements(y)] llk;
    for(i in 1:num_elements(y)){
        // Recorded DG of 0 could actually be from 0" to 0.05".
        // Thus, we want to give the cumulative probability that DG is in 
        // [0, 0.05] 
      if (y[i] == 0) 
        // recorded DG = 0 means actual could be anywhere from 0-0.05.
        llk[i] = gamma_lcdf(0.05|shape[i], rate[i]); 
      else
        llk[i] = gamma_lpdf(y[i]|shape[i], rate[i]);
    }
    return sum(llk);
}
}

data {
  int<lower=0> N;
  vector[N] dg_fvs;
  vector[N] dg_fia;
}


parameters {
  real<lower=0> beta;
  real<lower=0> sigma;
}

transformed parameters{
  // mean for each observation depends on dg_fvs
  vector<lower = 0>[N] mu = beta*dg_fvs;
  vector[N] shape = (mu^2)/(sigma^2);
  vector[N] rate = shape./mu;
}

model {
  beta ~ lognormal(0, 0.5); // AKA log(beta) ~ N(1, 1.65)
  sigma ~ normal(0, 2);
  dg_fia ~ gamma_zeroes(shape, rate);
}

