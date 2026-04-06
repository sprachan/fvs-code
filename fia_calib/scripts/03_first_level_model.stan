//
// This Stan program defines a simple subset of the diameter growth model,
// where the scale factor, beta, is not hierarchical.
//

// The input data are vectors 'dg_fvs' and 'dg_fia' of length 'N'.

// Following the "integrating out" method from 
// https://mc-stan.org/docs/stan-users-guide/truncation-censoring.html#censored.section
// to allow 0 measurements of diameter growth:

functions{
  real lognormal_zeroes_lpdf(vector y, vector location, vector scale){
    vector [num_elements(y)] llk;
    for(i in 1:num_elements(y)){
        // Recorded DG of 0 could actually be from 0" to 0.05".
        // Thus, we want to give the cumulative probability that DG is in 
        // [0, 0.05] 
      if (y[i] == 0) 
        // recorded DG = 0 means actual could be anywhere from 0-0.05.
        llk[i] = lognormal_lpdf(0.05|location[i], scale[i]); 
      else
        llk[i] = lognormal_lpdf(y[i]|location[i], scale[i]);
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
  vector[N] location = log((pow(mu, 2))./sqrt(pow(mu, 2)+pow(sigma, 2)));
  vector[N] scale = log(1+pow(sigma, 2)./pow(mu, 2));
}

model {
  beta ~ gamma(2, 1);
  sigma ~ normal(0, 2);
  dg_fia ~ lognormal_zeroes(location, scale);
}

