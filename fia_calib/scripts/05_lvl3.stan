//
// This Stan program defines a three-level hierarchical model. 
//

// Stan doesn't allow 0 values in the gamma. Following the "integrating out"
// method from https://mc-stan.org/docs/stan-users-guide/truncation-censoring.html#censored.section

functions{
  real gamma_zeroes_lpdf(vector y, vector shape, vector rate){
    vector [num_elements(y)] llk;
    for(i in 1:num_elements(y)){
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
  int<lower=0> N_plots;
  vector<lower=0>[N] G_r;
  vector[N] larch; // binary indicator on species ID
  vector[N] initial_dbh; // DBH at first measurement/beginning of projection
  array[N] int plot_id;
  vector[N_plots] evap;
  vector[N_plots] density;
}


parameters {
  // tree level coefficients
  real beta_larch;
  real beta_size;
  real<lower=0> sigma;
  
  // plot
  real beta_evap;
  real beta_density;
  real beta_0;
  real sigma_plot;// note no constraint because this will be exponentiated!
}

transformed parameters{
  vector[N_plots] alpha_plot = beta_0+beta_evap*evap + beta_density*density+sigma_plot;
  vector[N] mu; 
  mu = exp(alpha_plot[plot_id]+larch.*beta_larch+initial_dbh.*beta_size);
  vector[N] shape = square(mu)./square(sigma);
  vector[N] rate = mu./square(sigma);
}

model {
  // tree-level priors
  sigma ~ normal(0, 1); // draw one value total
  beta_larch ~ normal(0, 0.5);
  beta_size ~ normal(0, 0.75); // exp(0.75) = 2.1
  
  // plot-level priors
  beta_evap ~ normal(0, 0.75);
  beta_density ~ normal(0, 0.75);
  beta_0 ~ normal(0, 0.25); // exp(0.25) = 1.3
  
// most prob mass for sigma should be between 0 and 0.7 because we exponentiate;
// exp(0.7) ~ 2 and lim(x --> -infty) exp(x) = 0. 
// So we want a negative mean here and smallish SD.
  sigma_plot ~ normal(-0.25, 0.5); 

  // data model
  G_r ~ gamma_zeroes(shape, rate);
}

