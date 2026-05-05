//
// This Stan program defines a three-level hierarchical model. 
//

// Stan doesn't allow 0 values in the gamma. Following the "integrating out"
// method from https://mc-stan.org/docs/stan-users-guide/truncation-censoring.html#censored.section

functions{
  real gamma_zeroes_lpdf(vector y, vector shape, real rate){
    vector [num_elements(y)] llk;
    for(i in 1:num_elements(y)){
      if (y[i] == 0) 
        // recorded DG = 0 means actual could be anywhere from 0-0.05.
        llk[i] = gamma_lcdf(0.05|shape[i], rate); 
      else
        llk[i] = gamma_lpdf(y[i]|shape[i], rate);
    }
    return sum(llk);
}
}

data {
  int<lower=0> N;
  int<lower=0> N_plots;
  vector<lower=0>[N] G_r;
  vector[N] larch; // binary indicator on species ID
  array[N] int plot_id;
  vector[N_plots] evap;
  vector[N_plots] density;
}


parameters {
  // tree level parameters
  real<lower=0> scale;
  real beta_larch;
  
  // plot parameters
  real beta_evap;
  real beta_density;
  real beta_0;
  real<lower=0> sigma_plot;
  vector[N_plots] alpha_plot_std;
}

transformed parameters{
  vector[N_plots] alpha_plot = beta_0+beta_evap.*evap+beta_density.*density + sigma_plot.*alpha_plot_std;
  vector[N] mu = exp(alpha_plot[plot_id]+beta_larch.*larch);
  vector[N] shape = mu/scale;
}

model {
  // tree-level priors
  scale ~ cauchy(0, 1); // draw one value total
  beta_larch ~ normal(0, 0.5);
  
  // plot-level priors
  beta_evap ~ normal(0, 0.75);
  beta_density ~ normal(0, 0.75);
  beta_0 ~ normal(0, 0.5); 
  alpha_plot_std ~ std_normal();
  sigma_plot ~ normal(0, 1); 

  // data model
  G_r ~ gamma_zeroes(shape, 1/scale);
}

generated quantities {
  array[N] real y_rep = gamma_rng(shape, 1/scale);
}
