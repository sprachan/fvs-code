//
// This Stan program defines a two-level hierarchical model. 
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
}


parameters {
  real beta_larch;
  real beta_size;
  vector[N_plots] alpha_plot;
  real<lower=0> sigma;
}

transformed parameters{
  vector[N] mu; 
  mu = exp(alpha_plot[plot_id]+larch.*beta_larch+initial_dbh.*beta_size);
  vector[N] shape = square(mu)./square(sigma);
  vector[N] rate = mu./square(sigma);
}

model {
  // priors
  sigma ~ normal(0, 1); // draw one value total
  beta_larch ~ normal(0, 0.5);
  alpha_plot ~ normal(0, 0.5); // draw one value PER PLOT.exp(0.5) = 1.65
  beta_size ~ normal(0, 0.75); // exp(0.75) = 2.1

  // data model
  G_r ~ gamma_zeroes(shape, rate);
}

