//
// This Stan program defines a two-level hierarchical model. 
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
  vector[N] initial_dbh; // DBH at first measurement/beginning of projection
  array[N] int plot_id;
}


parameters {
  real beta_larch;
  real beta_size;
  real plot_mean;
  vector[N_plots] alpha_plot_std;
  real<lower=0> rate;
  real<lower=0> sigma_plot;
}

transformed parameters{
  vector[N_plots] alpha_plot = plot_mean+sigma_plot*alpha_plot_std;
  vector[N] mu; 
  mu = exp(alpha_plot[plot_id]+larch.*beta_larch+initial_dbh.*beta_size);
  
  vector[N] shape = mu*rate;
}

model {
  // priors
  rate ~ cauchy(0, 1); // draw one value total
  beta_larch ~ normal(0, 0.5);
  beta_size ~ normal(0, 0.75); // exp(0.75) = 2.1
  
  // plot-level priors
  plot_mean ~ normal(0, 0.5);
  sigma_plot ~ normal(0, 1);
  alpha_plot_std ~ std_normal();

  // data model
  G_r ~ gamma_zeroes(shape, rate);
}

