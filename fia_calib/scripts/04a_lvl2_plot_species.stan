//
// This Stan program defines a two-level hierarchical model. Remeasured FIA
// diameter growth is drawn from a distribution with mean around beta*DG_FVS
// where beta is a scale factor and DG_FVS is FVS projected diameter growth.
// Beta itself is drawn from a distribution that has a plot-level effect and
// an initial tree size effect.
//

// The input data are:
// N: total number of trees in the dataset
// N_plot: total number of plots in the dataset
// N_species: number of tree species
// 'dg_fvs' and 'dg_fia': numeric vectors, length N
// species: matrix of indicator variables on species
// 'plot_id': integer vector, length N_plot

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
  array[N] int plot_id;
}


parameters {
  real beta_larch;
  vector[N_plots] alpha_plot;
  real<lower=0> sigma;
}

transformed parameters{
  vector[N] mu; 
  mu = exp(alpha_plot[plot_id]+larch.*beta_larch);
  vector[N] shape = square(mu)./square(sigma);
  vector[N] rate = mu./square(sigma);
}

model {
  // priors
  sigma ~ normal(0, 1); // draw one value total
  beta_larch ~ normal(0, 0.5);
  alpha_plot ~ normal(0, 0.5); // draw one value PER PLOT


  // data model
  G_r ~ gamma_zeroes(shape, rate);
}

