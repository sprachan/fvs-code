//
// This Stan program defines a simple subset of the diameter growth model,
// where the scale factor, beta, is not hierarchical.
//

// The input data is the vector G_r (the ratio of FIA basal area increment to FVS
// basal area increment), of length 'N'.

functions{
  real gamma_zeroes_lpdf(vector y, real shape, real rate){
    vector [num_elements(y)] llk;
    for(i in 1:num_elements(y)){
      if (y[i] == 0) 
        // recorded DG = 0 means actual could be anywhere from 0-0.05.
        llk[i] = gamma_lpdf(0.05|shape, rate); 
      else
        llk[i] = gamma_lpdf(y[i]|shape, rate);
    }
    return sum(llk);
}
}


data {
  int<lower=0> N;
  vector[N] G_r;
}


parameters {
  real mu;
  real<lower=0> rate;
}

transformed parameters{
  real shape = mu*rate;
}

model {
  // priors
  mu ~ gamma(1, 2);
  rate ~ cauchy(0, 1);
  
  // data model
  G_r ~ gamma_zeroes(shape, rate);
}

