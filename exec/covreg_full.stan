data {
  int<lower=1> K;
  int<lower=1> P;
  int<lower=P-1> N;
  matrix[P,P] YY[K];
  matrix[K,2] X;
}
parameters {
  vector<lower=0>[P] sigma[2];
  corr_matrix[P] Omega[2];
  real<lower=P-1,upper=N-1> nu;
}
model {
  matrix[P,P] Sigma[2];
  // prior
  for (i in 1:2) {
    //sigma[i] ~ cauchy(0, 2.5);
    Omega[i] ~ lkj_corr(1);
    Sigma[i] = quad_form_diag(Omega[i], sigma[i]);
  }
  // likelihood
  for (k in 1:K)
    YY[k] ~ wishart(nu,X[k,1]*Sigma[1] + X[k,2]*Sigma[2]);
}
