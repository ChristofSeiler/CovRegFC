data {
  int<lower=1> K;
  int<lower=1> J;
  int<lower=0> N;
  vector[J] x[N];
  vector[K] y[N];
}
parameters {
  matrix[K, J] alpha;
  matrix[K, J] beta;
  cholesky_factor_corr[K] L_Omega;
  vector<lower=0>[K] L_sigma;
  real gamma[N];
}
model {
  vector[K] mu[N];
  matrix[K, K] L_Sigma;
  for (n in 1:N)
    mu[n] = alpha * x[n] + gamma[n] * beta * x[n];
  L_Sigma = diag_pre_multiply(L_sigma, L_Omega);
  to_vector(alpha) ~ normal(0, 5);
  to_vector(beta) ~ normal(0, 5);
  gamma ~ normal(0,1);
  L_Omega ~ lkj_corr_cholesky(1);
  L_sigma ~ cauchy(0, 2.5);
  y ~ multi_normal_cholesky(mu, L_Sigma);
}
generated quantities {
  //vector[K] mu[N];
  //matrix[K, K] L_Sigma;
  corr_matrix[K] Omega;
  //vector[K] y_tilde[N];
  // correlation matrix
  Omega = multiply_lower_tri_self_transpose(L_Omega);
  // predictive check
  //L_Sigma = diag_pre_multiply(L_sigma, L_Omega);
  //for(n in 1:N) {
  //  mu[n] = alpha * x[n] + gamma[n] * beta * x[n];
  //  y_tilde[n] = multi_normal_cholesky_rng(mu[n], L_Sigma);
  //}
}
