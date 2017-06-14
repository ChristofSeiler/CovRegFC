data {
  int<lower=1> K;
  int<lower=1> J;
  int<lower=0> N;
  vector[J] x[N];
  vector[K] y[N];
}
parameters {
  //matrix[K, J] A;
  matrix[K, J] B;
  vector<lower=0>[K] sigma;
  real gamma[N];
}
model {
  // prior
  gamma ~ normal(0,1);
  // likelihood
  for (n in 1:N) {
    // A * x[n] + gamma[n] * B * x[n];
    y[n] ~ normal(gamma[n] * B * x[n], sigma);
  }
}
