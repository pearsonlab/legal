data {
  int L;  // lower censoring
  int U;  // upper censoring
  int<lower=0> Nsub;  // number of subjects
  int<lower=0> N;  // number of observations
  int<lower=0> NL;  // number of lower censored data
  int<lower=0> NU;  // number of upper censored data
  int<lower=0> P;  // number of regressors
  real<lower=L, upper=U> R[N];  // ratings
  matrix[N, P] X;  // design matrix for data
  matrix[NL, P] XL;  // design matrix for lower censored data
  matrix[NU, P] XU;  // design matrix for upper censored data
  int<lower=0> S[N];  // subject corresponding to each rating
  int<lower=0> SL[NL];  // subject corresponding to each lower censored rating
  int<lower=0> SU[NU];  // subject corresponding to each upper censored rating
}
transformed data {
  real M;

  M = (U + L)/2.;
}
parameters {
  vector[P] mu;  // population effect mean
  vector<lower=0>[P] tau;  // population std
  vector[P] eps[Nsub];  // individual variability
  real<lower=0> sigma;  // observation std
}
transformed parameters {
  real theta[N];
  real thetaL[NL];
  real thetaU[NU];
  vector[P] beta[Nsub];  // individual effects

  // draw individual effects
  for (i in 1:Nsub)
    beta[i] = mu + tau .* eps[i];

  // draw mean predictions
  for (j in 1:N)
    theta[j] = dot_product(X[j], beta[S[j]]);

  for (j in 1:NL)
    thetaL[j] = dot_product(XL[j], beta[SL[j]]);

  for (j in 1:NU)
    thetaU[j] = dot_product(XU[j], beta[SU[j]]);
}
model {
  for (i in 1:Nsub)
    eps[i] ~ normal(0., 1.);

  mu ~ normal(M, M);
  tau ~ cauchy(0, M);

  sigma ~ cauchy(0, M/10.);

  R ~ normal(theta, sigma);

  for (i in 1:NL)
    target += normal_lcdf(L | thetaL[i], sigma);
  for (i in 1:NU)
    target += normal_lccdf(L | thetaL[i], sigma);
}

