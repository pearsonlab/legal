data {
  int L;  # lower censoring
  int U;  # upper censoring
  int<lower=0> Nsub;  # number of subjects
  int<lower=0> Nc;  # number of cases
  int<lower=0> Ng;  # number of groups
  int<lower=0> N;  # number of observations
  int<lower=0> NL;  # number of lower censored data
  int<lower=0> NU;  # number of upper censored data
  int<lower=0> P;  # number of regressors
  real<lower=L, upper=U> R[N];  # ratings
  matrix[N, P] X;  # design matrix for data
  matrix[NL, P] XL;  # design matrix for lower censored data
  matrix[NU, P] XU;  # design matrix for upper censored data
  int<lower=0> S[N];  # subject corresponding to each rating
  int<lower=0> SL[NL];  # subject corresponding to each lower censored rating
  int<lower=0> SU[NU];  # subject corresponding to each upper censored rating
  int<lower=0> C[N];  # case corresponding to each rating
  int<lower=0> CL[NL];  # case corresponding to each lower censored rating
  int<lower=0> CU[NU];  # case corresponding to each upper censored rating
  int<lower=0> G[Nsub];  # group corresponding to each subject
}
transformed data {
  real M;
  int<lower=0> GG[N];  # group for each trial

  M = (U + L)/2.;
  GG = G[S];
  
}
parameters {
  # mean and variance across scenarios for each regressor within group
  vector[P] mu[Ng];  
  vector<lower=0>[P] eta[Ng];  
  
  # mean and variance across subjects within scenario for each group
  vector<lower=0>[P] tau[Ng, Nc];
  
  # residuals
  vector[P] delta[Ng, Nc];  # scenario, group specific
  vector[P] eps[Nsub];  # subject-specific
  real<lower=0> sigma[Ng];  # observation noise
  
}
transformed parameters {
  real theta[N];
  real thetaL[NL];
  real thetaU[NU];
  vector[P] gamma[Ng, Nc];  # scenario effects
  vector[P] beta[Nsub, Nc];  # individual effects
  
  # draw scenario effects for each group
  for (g in 1:Ng) {
    for (c in 1:Nc) {
      gamma[g, c] = mu[g] + eta[g] .* delta[g, c];
    }
  }

  # draw individual effects
  for (c in 1:Nc) {
    for (i in 1:Nsub) {
      beta[i, c] = gamma[G[i], c] + tau[G[i], c] .* eps[i];
    }
  }

  # get linear predictor
  for (j in 1:N)
    theta[j] = dot_product(X[j], beta[S[j], C[j]]);

  for (j in 1:NL)
    thetaL[j] = dot_product(XL[j], beta[SL[j], C[j]]);

  for (j in 1:NU)
    thetaU[j] = dot_product(XU[j], beta[SU[j], C[j]]);
}
model {
  for (i in 1:Nsub)
    eps[i] ~ normal(0., 1.);

  for (g in 1:Ng) {
    for (c in 1:Nc) {
      delta[g, c] ~ normal(0., 1.);
    }
  }
  
  for (g in 1:Ng) {
    mu[g] ~ normal(M, M);
    eta[g] ~ cauchy(0, M);
  }

  sigma ~ cauchy(0, M/10.);

  R ~ normal(theta, sigma[GG]);

  for (i in 1:NL)
    target += normal_lcdf(L | thetaL[i], sigma[GG[i]]);
  for (i in 1:NU)
    target += normal_lccdf(L | thetaL[i], sigma[GG[i]]);
}

