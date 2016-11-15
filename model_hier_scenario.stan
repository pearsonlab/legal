data {
  int L;  # lower censoring
  int U;  # upper censoring
  int<lower=0> Nsub;  # number of subjects
  int<lower=0> Nc;  # number of cases
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
}
transformed data {
  real M;

  M = (U + L)/2.;
}
parameters {
  # mean and variance across scenarios for each regressor 
  vector[P] mu;  
  vector<lower=0>[P] eta;  
  
  # mean and variance across subjects within scenario 
  vector<lower=0>[P] tau[Nc];
  
  # residuals
  vector[P] delta[Nc];  # scenario-specific
  vector[P] eps[Nsub];  # subject-specific
  real<lower=0> sigma;  # observation noise
  
}
transformed parameters {
  real theta[N];
  real thetaL[NL];
  real thetaU[NU];
  vector[P] gamma[Nc];  # scenario effects
  vector[P] beta[Nsub, Nc];  # individual effects
  
  # draw scenario effects for each group
  for (c in 1:Nc) {
    gamma[c] = mu + eta .* delta[c];
  }

  # draw individual effects
  for (c in 1:Nc) {
    for (i in 1:Nsub) {
      beta[i, c] = gamma[c] + tau[c] .* eps[i];
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
    eps[i] ~ cauchy(0., 1.);

  for (c in 1:Nc) {
    delta[c] ~ cauchy(0., 1.);
  }
  
  mu ~ normal(M, M);
  eta ~ cauchy(0, M);
  for (c in 1:Nc)
    tau[c] ~ cauchy(0, M);

  sigma ~ cauchy(0, M/10.);

  R ~ normal(theta, sigma);

  for (i in 1:NL)
    target += normal_lcdf(L | thetaL[i], sigma);
  for (i in 1:NU)
    target += normal_lccdf(L | thetaL[i], sigma);
}

