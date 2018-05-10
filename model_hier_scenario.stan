data {
  int L;  // lower censoring
  int U;  // upper censoring
  int<lower=0> Nsub;  // number of subjects
  int<lower=0> Nc;  // number of cases
  int<lower=0> N;  // number of observations
  int<lower=0> P;  // number of regressors
  real<lower=L, upper=U> R[N];  // ratings
  int<lower=-1, upper=1> cens[N];  // -1 = left censor, 1 = right censor, 0 = none
  matrix[N, P] X;  // design matrix for data
  int<lower=0> S[N];  // subject corresponding to each rating
  int<lower=0> C[N];  // case corresponding to each rating
}

transformed data {
  real M;

  M = (U + L)/2.;
}
parameters {
  // mean and variance across scenarios for each regressor
  vector[P] mu;
  vector<lower=0>[P] eta;

  // mean and variance across subjects within scenario
  vector<lower=0>[P] tau[Nc];

  // residuals
  vector[P] delta[Nc];  // scenario-specific
  vector[P] eps[Nsub];  // subject-specific
  real<lower=0> sigma;  // observation noise

  // degrees of freedom
  real<lower=1> nu_eps;
  real<lower=1> nu_delta;

}

transformed parameters {
  real theta[N];
  vector[P] gamma[Nc];  // scenario effects
  vector[P] beta[Nsub, Nc];  // individual effects

  // draw scenario effects for each group
  for (c in 1:Nc) {
    gamma[c] = mu + eta .* delta[c];
  }

  // draw individual effects
  for (c in 1:Nc) {
    for (i in 1:Nsub) {
      beta[i, c] = gamma[c] + tau[c] .* eps[i];
    }
  }

  // get linear predictor
  for (j in 1:N)
    theta[j] = dot_product(X[j], beta[S[j], C[j]]);
}

model {
  for (i in 1:Nsub)
    eps[i] ~ student_t(nu_eps, 0., 1.);

  for (c in 1:Nc) {
    delta[c] ~ student_t(nu_delta, 0., 1.);
  }

  nu_eps ~ normal(0, 100);
  nu_delta ~ normal(0, 100);

  mu ~ normal(M, M);
  eta ~ cauchy(0, M);
  for (c in 1:Nc)
    tau[c] ~ cauchy(0, M);

  sigma ~ cauchy(0, M/10.);

  for (i in 1:N) {
      if (cens[i] == 0)
        R[i] ~ normal(theta[i], sigma);
      else if (cens[i] == -1)
        target += normal_lcdf(L | theta[i], sigma);
      else if (cens[i] == 1)
        target += normal_lccdf(U | theta[i], sigma);
  }
}
