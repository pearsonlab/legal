// Model 0:
// Uses provided random and fixed effects
// No missing outcomes
// No covariance among random effects
// No covariance among outcomes

data {
  int<lower = 1> Nsub;  // number of subjects
  int<lower = 1> Ntrials;  // number of trials in dataset
  int<lower = 0> Nfixed;  // number of fixed effect variables
  int<lower = 0> Nrand;  // number of random effects variables
  int<lower = 1> Nout;  // number of outcome variables
  int<lower = 0> Rmin;  // minimum value of outcome variable
  int<lower = 0> Rmax;  // maximum value of outcome variable
  int<lower = 1> sub[Ntrials];  // which subject did each trial
  matrix[Ntrials, Nfixed] X;  // fixed effects design matrix
  matrix[Ntrials, Nrand] Z;  // random effects design matrix
  matrix<lower = Rmin, upper = Rmax>[Ntrials, Nout] R;  // outcomes matrix
}

transformed data {
  real Rmid;
  real Rrange;

  Rmid <- 0.5 * (Rmax - Rmin);
  Rrange <- Rmax - Rmin;
}

parameters {
  vector[Nfixed] beta;  // vector of fixed effects
  vector[Nrand] mu;  // means of random effects
  vector<lower = 0>[Nrand] sig;  // standard deviations of random effects
  vector[Nrand] gamma[Nsub];  // random effects for each subject
  real<lower = 0> S[Nout];  // standard deviations of outcome variables
}

model {
  // hyperpriors on random effects
  mu ~ normal(Rmid, Rrange);
  sig ~ exponential(1 / Rrange);

  // draw random effects
  for (ind in 1:Nsub) {
    gamma[ind] ~ normal(mu, sig);
  }

  // priors on fixed effects
  beta ~ normal(0, Rrange);

  // do trials
  for (ind in 1:Ntrials) {
    for (v in 1:Nout) {
      R[ind, v] ~ normal(X[ind] * beta + Z[ind] * gamma[sub[ind]], S[v]) T[Rmin, Rmax];
    }
  }
}