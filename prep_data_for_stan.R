# this file reads in the survey data and prepares it for export to Stan
source('helpers.R')

prep_data <- function(fixed=c('physical', 'history', 'victim', 'witness'),
                      random=c('scenario'), outcomes=c('rate_outrage', 'rate_punishment', 'rate_threat', 'rating'), baseline='scenario') {
  # this function reads in input data and returns a list containing data to be 
  # fed to Stan
  # INPUTS:
  # fixed: a vector of variable names with fixed effects
  # random: a vector of variable names with random effects
  # outcomes: a vector of names of outcome variables
  # baseline: variable whose baseline category should be included 
  # in the model in place of the constant
  
  # read data
  data_file_name <- 'data/data_sq_deid.csv'
  df <- read.csv(data_file_name)
  
  # make a vector of all predictors
  predictors <- c(baseline, random, fixed)  # make sure baseline variable is first
  predictors <- unique(predictors)
  
  # make sure some variables are appropriately encoded
  df <- convert_to_factor(df, predictors)
  df$ID <- as.integer(df$hashedID)
  
  # drop any duplicate (question, ID) pairs
  df <- df[!duplicated(df[, c('question', 'ID')]),]  
  
  # make a design matrix of dummy-coded predictors
  Mdf <- get_design_matrix(df, predictors)
  M <- reshape_data_by_unit(Mdf, df$question, df$ID)
  
  # separate out fixed and random effects
  X <- extract_subset(M, fixed)
  Z <- extract_subset(M, random)
  
  # get ratings output by subject
  R <- reshape_data_by_unit(df[, outcomes], df$question, df$ID)
  
  # define some constants
  Nsub <- dim(X)[3]  # number of subjects
  Ntrials <- dim(X)[1]  # number of responses per subject
  Nfixed <- dim(X)[2]  # number of fixed effects regressors/independent variables
  Nrand <- dim(Z)[2]  # number of random effects regressors
  Nout <- dim(R)[2]  # number of outcome/dependent variables
  
  Rmin = 0  # minimum outcome rating
  Rmax = 100  # maximum outcome rating
  
  # make a list of variables to dump out
  dat <- list(Nsub = Nsub, Ntrials = Ntrials, Nfixed = Nfixed, 
              Nrand = Nrand, Nout = Nout, X = X, Z = Z, R = R, 
              Rmax = Rmax, Rmin = Rmin, fixed_names = dimnames(X), 
              rand_names = dimnames(Z), outcome_names = dimnames(R))
  
  return(dat)
}
