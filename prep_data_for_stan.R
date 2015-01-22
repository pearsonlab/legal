# this file reads in the survey data and prepares it for export to Stan
source('helpers.R')

# read data
data_file_name <- 'data/data_sq_deid.csv'
df <- read.csv(data_file_name)

# define some classes of variables
# NOTE: order matters for predictors, since we're omitting the baseline
predictors <- c('scenario', 'physical', 'history', 'victim', 'witness')
fixed <- c('physical', 'history', 'victim', 'witness')
random <- c('scenario')
outcomes <- c('rate_outrage', 'rate_punishment', 'rate_threat', 'rating')

# make sure some variables are appropriately encoded
df <- convert_to_factor(df, predictors)
df$ID <- as.integer(df$hashedID)

# drop any duplicate (question, ID) pairs
df <- df[!duplicated(df[, c('question', 'ID')]),]  

# make a design matrix of dummy-coded predictors
Xdf <- get_fixed_design_matrix(df, predictors)
X <- reshape_data_by_unit(Xdf, df$question, df$ID)

# get ratings output by subject
R <- reshape_data_by_unit(df[, outcomes], df$question, df$ID)

# define some constants
Nsub <- dim(X)[3]  # number of subjects
Nresp <- dim(X)[1]  # number of responses
Nvar <- dim(X)[2]  # number of regressors/independent variables
Nout <- dim(R)[2]  # number of outcome/dependent variables

Rmin = 0  # minimum outcome rating
Rmax = 100  # maximum outcome rating

# make a list of variables to dump out
dat <- list(Nsub = Nsub, Nresp = Nresp, Nvar = Nvar, Nout = Nout, 
            X = X, R = R, Rmax = Rmax, Rmin = Rmin, xnames = dimnames(X), 
            rnames = dimnames(R))

save(list=c('dat'),file='data_for_stan')
rm(list=ls())