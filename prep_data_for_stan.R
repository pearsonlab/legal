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
X <- get_fixed_design_matrix(df, predictors)

