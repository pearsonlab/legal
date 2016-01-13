# includes small helper functions

convert_to_factor <- function(df, which_vars) {
  # convert the variable names in df given in which_vars to factors
  for (vv in which_vars) {
    df[[vv]] <- as.factor(df[[vv]])
  }
  return(df)
}

get_design_matrix <- function(df, predictors) {
  # given a set of predictors, get the design matrix, assuming all effects are fixed
  # IMPORTANT: since the model contains no constant, the first element of 
  # predictors will have its baseline included
  formula <- as.formula(paste("~ -1 +",paste(predictors, collapse="+"))) 
  X_master <- model.matrix(formula, df)  # matrix of dummy variables for each trial
  return(X_master)
}

reshape_data_by_unit <- function(data, rowvar, unitvar) {
  # given the name of a row variable and a unit variable, reshapes the dataframe 
  # into a multidimensional array (rowvar, cols, unitvar)
  require(reshape2)
  df <- as.data.frame(cbind(rowvar, unitvar, data))
  names(df)[1:2] <- c('rowvar', 'unitvar')
  suppressWarnings(melted <- melt(df, id.vars=c('rowvar', 'unitvar')))
  
  # now cast to unit x row x column
  # this is per stan convention of reading array indices before matrix/vector 
  # indices (cf. 23.8 on Type Inference in Stan Manual)
  return(acast(melted, unitvar ~ rowvar ~ variable))
} 

extract_subset <- function(M, vars) {
  # given a unit x rows x cols multidimensional array and a list of variables in vars,
  # return the slice of the array along axis 2 corresponding to variable names containing
  # any element in vars
  
  or_pattern <- paste(vars, collapse='|')
  to_grab <- grepl(or_pattern, dimnames(M)[3][[1]])
  return(M[,,to_grab])
}

extract_fixed <- function(fitobj) {
  # extract fixed effects matrix from fitobj
  ss <- summary(fitobj)$solutions
  colnames(ss) <- sanitize_colnames(colnames(ss))
  return(ss)
}

extract_random <- function(fitobj) {
  # extract random effects covariance matrix from fitobj
  ss <- summary(fitobj)$Gcovariances
  colnames(ss) <- sanitize_colnames(colnames(ss))
  return(ss)
}

extract_resid <- function(fitobj) {
  # extract residuals covariance matrix from fitobj
  ss <- summary(fitobj)$Rcovariances
  colnames(ss) <- sanitize_colnames(colnames(ss))
  return(ss)
}

sanitize_colnames <- function(cnames) {
  # get rid of awkward characters in column names
  clean <- sub(' ', '.', cnames)
  clean <- sub('%', '', clean)
  clean <- sub('-', '', clean)
  return(clean)
}

process_fixed_names <- function(X) {
  # given fixed effects matrix, return dataframe with columns outcome, predictor
  vnames <- rownames(X)
  
  # remove the word 'trait' from beginning of each name
  prestr <- 'trait'
  stripped <- sapply(vnames, function(x) {substr(x, nchar(prestr) + 1, nchar(x))})
  split <- strsplit(stripped, ':')
  frame <- data.frame(matrix(unlist(split), nrow=length(split), byrow=T))
  npreds <- length(split[[1]]) - 1
  names(frame) <- c('outcome', paste('predictor', 1:npreds, sep='.'))
  return(frame)
}

process_cov_names <- function(vnames) {
  # given covariance effects names, return processed name series
  N <- sqrt(length(vnames))  # number of variables
  
  # remove anything after the period
  stripped <- sapply(vnames, function(x) strsplit(x, '.', fixed=TRUE)[[1]][1])
  
  # split row and column around separator
  split <- strsplit(stripped, ':')
  
  # get row for each entry
  rnames <- sapply(split, function(x) x[1])
  
  return(rnames[1:N])
}

make_cov_mat <- function(X) {
  # given a column matrix with row names, return a reshaped matrix, appropriately named
  N <- sqrt(length(X))
  Xmat <- matrix(X, nrow = N, ncol = N)
  vnames <- process_cov_names(names(X))  
  rownames(Xmat) <- vnames
  colnames(Xmat) <- vnames
  return(Xmat)
}

assemble_cov_mats <- function(X) {
  # given covariance effects from mcmc, return one labeled cov matrix for each
  # column (mean, upper and lower CI, etc.)
  N <- sqrt(nrow(X))
  mlist <- list()
  cnames <- colnames(X)
  for (ind in 1:N) {
    mlist[[cnames[ind]]] <- make_cov_mat(X[, ind])
  }
  return(mlist)  
}

order_scenarios <- function(df, var) {
  # reorder the levels of the predictor variable in df by the outcome var
  # return dataframe
  df$predictor <- droplevels(df$predictor)
  pp <- reorder(df$predictor[se$outcome == var], df$post.mean[se$outcome == var])
  df$predictor <- factor(df$predictor, levels(pp))
  return(df)
}

standardize_heights <- function(...) {
  # given a list of ggplots, make sure the axes line up when plots stacked horizontally
  grob_list <- lapply(..., function(x) {if (is.grob(x)) x else ggplotGrob(x)})  # convert ggplots to grobs
  grob_heights <- lapply(grob_list, function(x) {x$heights[2:5]})  # get heights
  maxHeight <- do.call(grid::unit.pmax, grob_heights)  # take max
  grob_list <- lapply(grob_list, function(x) {x$heights[2:5] <- maxHeight; x}) # reassign heights
}