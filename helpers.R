# includes small helper functions

convert_to_factor <- function(df, which_vars) {
  # convert the variable names in df given in which_vars to factors
  for (vv in which_vars) {
    df[[vv]] <- as.factor(df[[vv]])
  }
  return(df)
}

get_fixed_design_matrix <- function(df, predictors) {
  # given a set of predictors, get the design matrix, assuming all effects are fixed
  # IMPORTANT: since the model contains no constant, the first element of 
  # predictors will have its baseline included
  formula <- as.formula(paste("~ -1 +",paste(predictors, collapse="+"))) 
  X_master <- model.matrix(formula, df)  # matrix of dummy variables for each trial
  return(X_master)
}

reshape_data_by_sub <- function(df, rowvar, unitvar) {
  # given the name of a row variable and a unit variable, reshapes the dataframe 
  # into a multidimensional array (rowvar, cols, unitvar)
  require(reshape2)
  suppressWarnings(melted <- melt(df, id.vars=c(rowvar, unitvar)))
  castform <- as.formula(paste(c(rowvar, 'variable', unitvar), collapse='~'))
  return(acast(melted, castform))
} 

