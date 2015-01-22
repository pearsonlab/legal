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
