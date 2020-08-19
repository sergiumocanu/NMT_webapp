configs_format <- function(raw_configs){
  var_names <- names(raw_configs) # getting the raw names of the field values
  var_values <- unlist(raw_configs) # getting the actual values
  equal_sign <- rep(c("="), times = length(raw_configs)) # making the equal sign by using repeat rep()
  
  
  # concatenating everything in one matrix
  print(matrix(c(var_names, equal_sign, var_values), nrow = length(raw_configs), ncol = 3))
  
}
