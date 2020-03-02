require(readr)
require(stringr)

defaultConfig <- function(configFilePath){
  # configfilepath <- "RBioFS/connectivity_ml_config"
  
  
  configs <- read_delim(configFilePath, 
                        "\t", escape_double = FALSE, col_names = FALSE, 
                        comment = "#", trim_ws = TRUE, quote = "\"",
                        escape_backslash = FALSE)
  config_values <- sub(".*=", '', configs[[1]])
  config_var_names <- sub("\\=.*", '', configs[[1]])
  
  # for (i in 1:length(config_values)){
  #   print(config_values[i])
  #   str_replace_all(config_values[i], "[\"]")
  #   print(config_values[i])
  # }
  
  
  
  # to do: implement checking the variable existence
  
  varNames <- list('log2_trans')
  print("=========================================")
  # print(config_values)
  
  output <- list(configs, config_values, config_var_names)
  return(output)
}