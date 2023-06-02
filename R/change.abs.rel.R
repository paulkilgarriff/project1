
#function to calculate relative and absolute change
#df - a dataframe
#year1 - first year
#year2 - second year
#var_pre - variable prefix, e.g. pop
#var_post - variable prefix after operation is performed, e.g. chg

calculate.rel.change <- function(df, year1, year2, var_pre) {
  df[paste0("rel_",var_pre,"_",year1, "_", year2)] <- 
    round((100*((df[paste0(var_pre,"_", year2)] - df[paste0(var_pre,"_", year1)]) / df[paste0(var_pre,"_", year1)])),digits=3)
  return(df)
}


calculate.abs.change <- function(df, year1, year2, var_pre) {
  df[paste0("abs_",var_pre,"_",year1, "_", year2)] <- 
    (df[paste0(var_pre,"_", year2)] - df[paste0(var_pre,"_", year1)]) 
  return(df)
}