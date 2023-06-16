df1 <- read.csv("Output/census_22_hse_type_cty.csv", row.names = NULL)
df2 <- read.csv("Output/census_22_hh_fam_pop_cty.csv", row.names = NULL)
df3 <- read.csv("Output/census_22_pop_age_cty.csv", row.names = NULL)
df4 <- read.csv("Output/cso_house_completions_cty.csv", row.names = NULL)

# Define the function
remove_column_if_exists <- function(df, colname) {
  if (colname %in% names(df)) {
    df[[colname]] <- NULL
  }
  return(df)
}

# Use the function on df1 and df2
df1 <- remove_column_if_exists(df1, "X")
df2 <- remove_column_if_exists(df2, "X")
df3 <- remove_column_if_exists(df3, "X")
df4 <- remove_column_if_exists(df4, "X")
library(dplyr)
#fix df1
df1 <- df1 %>%
  mutate(
    category = case_when(
      var_name %in% c("All types of sewerage","Individual septic tank","Individual treatment not septic tank","No sewerage facility","Not stated","Other type of sewage", "Public scheme" ) ~ "sewerage",
      var_name %in% c("built_all", "built_none","built_p19_70","built_p2016","built_y70_90","built_y91_15") ~ "built",
      var_name %in% c("tenure_all", "tenure_mort", "tenure_other", "tenure_owner", "tenure_rent", "tenure_social") ~ "tenure",
      var_name %in% c("house_all", "house_apart", "house_detach", "house_none", "house_scheme") ~ "house_type",
      TRUE ~ NA_character_  # This will assign NA to any county not listed above
    )
  )

#fix df1
df2 <- df2 %>%
  mutate(
    category = case_when(
      var_name %in% c("coup_pers_hh", "coup_kid_pers_hh", "fam_mult_pers_hh", "fam_oth_pers_hh", "h_shr_pers_hh",
                      "lone_kid_pers_hh", "sing_pers_hh","all_pers_hh") ~ "family_persons",
      var_name %in% c("coup_tt_hhs", "coup_kid_tt_hhs", "fam_mult_tt_hhs",
                      "fam_oth_tt_hhs", "h_shr_tt_hhs", "lone_kid_tt_hhs", "sing_tt_hhs","all_tt_hhs") ~ "family_households",
      var_name %in% c("population") ~ "family_population",
      TRUE ~ NA_character_  # This will assign NA to any county not listed above
    )
  )

#fix df1
df3 <- df3 %>%
  mutate(
    category = case_when(
      var_name %in% c("age_15_64", "age_over65", "age_under15") ~ "age",
      TRUE ~ NA_character_  # This will assign NA to any county not listed above
    )
  )



all_df <- rbind(df1,df2)
all_df <- rbind(all_df,df3)
all_df <- rbind(all_df,df4)
all_df$X.1 <- NULL
all_df$X <- NULL

write.csv(all_df, "Output/census_22_all_cty.csv", row.names = FALSE)
print(unique(all_df$county))
print(unique(all_df$year))
print(unique(all_df$var_name))
print(unique(all_df$magnitude))
print(unique(all_df$temporal))
print(unique(all_df$category))
