
rm(list = ls())
install_if_not_present <- function(package_name) {
  if (!require(package_name, character.only = TRUE)) {
    install.packages(package_name)
  }
}

#load packages required
list_packages <- c("csodata","dplyr","tidyverse","ggplot2","scales","reshape2","extrafont")

for (i in list_packages) {
  install_if_not_present(i)
  library(i, character.only = TRUE)
}


#Load data from cso website - (This can sometimes take a while to connect)
source("R/load.cso.hh.data.R")
source("R/load.cso.pop.data.R")

#convert all column names to lower case
colnames(cso_pop) <- tolower(colnames(cso_pop))
names(cso_pop)[names(cso_pop) == "censusyear"] <- "year"
names(cso_pop)[names(cso_pop) == "county.and.city"] <- "county"
var_list <- c("year","sex","statistic","county")
cso_pop[var_list] <- lapply(cso_pop[var_list], as.character)
#rename Dun Laoighaire as issue if not UTF8 format
cso_pop[,"county"] <- ifelse(grepl("laoghai", cso_pop[,"county"], ignore.case = TRUE), "DLR", cso_pop[,"county"])

# Filter the data frame
cso_pop <- cso_pop %>%
  # Remove rows where age is "All ages"
  filter(sex == "Both sexes") %>%
  filter(statistic == "Population") 

#convert all column names to lower case
colnames(cso_hh) <- tolower(colnames(cso_hh))
names(cso_hh)[names(cso_hh) == "census.year"] <- "year"
names(cso_hh)[names(cso_hh) == "county.and.city"] <- "county"
names(cso_hh)[names(cso_hh) == "composition.of.private.household"] <- "comp"
var_list <- c("year","comp","statistic","county")
cso_hh[var_list] <- lapply(cso_hh[var_list], as.character)
#rename Dun Laoighaire as issue if not UTF8 format
cso_hh[,"county"] <- ifelse(grepl("laoghai", cso_hh[,"county"], ignore.case = TRUE), "DLR", cso_hh[,"county"])



couple + children =    "Married couple with children" ,"Cohabiting couple with children"
couple = "Married couple","Cohabiting couple"    
single_p = "One parent mother with children","One parent father with children"
other_person_hhs =  "Married couple with other persons", "Married couple with children and other persons" ,"Cohabiting couple with other persons" ,
"Cohabiting couple with children and other persons","One parent mother with children and other persons", "One parent father with children and other persons"   

multi_fam = "Two family units with/without other persons" ,"Three or more family units with/without other persons"
share = "Non-family households containing related persons","Households comprised of unrelated persons only"


print(unique(cso_hh$comp))
# Filter the data frame
cso_pop <- cso_pop %>%
  # Remove rows where age is "All ages"
  filter(sex == "Both sexes") %>%
  filter(statistic == "Population") 




# Replace "year" or "years" with "" and remove spaces
cso_pop_age <- cso_pop_age %>%
  mutate(age = ifelse(grepl("year", age), gsub("\\s*year[s]*\\s*", "", age), age))
#remove spaces
cso_pop_age$age <- sub("[[:space:]].*", "", cso_pop_age$age)
#convert to numeric
cso_pop_age$age <- as.numeric(cso_pop_age$age)

# Create age_group based on age
cso_pop_age <- cso_pop_age %>%
  mutate(
    age_group = case_when(
      age < 15 ~ "u15",
      age >= 15 & age <= 64 ~ "age15_64",
      age > 64 ~ "o65",
      TRUE ~ "other"  # This will capture any other cases, e.g. NAs
    )
  )

#collapse by census year, county and age group
cso_pop_age <- cso_pop_age %>%
  group_by(year,county,age_group) %>%
  summarise(total1=sum(value))

#Derive Region name using county name
reg_ten <- cso_pop_age %>%
  mutate(
    region = case_when(
      county %in% c("Carlow","Kilkenny", "Offaly", "Cavan", 
                    "Westmeath", "Louth", "Wexford","Laois") ~ "wGDA",
      county %in% c("Cork City and Cork County", "Waterford City and County", "Kerry") ~ "South",
      county %in% c("Dublin City", "South Dublin", "DLR", "Kildare","Wicklow",
                    "Fingal", "Meath") ~ "GDA",
      county %in% c("Galway County", "Limerick City and County", "Clare", "Galway City") ~ "West",
      county %in% c("Mayo", "Longford", "Roscommon", "Tipperary") ~ "Other",
      county %in% c("Donegal", "Leitrim", "Monaghan", "Sligo") ~ "Border",
      county %in% c("State") ~ "State",
      TRUE ~ NA_character_  # This will assign NA to any county not listed above
    )
  )
#sum by region
reg_ten <- reg_ten %>%
  filter(region != "State") %>%
  group_by(year,region,age_group) %>%
  summarise(total1=sum(total1))
#rename variable
names(reg_ten)[names(reg_ten) == "region"] <- "county"
#append to data
# Append to the original dataframe
cso_pop_age <- rbind(cso_pop_age, reg_ten)

##################

# Calculate absolute and relative change and store in new data frame
df_changes <- cso_pop_age %>%
  group_by(county, age_group) %>%
  arrange(year) %>%
  mutate(
    abs_change = round(total1 - lag(total1),digits=0),
    rel_change = round(100*((total1 - lag(total1)) / lag(total1)),digits=2)
  ) %>%
  pivot_longer(c(abs_change, rel_change), names_to = "type", values_to = "change") %>%
  mutate(type1 = ifelse(grepl("abs_change", type), "absolute", "relative"))

df_changes <- df_changes %>%
  # Remove rows where age is "All ages"
  filter(year != "2011")

#rename and order variables
df_changes$year[df_changes$year == "2016"] <- "2011-2016"
df_changes$year[df_changes$year == "2022"] <- "2016-2022"
df_changes$age_group[df_changes$age_group == "u15"] <- "age_under15"
df_changes$age_group[df_changes$age_group == "o65"] <- "age_over65"
df_changes$age_group[df_changes$age_group == "age15_64"] <- "age_15_64"
#
names(df_changes)[names(df_changes) == "age_group"] <- "var_name"
names(df_changes)[names(df_changes) == "change"] <- "value"
names(df_changes)[names(df_changes) == "type1"] <- "magnitude"
df_changes$temporal <- "change"

df_changes <- df_changes[c("county","year","var_name","magnitude","temporal","value")]


#get age ratios
df_ratios <- cso_pop_age %>%
  group_by(county, year) %>%
  summarise(
    age_over65 = (total1[age_group == "o65"]) / (total1[age_group == "age15_64"]),
    age_under15 = (total1[age_group == "u15"]) / (total1[age_group == "age15_64"])
  )

long_df_ratios <- df_ratios %>%
  tidyr::pivot_longer(
    cols = c(age_over65, age_under15),
    names_to = "var_name",
    values_to = "value"
  )

#
names(df_changes)[names(df_changes) == "age_group"] <- "var_name"
names(df_changes)[names(df_changes) == "change"] <- "value"
long_df_ratios$magnitude <- "ratio"
long_df_ratios$temporal <- "static"
long_df_ratios$value <- round(100*(long_df_ratios$value),digits=2)
long_df_ratios <- long_df_ratios[c("county","year","var_name","magnitude","temporal","value")]

#And now values
cso_pop_age_val <- cso_pop_age
cso_pop_age_val$age_group[cso_pop_age_val$age_group == "u15"] <- "age_under15"
cso_pop_age_val$age_group[cso_pop_age_val$age_group == "o65"] <- "age_over65"
cso_pop_age_val$age_group[cso_pop_age_val$age_group == "age15_64"] <- "age_15_64"
names(cso_pop_age_val)[names(cso_pop_age_val) == "age_group"] <- "var_name"
names(cso_pop_age_val)[names(cso_pop_age_val) == "total1"] <- "value"
cso_pop_age_val$magnitude <- "absolute total"
cso_pop_age_val$temporal <- "static"

#bring all together
cso_pop_age <- rbind(cso_pop_age_val,long_df_ratios)
cso_pop_age <- rbind(cso_pop_age_val,df_changes)

write.csv(cso_pop_age,"Output/census_22_pop_age_cty.csv")
