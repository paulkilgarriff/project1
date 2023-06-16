
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
source("R/load.cso.house.data.22.R")

#
data_list <- c("hou_age","hs_type","sewer","tenure")
#convert names to lower
# Function to process each dataframe
process_dataframe <- function(df) {
  # Convert all column headers to lower case
  names(df) <- tolower(names(df))
  names(df)[names(df) == "census.year"] <- "year"
  names(df)[names(df) == "county.and.city"] <- "county"
  
  # Convert all variables to as.character except if the name is "value"
  df[] <- lapply(names(df), function(x) {
    if (x != "value") {
      return(as.character(df[[x]]))
    }
    df[[x]]
  })
  df[,"county"] <- ifelse(grepl("laoghai", df[,"county"], ignore.case = TRUE), "DLR", df[,"county"])
  
  df
}
for (i in data_list) {
  df1 <- get(i)
  df1 <- process_dataframe(df1)
  assign(i, df1, envir = .GlobalEnv)
}

# Apply the function to each dataframe in the list
names(hou_age)[names(hou_age) == "year.built"] <- "var_name"
names(hs_type)[names(hs_type) == "type.of.private.accommodation"] <- "var_name"
names(sewer)[names(sewer) == "type.of.sewerage.facility"] <- "var_name"
names(tenure)[names(tenure) == "nature.of.occupancy"] <- "var_name"

# Filter the data frame
hs_type <- hs_type %>%
  filter(period.in.which.built == "All years")
hs_type$period.in.which.built<-NULL
sewer <- sewer %>%
  filter(year.built == "All years")
sewer$year.built <- NULL
tenure <- tenure %>%
  filter(year.built == "All years") 
tenure$year.built <- NULL

print(unique(hou_age$var_name))
#create new groups for year_built
hou_age <- hou_age %>%
  filter(statistic == "Permanent Private Households") %>%
  mutate(
    var_name = case_when(
      var_name %in% c("All years") ~ "built_all",
      var_name %in% c( "Before 1919", "1919 to 1945", "1946 to 1960","1961 to 1970") ~ "built_p19_70",
      var_name %in% c("1971 to 1980","1981 to 1990") ~ "built_y70_90",
      var_name %in% c("1991 to 2000","2001 to 2010","2011 to 2015" ) ~ "built_y91_15",
      var_name %in% c("2016 or later") ~ "built_p2016",
      var_name %in% c("Not stated") ~ "built_none",
      TRUE ~ NA_character_  # This will assign NA to any county not listed above
    )
  )
#create new groups for tenure
tenure <- tenure %>%
  mutate(
    var_name = case_when(
      var_name %in% c("All types of occupancy") ~ "tenure_all",
      var_name %in% c( "Owner occupied without loan or mortgage") ~ "tenure_owner",
      var_name %in% c("Owner occupied with loan or mortgage" ) ~ "tenure_mort",
      var_name %in% c("Rented from private landlord" ) ~ "tenure_rent",
      var_name %in% c("Rented from a local authority","Rented from a voluntary body") ~ "tenure_social",
      var_name %in% c("Occupied free of rent","Not stated") ~ "tenure_other",
      TRUE ~ NA_character_  # This will assign NA to any county not listed above
    )
  )
#create new groups for house type
hs_type <- hs_type %>%
  mutate(
    var_name = case_when(
      var_name %in% c("All households") ~ "house_all",
      var_name %in% c( "Detached house" ) ~ "house_detach",
      var_name %in% c("Semi- detached house","Terraced house") ~ "house_scheme",
      var_name %in% c("Flat or apartment in a purpose- built block","Flat or apartment in a converted house or commercial building","Bed-sit") ~ "house_apart",
      var_name %in% c("Not stated") ~ "house_none",
      TRUE ~ NA_character_  # This will assign NA to any county not listed above
    )
  )

#bind together

all_hou <- rbind(hou_age,hs_type)
all_hou <- rbind(all_hou,sewer)
all_hou <- rbind(all_hou,tenure)
#summarise by group
all_hou <- all_hou %>%
  group_by(year,county,var_name) %>%
  summarise(value=sum(value))
#now get regions data
reg_summary <- all_hou %>%
  filter(county != "State") %>%
  mutate(
    county = case_when(
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
reg_summary <- reg_summary %>%
  group_by(year,county,var_name) %>%
  summarise(value=sum(value))
#bind back into main data
all_hou <- rbind(all_hou,reg_summary)

#get static absolute totals
df_hh <- all_hou
df_hh$magnitude <- "absolute total"
df_hh$temporal <- "static"
df_hh <- df_hh[c("county","year","var_name","magnitude","temporal","value")]

#calculate changes
df_changes <- all_hou
df_changes$year <- as.numeric(df_changes$year)
# Calculate absolute and relative change and store in new data frame
df_changes <- df_changes %>%
  group_by(county, var_name) %>%
  arrange(year) %>%
  mutate(
    abs_change = round(value - lag(value),digits=0),
    rel_change = round(100*((value - lag(value)) / lag(value)),digits=2)
  ) %>%
  pivot_longer(c(abs_change, rel_change), names_to = "type", values_to = "change") %>%
  mutate(type1 = ifelse(grepl("abs_change", type), "absolute", "relative"))

df_changes <- df_changes %>%
  # Remove rows where age is "All ages"
  filter(year != 2011)

df_changes$value <- NULL
#rename and order variables
df_changes$year[df_changes$year == "2016"] <- "2011-2016"
df_changes$year[df_changes$year == "2022"] <- "2016-2022"
names(df_changes)[names(df_changes) == "change"] <- "value"
names(df_changes)[names(df_changes) == "type1"] <- "magnitude"
df_changes$temporal <- "change"

df_changes <- df_changes[c("county","year","var_name","magnitude","temporal","value")]

#bring all together and save
cso_house_all <- rbind(df_hh,df_changes)

write.csv(cso_house_all,"Output/census_22_hse_type_cty.csv")
