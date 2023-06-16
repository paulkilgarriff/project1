
library(dplyr)
source("R/load.cso.completions.data.R")
hou_comp_copy <- hou_comp

hou_comp <- hou_comp_copy
#convert all column names to lower case
colnames(hou_comp) <- tolower(colnames(hou_comp))
names(hou_comp)[names(hou_comp) == "censusyear"] <- "year"
names(hou_comp)[names(hou_comp) == "local.authority"] <- "county"
var_list <- c("type.of.house","quarter","statistic","county")
hou_comp[var_list] <- lapply(hou_comp[var_list], as.character)
#rename Dun Laoighaire as issue if not UTF8 format
hou_comp[,"county"] <- ifelse(grepl("laoghai", hou_comp[,"county"], ignore.case = TRUE), "DLR", hou_comp[,"county"])

print(unique(hou_comp$county))

# Replace "Galway County Coun" with "Galway County" in the 'county' variable
hou_comp$county <- gsub("Galway County Council", "Galway County", hou_comp$county)
hou_comp$county <- gsub("Galway City Council", "Galway City", hou_comp$county)
hou_comp$county <- gsub("Dublin City Council", "Dublin City", hou_comp$county)
hou_comp$county <- gsub("Dún Laoghaire Rathdown County Council", "DLR", hou_comp$county)
hou_comp$county <- gsub("Ireland", "State", hou_comp$county)
hou_comp$county <- gsub(" City & County Council", "", hou_comp$county)
hou_comp$county <- gsub(" City Council", "", hou_comp$county)
hou_comp$county <- gsub(" County Council", "", hou_comp$county)
print(unique(hou_comp$county))
#filter using quarters
# Define the vector of quarters
quarters <- c("2016Q2", "2016Q3", "2016Q4", "2017Q1", "2017Q2", "2017Q3", "2017Q4", "2018Q1", "2018Q2",
              "2018Q3", "2018Q4", "2019Q1", "2019Q2", "2019Q3", "2019Q4", "2020Q1", "2020Q2", "2020Q3", 
              "2020Q4", "2021Q1", "2021Q2", "2021Q3", "2021Q4", "2022Q1")

# Filter the dataframe based on the 'quarter' variable
hou_comp <- hou_comp %>% filter(quarter %in% quarters)

hou_comp <- hou_comp %>% filter(statistic %in% c("New Dwelling Completion"))

hou_comp <- hou_comp %>%
  group_by(type.of.house,county)%>%
  summarise(value=sum(value))

reg_pop <- hou_comp %>%
  filter(county != "State") %>%
  mutate(
    county = case_when(
      county %in% c("Carlow","Kilkenny", "Offaly", "Cavan", 
                    "Westmeath", "Louth", "Wexford","Laois") ~ "wGDA",
      county %in% c("Cork", "Waterford", "Kerry") ~ "South",
      county %in% c("Dublin City", "South Dublin", "DLR", "Kildare","Wicklow",
                    "Fingal", "Meath") ~ "GDA",
      county %in% c("Galway County", "Limerick", "Clare", "Galway City") ~ "West",
      county %in% c("Mayo", "Longford", "Roscommon", "Tipperary") ~ "Other",
      county %in% c("Donegal", "Leitrim", "Monaghan", "Sligo") ~ "Border",
      county %in% c("State") ~ "State",
      TRUE ~ NA_character_  # This will assign NA to any county not listed above
    )
  )
reg_pop <- reg_pop %>%
  group_by(county,type.of.house) %>%
  summarise(value=sum(value))
#merge back in
hou_comp <- rbind(hou_comp,reg_pop)

names(hou_comp)[names(hou_comp) == "type.of.house"] <- "var_name"
hou_comp$var_name <- tolower(hou_comp$var_name)
hou_comp$var_name <- gsub("all house types", "all", hou_comp$var_name)
hou_comp$var_name <- gsub("scheme house", "scheme", hou_comp$var_name)
hou_comp$var_name <- gsub("single house", "single", hou_comp$var_name)
hou_comp$var_name <- gsub(" ", "_", hou_comp$var_name)
hou_comp$var_name <- paste0("new_build_",hou_comp$var_name)
hou_comp$temporal <- "static"
hou_comp$magnitude <- "absolute_total"
hou_comp$category <- "new_dwelling_completions"
hou_comp$year <- "2016Q2-2022Q1"
hou_comp <- hou_comp[c("county","year","var_name","magnitude","temporal","value","category")]

write.csv(hou_comp,"Output/cso_house_completions_cty.csv",row.names = FALSE)
