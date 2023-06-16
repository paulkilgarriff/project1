
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
cso_pop <- cso_pop[c(1,2,4,5)]
reg_pop <- cso_pop %>%
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
reg_pop <- reg_pop %>%
  group_by(statistic,year,county) %>%
  summarise(value=sum(value))
#merge back in
cso_pop <- rbind(cso_pop,reg_pop)

#convert all column names to lower case
colnames(cso_hh) <- tolower(colnames(cso_hh))
names(cso_hh)[names(cso_hh) == "census.year"] <- "year"
names(cso_hh)[names(cso_hh) == "county.and.city"] <- "county"
names(cso_hh)[names(cso_hh) == "composition.of.private.household"] <- "comp"
var_list <- c("year","comp","statistic","county")
cso_hh[var_list] <- lapply(cso_hh[var_list], as.character)
#rename Dun Laoighaire as issue if not UTF8 format
cso_hh[,"county"] <- ifelse(grepl("laoghai", cso_hh[,"county"], ignore.case = TRUE), "DLR", cso_hh[,"county"])

#Derive Region name using county name
cso_hh <- cso_hh %>%
  mutate(
    hs_comp = case_when(
      comp %in% c("One person") ~ "sing",
      comp %in% c("Married couple with children" ,"Cohabiting couple with children") ~ "coup_kid",
      comp %in% c("Married couple","Cohabiting couple") ~ "coup",
      comp %in% c("One parent mother with children","One parent father with children") ~ "lone_kid",
      comp %in% c( "Married couple with other persons", "Married couple with children and other persons" ,
                   "Cohabiting couple with children and other persons","One parent mother with children and other persons", "One parent father with children and other persons",   
                   "Cohabiting couple with other persons" ) ~ "fam_oth",
      comp %in% c("Two family units with/without other persons" ,"Three or more family units with/without other persons") ~ "fam_mult",
      comp %in% c("Non-family households containing related persons","Households comprised of unrelated persons only") ~ "h_shr",
      TRUE ~ NA_character_  # This will assign NA to any county not listed above
    )
  )

cso_hh <- cso_hh %>%
  group_by(statistic,year,county,hs_comp) %>%
  summarise(value=sum(value))
  #get total
cso_all <- cso_hh %>%
  group_by(statistic,year,county) %>%
  summarise(value=sum(value))
cso_all$hs_comp <- "all"
#merge back in
cso_hh <- rbind(cso_hh,cso_all)

#Derive Region name using county name
reg_ten <- cso_hh %>%
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
reg_ten <- reg_ten %>%
  group_by(statistic,year,county,hs_comp) %>%
  summarise(value=sum(value))
#merge back in
cso_hh <- rbind(cso_hh,reg_ten)


#cso_hh
cso_hh <- cso_hh %>%
  mutate(
    statistic = case_when(
      statistic %in% c("All persons in private households") ~ "pers_hh",
      statistic %in% c("Total private households") ~ "tt_hhs",
      TRUE ~ NA_character_  # This will assign NA to any county not listed above
    )
  )

cso_hh$var_name <- paste0(cso_hh$hs_comp,"_",cso_hh$statistic)
df_hh <- cso_hh
df_hh$magnitude <- "absolute total"
df_hh$temporal <- "static"
df_hh <- df_hh[c("county","year","var_name","magnitude","temporal","value")]
df_hh$year  <- as.character(df_hh$year)
cso_hh$year <- as.numeric(cso_hh$year)
# Calculate absolute and relative change and store in new data frame
df_changes <- cso_hh %>%
  group_by(county, hs_comp, var_name) %>%
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


#population
df_pop <- cso_pop
df_pop$magnitude <- "absolute total"
df_pop$temporal <- "static"
df_pop$var_name <- "population"
df_pop <- df_pop[c("county","year","var_name","magnitude","temporal","value")]

cso_pop$year <- as.numeric(cso_pop$year)
# Calculate absolute and relative change and store in new data frame
df_pchanges <- cso_pop %>%
  group_by(county) %>%
  arrange(year) %>%
  mutate(
    abs_change = round(value - lag(value),digits=0),
    rel_change = round(100*((value - lag(value)) / lag(value)),digits=2)
  ) %>%
  pivot_longer(c(abs_change, rel_change), names_to = "type", values_to = "change") %>%
  mutate(type1 = ifelse(grepl("abs_change", type), "absolute", "relative"))

df_pchanges <- df_pchanges %>%
  # Remove rows where age is "All ages"
  filter(year != 2006)

df_pchanges$value <- NULL
#rename and order variables
df_pchanges$year[df_pchanges$year == "2016"] <- "2011-2016"
df_pchanges$year[df_pchanges$year == "2022"] <- "2016-2022"
names(df_pchanges)[names(df_pchanges) == "change"] <- "value"
names(df_pchanges)[names(df_pchanges) == "type1"] <- "magnitude"
df_pchanges$temporal <- "change"
df_pchanges$var_name <- "population"
df_pchanges <- df_pchanges[c("county","year","var_name","magnitude","temporal","value")]


#bring all together
cso_pop_hh_all <- rbind(df_hh,df_changes)
cso_pop_hh_all <- rbind(cso_pop_hh_all,df_pop)
cso_pop_hh_all <- rbind(cso_pop_hh_all,df_pchanges)

write.csv(cso_pop_hh_all,"Output/census_22_hh_fam_pop_cty.csv")
