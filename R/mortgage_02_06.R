
#Load mortage data 1996, 2002 and 2006
source("R/load.cso.mort.data.02.R")
source("R/load.cso.mort.data.06.R")

library(ggplot2)
library(dplyr)

#mortgage holders by region

#census 2002
reg_ten_02 <- cso_mort_02
colnames(reg_ten_02) <- c("stat","occup","county","year","value")
var_list <- c("stat","occup","county","year")
reg_ten_02[var_list] <- lapply(reg_ten_02[var_list], as.character)
reg_ten_02 <- reg_ten_02[which(reg_ten_02[["stat"]]=="2002 Private Dwellings in Permanent Housing Units"&
                                 reg_ten_02[["occup"]]=="Owner occupied with loan or mortgage"|
                                 reg_ten_02[["occup"]]=="Not stated"|
                                 reg_ten_02[["occup"]]=="All types of occupancy"),]

reg_ten_02 <- reg_ten_02 %>%
  mutate(
    region = case_when(
      county %in% c("Carlow","Kilkenny", "Offaly", "Cavan", 
                    "Westmeath", "Louth", "Wexford","Laois") ~ "wGDA",
      county %in% c("Cork County","Cork City",  "Waterford County",
                    "Waterford City","Kerry") ~ "South",
      county %in% c("Dublin City", "South Dublin", "DLR", "Kildare","Wicklow",
                    "Fingal", "Meath","Dún Laoghaire-Rathdown") ~ "GDA",
      county %in% c("Galway County", "Limerick City","Limerick County" , "Clare", "Galway City") ~ "West",
      county %in% c("Mayo", "Longford", "Roscommon", "North Tipperary",
                    "South Tipperary") ~ "Other",
      county %in% c("Donegal", "Leitrim", "Monaghan", "Sligo") ~ "Border",
      county %in% c("State") ~ "State",
      TRUE ~ NA_character_  # This will assign NA to any county not listed above
    )
  )
reg_ten_02 <- na.omit(reg_ten_02, reg_ten_02[,"region"])
reg_ten_02 <- reg_ten_02  %>%
  group_by(occup,year,region)%>%
  summarise(value= sum(value))
#convert from long to wide
reg_ten_02 <- reg_ten_02 %>%
  spread(occup, value)
#get share of mortgage holders by region
colnames(reg_ten_02) <- c("year","region","all","ns","mort")
reg_ten_02[,"all"] <- reg_ten_02[,"all"]-reg_ten_02[,"ns"]
reg_ten_02$ns <- NULL
reg_ten_02[,"pct_mort"]<- 100*(reg_ten_02[,"mort"]/ reg_ten_02[,"all"])
reg_ten_02$year<- as.numeric(reg_ten_02$year)

#Census 2006
reg_ten_06 <- cso_mort_06
colnames(reg_ten_06) <- c("stat","occup","county","year","value")
var_list <- c("stat","occup","county","year")
reg_ten_06[var_list] <- lapply(reg_ten_06[var_list], as.character)
reg_ten_06 <- reg_ten_06[which(reg_ten_06[["stat"]]==" Private Dwellings in Permanent Housing Units"&
                                 reg_ten_06[["occup"]]=="Owner occupied with loan or mortgage"|
                                 reg_ten_06[["occup"]]=="Not stated"|
                                 reg_ten_06[["occup"]]=="All types of occupancy"),]

reg_ten_06 <- reg_ten_06 %>%
  mutate(
    region = case_when(
      county %in% c("Carlow","Kilkenny", "Offaly", "Cavan", 
                    "Westmeath", "Louth", "Wexford","Laois") ~ "wGDA",
      county %in% c("Cork County","Cork City",  "Waterford County",
                    "Waterford City","Kerry") ~ "South",
      county %in% c("Dublin City", "South Dublin", "DLR", "Kildare","Wicklow",
                    "Fingal", "Meath","Dún Laoghaire-Rathdown") ~ "GDA",
      county %in% c("Galway County", "Limerick City","Limerick County" , "Clare", "Galway City") ~ "West",
      county %in% c("Mayo", "Longford", "Roscommon", "North Tipperary",
                    "South Tipperary") ~ "Other",
      county %in% c("Donegal", "Leitrim", "Monaghan", "Sligo") ~ "Border",
      county %in% c("State") ~ "State",
      TRUE ~ NA_character_  # This will assign NA to any county not listed above
    )
  )

reg_ten_06 <- na.omit(reg_ten_06, reg_ten_06[,"region"])
reg_ten_06 <- reg_ten_06  %>%
  group_by(occup,year,region)%>%
  summarise(value= sum(value))

#convert from long to wide
reg_ten_06 <- reg_ten_06 %>%
  spread(occup, value)
#get share of mortgage holders by region
colnames(reg_ten_06) <- c("year","region","all","ns","mort")
reg_ten_06[,"all"] <- reg_ten_06[,"all"]-reg_ten_06[,"ns"]
reg_ten_06$ns <- NULL
reg_ten_06[,"pct_mort"]<- 100*(reg_ten_06[,"mort"]/ reg_ten_06[,"all"])
reg_ten_06$year<- as.numeric(reg_ten_06$year)
