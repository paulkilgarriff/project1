
rm(list = ls())
install_if_not_present <- function(package_name) {
  if (!require(package_name, character.only = TRUE)) {
    install.packages(package_name)
  }
}

#load packages required
list_packages <- c("csodata","dplyr","tidyverse")

for (i in list_packages) {
  install_if_not_present(i)
  library(i, character.only = TRUE)
}

#Load data from cso website - (This can sometimes take a while to connect)
source("R/load.cso.occupancy.data.R")


##########
#Using data with number of private households by county and city for Census 2022
#And total persons living in private households
#convert all column names to lower case
colnames(cso_no) <- tolower(colnames(cso_no))
#rename column using a shorter variable name
names(cso_no)[names(cso_no) == "nature.of.occupancy"] <- "occup"
cso_no <- cso_no[which(cso_no[["statistic"]]=="Permanent Private Households"),]
#convert data type from factor to character
var_list <- c("census.year","occup","county.and.city","year.built")
cso_no[var_list] <- lapply(cso_no[var_list], as.character)
#rename Dun Laoighaire as issue if not UTF8 format
cso_no[,"county.and.city"] <- ifelse(grepl("laoghai", cso_no[,"county.and.city"], ignore.case = TRUE), "DLR", cso_no[,"county.and.city"])
#get total number of households
cso_no_sum <- cso_no %>%
  group_by(occup,census.year,county.and.city) %>%
  summarise(total1=sum(value))
##
#Total households
all_hh <- cso_no_sum[which(cso_no_sum[["occup"]]=="All types of occupancy"),]
#drop statistic variable only contains single piece of information
all_hh[,"occup"] <- NULL
#rename total households variable
names(all_hh)[names(all_hh) == "total1"] <- "tt_hh"
#Hhs is added before the census year as we do not want a column name starting with a
#numeric character. Also we want to distinguish between pop, hh and perhh later
all_hh[,"census.year"] <- paste("hhs", all_hh[["census.year"]], sep = "_")
#convert from long to wide
all_hh_wide <- all_hh %>%
  spread(census.year, tt_hh)

##
#Total persons in private households
mort_hh <- cso_no_sum[which(cso_no_sum[["occup"]]=="Owner occupied with loan or mortgage"),]
#drop statistic variable only contains single piece of information
mort_hh[,"occup"] <- NULL
#rename total persons in private households variable
names(mort_hh)[names(mort_hh) == "total1"] <- "tt_mort"
#perhh is added before the census year as we do not want a column name starting with a
#numeric character. Also we want to distinguish between pop, hh and perhh later
mort_hh[,"census.year"] <- paste("mort", mort_hh[["census.year"]], sep = "_")
#convert from long to wide
mort_hh_wide <- mort_hh %>%
  spread(census.year, tt_mort)


#combine population and households to get average household size
all_pp_hh <- merge(all_hh_wide,mort_hh_wide,by=("county.and.city"))
#calculate household size
#absolute population, persons and household change
#relative population, persons and household change
years <- c("2011", "2016", "2022")
all_pp_hh[paste0("pct_mort_", years)] <- 100*(all_pp_hh[paste0("mort_", years)] / all_pp_hh[paste0("hhs_", years)])

#get function to calculate relative and absolute change
source("R/change.abs.rel.R")
#Get all different combinations
year1 <- c("2011", "2016", "2011")
year2 <- c("2016", "2022", "2022")
var_pre <- c("hhs","mort")
combinations <- expand.grid(year1 = year1, year2 = year2, var_pre = var_pre,stringsAsFactors = F)
combinations <- distinct(combinations)
combinations <- combinations[which(combinations[["year1"]]!=combinations[["year2"]]),]
#Absolute change
for (j in 1:nrow(combinations)) {
  print(j)
  year1 <- combinations[,"year1"][j]
  year2 <- combinations[,"year2"][j]
  var_pre <- combinations[,"var_pre"][j]
  all_pp_hh <- calculate.abs.change(all_pp_hh,year1,year2,var_pre)
}
#Relative change
for (j in 1:nrow(combinations)) {
  print(j)
  year1 <- combinations[,"year1"][j]
  year2 <- combinations[,"year2"][j]
  var_pre <- combinations[,"var_pre"][j]
  all_pp_hh <- calculate.rel.change(all_pp_hh,year1,year2,var_pre)
}
