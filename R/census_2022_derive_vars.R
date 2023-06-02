
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
source("R/load.cso.hh.data.R")
source("R/load.cso.pop.data.R")

##########
#Using data with number of private households by county and city for Census 2022
#And total persons living in private households
#convert all column names to lower case
colnames(cso_hh) <- tolower(colnames(cso_hh))
#rename column using a shorter variable name
names(cso_hh)[names(cso_hh) == "composition.of.private.household"] <- "comp"
#convert data type from factor to character
var_list <- c("census.year","comp","statistic","county.and.city")
cso_hh[var_list] <- lapply(cso_hh[var_list], as.character)
#rename Dun Laoighaire as issue if not UTF8 format
cso_hh[,"county.and.city"] <- ifelse(grepl("laoghai", cso_hh[,"county.and.city"], ignore.case = TRUE), "DLR", cso_hh[,"county.and.city"])
#get total number of households
cso_hh <- cso_hh %>%
  group_by(statistic,census.year,county.and.city) %>%
  summarise(total1=sum(value))
##
#Total households
hhs_22 <- cso_hh[which(cso_hh[["statistic"]]=="Total private households"),]
#drop statistic variable only contains single piece of information
hhs_22[,"statistic"] <- NULL
#rename total households variable
names(hhs_22)[names(hhs_22) == "total1"] <- "tt_hh"
#Hhs is added before the census year as we do not want a column name starting with a
#numeric character. Also we want to distinguish between pop, hh and perhh later
hhs_22[,"census.year"] <- paste("hhs", hhs_22[["census.year"]], sep = "_")
#convert from long to wide
hhs_22_wide <- hhs_22 %>%
  spread(census.year, tt_hh)
##
#Total persons in private households
perhh_22 <- cso_hh[which(cso_hh[["statistic"]]=="All persons in private households"),]
#drop statistic variable only contains single piece of information
perhh_22[,"statistic"] <- NULL
#rename total persons in private households variable
names(perhh_22)[names(perhh_22) == "total1"] <- "tt_pershh"
#perhh is added before the census year as we do not want a column name starting with a
#numeric character. Also we want to distinguish between pop, hh and perhh later
perhh_22[,"census.year"] <- paste("perhh", perhh_22[["census.year"]], sep = "_")
#convert from long to wide
perhh_22_wide <- perhh_22 %>%
  spread(census.year, tt_pershh)


#########
#Population data -cso_pop
#convert all column names to lower case
colnames(cso_pop) <- tolower(colnames(cso_pop))
#rename census year so column names are the same as cso_hh
names(cso_pop)[names(cso_pop) == "censusyear"] <- "census.year"
#convert data type from factor to character
var_list <- c("census.year","sex","statistic","county.and.city")
cso_pop[var_list] <- lapply(cso_pop[var_list], as.character)
#subset data - interested in all persons and population only
cso_pop <- cso_pop[which(cso_pop[["sex"]]=="Both sexes"),]
cso_pop <- cso_pop[which(cso_pop[["statistic"]]=="Population"),]
#drop columns no longer required
cso_pop <- cso_pop[, !(names(cso_pop) %in% c("statistic","sex"))]
#rename Dun Laoighaire as fada causes issue with not UTF8 format
cso_pop[,"county.and.city"] <- ifelse(grepl("laoghai", cso_pop[,"county.and.city"], ignore.case = TRUE), "DLR", cso_pop[,"county.and.city"])
names(cso_pop)[names(cso_pop) == "value"] <- "pop"
#Pop is added before the census year as we do not want a column name starting with a
#numeric character
cso_pop[,"census.year"] <- paste("pop", cso_pop[["census.year"]], sep = "_")
#convert from long to wide
cso_pop_wide <- cso_pop %>%
  spread(census.year, pop)

#combine population and households to get average household size
all_pp_hh <- merge(hhs_22_wide,perhh_22_wide,by=("county.and.city"))
all_pp_hh <- merge(all_pp_hh,cso_pop_wide,by=("county.and.city"))
#calculate household size
#absolute population, persons and household change
#relative population, persons and household change
years <- c("2011", "2016", "2022")
all_pp_hh[paste0("hhsize_", years)] <- all_pp_hh[paste0("perhh_", years)] / all_pp_hh[paste0("hhs_", years)]

#get function to calculate relative and absolute change
source("R/change.abs.rel.R")
#Get all different combinations
year1 <- c("2011", "2016", "2011")
year2 <- c("2016", "2022", "2022")
var_pre <- c("hhs","perhh","pop")
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
#save dataset
write.csv(all_pp_hh,"Output/census_22_pop_hhs_cty.csv")

#Add additional column for NUTS3 regions

all_pp_hh$region <- ifelse(all_pp_hh$County.and.City %in% c("South Dublin", "Wicklow", "Kildare", "Dublin City", "Meath", "Fingal", "Dún Laoghaire-Rathdown"), "GDA", NA)
all_pp_hh$hh_size <- all_pp_hh$pop/all_pp_hh$hhs


#estimate change in number and percentage of mortgage holders per county
#look at number of new houses 2016 to 2022
#type of houses - one-off, apartment, scheme
#average size of house - construction costs
#average county income

#using SAPS 2016 - identify clusters where there is increasing population, above average income
#but below average mortgage holders

#I want to show why local house prices are needed.

#affordability is particularly problematic, which could inform housing and social policies.
#assessing current national policy targets - feasibility given viability issues

#macroprudential - identify areas that have weak demand
#use ratio of construction costs to market price as a proxy for market strength
#Examine LTV levels by area - is there any spatial patterns in the data
#Are there clusters of high or low LTV rates
#Cross examine these clusters with ratio of housing demand
#High LTV in areas wit strong demand carries less risk than high LTV in areas
#where demand is poor.

#Assess negative equity risk by area - 

#particular issue with construction costs and market value in poor viability areas.
#How is "value" estimated? Compare bank "value" with model "value". Examine if there
#is potentially over or under valuations.

#trend 2008 to 2016 showed Dublin bounced back faster.

#reduce risk with self-builds, building defects, control over quality - more risk
#construction cost limits - additional limits on price per m2 of construction
#construction costs often underestimated.
#with xxx self-builds per year - an estimated xx in finance
#poor data on construction costs, valuations??
#Negative equity risks
#or under po



gda <- all_pp_hh[ which(all_pp_hh$region=="GDA"),]

gda_tt <- gda %>%
  group_by(Census.Year) %>%
  summarise(tt_pop = sum(pop),
            tt_hhs = sum(hhs))
gda_tt$hh_size <- gda_tt$tt_pop/gda_tt$tt_hhs

library(ggplot2)

# Load ggplot2
library(ggplot2)

# Create the histogram
ggplot(gda, aes(x = County.and.City, y = hh_size, fill = factor(Census.Year))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "County and City", y = "Households", fill = "Census Year") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Load necessary libraries
library(dplyr)
library(tidyr)

#change in pop
df <- gda %>%
  arrange(County.and.City, Census.Year) %>%
  group_by(County.and.City) %>%
  mutate(perc_pop = (pop - lag(pop)) / lag(pop) * 100,
         perc_hhs = (hhs - lag(hhs)) / lag(hhs) * 100,
         perc_change = (hh_size - lag(hh_size)) / lag(hh_size) * 100)


df_wide <- df5 %>%
  tidyr::pivot_wider(names_from = Census.Year, values_from = pop)

df5 <- spread(df5, key = County.and.City, value = pop)
df5 <- df5 %>%
  group_by(County.and.City) %>%
  summarise(pop_11 = max(2011))

df5 <- all_pp_hh %>%
  arrange(County.and.City, Census.Year) %>%
  group_by(County.and.City) %>%
  mutate(pop_chang = (pop - lag(pop)),
         hhs_change = (hhs - lag(hhs)))
df5 <- df5[which(df5$Census.Year=="2022"),]
write.csv(df5,"Output/test3.csv")

df <- gda %>%
  arrange(County.and.City, Census.Year) %>%
  group_by(County.and.City) %>%
  mutate(perc_pop = (pop - lag(pop)) ,
         perc_hhs = (hhs - lag(hhs)) ,
         perc_change = (hh_size - lag(hh_size)) )
df <- df[which(df$Census.Year=="2022"),]
write.csv(df,"Output/test2.csv")

df <- df[which(df$Census.Year!="2011"),]
# Create the histogram
ggplot(df, aes(x = County.and.City, y = perc_change, fill = factor(Census.Year))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "County and City", y = "Households", fill = "Census Year") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(df, aes(x = County.and.City, y = perc_pop, fill = factor(Census.Year))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Local Authority", y = "% Change in Population", fill = "Census Year") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(df, aes(x = County.and.City, y = perc_hhs, fill = factor(Census.Year))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Local Authority", y = "% Change in Households", fill = "Census Year") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  scale_y_continuous(limits = c(0,15))

# Create the histogram
ggplot(df, aes(x = County.and.City, y = perc_change, fill = factor(Census.Year))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "County and City", y = "Households", fill = "Census Year") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


df2 <- df[which(df$Census.Year=="2022"),]


ggplot(df2) +
  geom_bar(aes(x = County.and.City, y = perc_hhs,fill="hhs"), stat = "identity", position = "dodge") +
  geom_bar(aes(x = County.and.City, y = perc_pop,fill="pop"), stat = "identity", position = "dodge") +
  labs(x = "Local Authority", y = "% Change", fill = "Census Year") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(limits = c(0,15))

# Grouped bar plot
# Grouped bar plot
df2_long <- df2 %>%
  tidyr::pivot_longer(cols = c(perc_hhs, perc_pop), names_to = "variable", values_to = "value") %>%
  mutate(variable = recode(variable, perc_hhs = "Households", perc_pop = "Population"))

p <- ggplot(df2_long, aes(x = County.and.City, y = value, fill = variable)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Local Authority", y = "% Change in Households", fill = "Census 2022") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(limits = c(0,15))

ggsave("Output/myplot.png", plot = p, width = 10, height = 10, dpi = 300)
