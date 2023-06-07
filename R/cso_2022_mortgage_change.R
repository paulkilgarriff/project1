#calculates share of mortgage holders by county and region for 2002,2006,2011,2016 and 2022 Census


rm(list = ls())
install_if_not_present <- function(package_name) {
  if (!require(package_name, character.only = TRUE)) {
    install.packages(package_name)
  }
}

#load packages required
list_packages <- c("csodata","dplyr","tidyverse","reshape")

for (i in list_packages) {
  install_if_not_present(i)
  library(i, character.only = TRUE)
}

#Load data from cso website - (This can sometimes take a while to connect)
source("R/load.cso.occupancy.data.R")

windowsFonts("Times" = windowsFont("Times"))
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
#Mortgage
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

all_hse_2022 <- sum(all_pp_hh[,"hhs_2022"])
mort_hse_2022 <- sum(all_pp_hh[,"mort_2022"])
avg_mort_22 <- 100*(mort_hse_2022/all_hse_2022)


all_pp_hh[,"mort_22_avg"] <- all_pp_hh[,"pct_mort_2022"] - avg_mort_22


#Census 2022 Tenure
cso_tenure_22 <- cso_no_sum[which(cso_no_sum[["census.year"]]=="2022"),]
cso_tenure_22$census.year <- NULL

#convert from long to wide
cso_tenure_22 <- cso_tenure_22 %>%
  spread(occup, total1)

colnames(cso_tenure_22) <- c("county","all","ns","free","mort","occ","public","volun","rent")
cso_tenure_22[,"all"] <- cso_tenure_22[,"all"] - cso_tenure_22[,"ns"]
cso_tenure_22[,"public"] <- cso_tenure_22[,"public"] + cso_tenure_22[,"volun"]
cso_tenure_22[,"owners"] <- cso_tenure_22[,"mort"] + cso_tenure_22[,"occ"]

#share mortgage holders
cso_tenure_22[,"pct_mort"] <- 100*(cso_tenure_22[,"mort"]/cso_tenure_22[,"all"])
#share renters
cso_tenure_22[,"pct_rent"] <- 100*(cso_tenure_22[,"rent"]/cso_tenure_22[,"all"])
#share owner occupiers
cso_tenure_22[,"pct_occ"] <- 100*(cso_tenure_22[,"occ"]/cso_tenure_22[,"all"])
#share owners
cso_tenure_22[,"pct_own"] <- 100*(cso_tenure_22[,"owners"]/cso_tenure_22[,"all"])

#country average
cso_state_22 <- cso_tenure_22[which(cso_tenure_22[["county"]]=="State"),]

#difference to average
cso_tenure_22[,"diff_mort"] <- 100*((cso_tenure_22[,"pct_mort"] - as.numeric(cso_state_22[1,"pct_mort"]))/cso_tenure_22[,"pct_mort"])
cso_tenure_22[,"diff_rent"] <- 100*((cso_tenure_22[,"pct_rent"] - as.numeric(cso_state_22[1,"pct_rent"]))/cso_tenure_22[,"pct_rent"])
cso_tenure_22[,"diff_occ"] <- 100*((cso_tenure_22[,"pct_occ"] - as.numeric(cso_state_22[1,"pct_occ"]))/cso_tenure_22[,"pct_occ"])
cso_tenure_22[,"diff_own"] <- 100*((cso_tenure_22[,"pct_own"] - as.numeric(cso_state_22[1,"pct_own"]))/cso_tenure_22[,"pct_own"])

cso_tenure_22[,"exp_mort"] <- cso_tenure_22[,"all"] * (as.numeric(cso_state_22[1,"pct_mort"])/100)
cso_tenure_22[,"abs_diff_mort"] <-cso_tenure_22[,"mort"] - cso_tenure_22[,"exp_mort"]
  
#write.csv(cso_tenure_22,"Output/tenure_22.csv")

#Census 2022 Tenure
cso_tenure_11 <- cso_no_sum[which(cso_no_sum[["census.year"]]=="2011"),]
cso_tenure_11$census.year <- NULL

#convert from long to wide
cso_tenure_11 <- cso_tenure_11 %>%
  spread(occup, total1)

colnames(cso_tenure_11) <- c("county","all","ns","free","mort","occ","public","volun","rent")
cso_tenure_11[,"all"] <- cso_tenure_11[,"all"] - cso_tenure_11[,"ns"]
cso_tenure_11[,"public"] <- cso_tenure_11[,"public"] + cso_tenure_11[,"volun"]
cso_tenure_11[,"owners"] <- cso_tenure_11[,"mort"] + cso_tenure_11[,"occ"]
#share mortgage holders
cso_tenure_11[,"pct_mort"] <- 100*(cso_tenure_11[,"mort"]/cso_tenure_11[,"all"])
#share renters
cso_tenure_11[,"pct_rent"] <- 100*(cso_tenure_11[,"rent"]/cso_tenure_11[,"all"])
#share owner occupiers
cso_tenure_11[,"pct_occ"] <- 100*(cso_tenure_11[,"occ"]/cso_tenure_11[,"all"])
#share owners
cso_tenure_11[,"pct_own"] <- 100*(cso_tenure_11[,"owners"]/cso_tenure_11[,"all"])
cso_state_11 <- cso_tenure_11[which(cso_tenure_11[["county"]]=="State"),]

cso_all <- rbind(cso_state_11,cso_state_22)

#write.csv(cso_all,"Output/tenure_state_11_22.csv")

#state gda
#Census 2022 Tenure
state_ten <- cso_no_sum[which(cso_no_sum[["county.and.city"]]=="State"),]
state_ten$county.and.city <- NULL
#convert from long to wide
state_ten <- state_ten %>%
  spread(occup, total1)

colnames(state_ten) <- c("year","all","ns","free","mort","occ","public","volun","rent")
state_ten[,"all"] <- state_ten[,"all"] - state_ten[,"ns"]
state_ten[,"public"] <- state_ten[,"public"] + state_ten[,"volun"]
state_ten[,"owners"] <- state_ten[,"mort"] + state_ten[,"occ"]

#calculate percentages
state_ten[,"pct_own"]<- 100*(state_ten[,"occ"]/ state_ten[,"all"])
state_ten[,"pct_mort"]<- 100*(state_ten[,"mort"]/ state_ten[,"all"])
state_ten[,"pct_rent"]<- 100*(state_ten[,"rent"]/ state_ten[,"all"])
state_ten[,"pct_pub"]<- 100*(state_ten[,"public"]/ state_ten[,"all"])
state_pct <- state_ten[c("year","pct_own","pct_mort","pct_rent","pct_pub")]
#reshape data from wide to long
state_pct <- reshape::melt(state_pct, id.vars = "year", variable.name = "pct_new", value.name = "value")

state_pct[,"year"]<- as.numeric(state_pct[,"year"])
# Generate the plot
p <- ggplot(state_pct, aes(x = year, y = value, color = pct_m)) +
  geom_line(linewidth=2) +
  theme_bw() +
  scale_x_continuous(breaks=c(2011,2016,2022))+
  scale_color_manual(values = c("pct_own" = "red", "pct_mort" = "brown", "pct_rent" = "blue",
                               "pct_pub" = "orange"), 
                    labels = c("pct_own" = "Own Outright", "pct_mort" = "Own with Mortgage", 
                               "pct_rent" = "Private Renting","pct_pub" = "Social Housing")) +
  theme(
    text = element_text(family = "Times", color = "black"), # Use Times font
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"), # Centre align the title
    axis.title = element_text(size = 12), # Set axis title size
    axis.text = element_text(size = 10), # Set axis text size
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  labs(
    x = "Census Year",
    y = "Percentage of Households",
    title = "Household percentage by Housing Tenure",
    color = "Housing Tenure",
    caption = "Source: Census 2022 (FY034B)"
  )

# Show the plot
ggsave("Output/housing_tenure.png", plot = p, width = 10, height = 6, dpi = 500)


#change by region
reg_ten <- cso_no_sum
colnames(reg_ten) <- c("occup","year","county","value")

reg_ten <- reg_ten %>%
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


reg_ten <- reg_ten  %>%
  group_by(occup,year,region)%>%
  summarise(value= sum(value))
#examine mortgages
reg_ten <- reg_ten[which(reg_ten[["occup"]]=="All types of occupancy"|
                         reg_ten[["occup"]]=="Owner occupied with loan or mortgage"|
                         reg_ten[["occup"]]=="Not stated"),]

#convert from long to wide
reg_ten <- reg_ten %>%
  spread(occup, value)

colnames(reg_ten) <- c("year","region","all","ns","mort")
reg_ten[,"all"] <- reg_ten[,"all"]-reg_ten[,"ns"]
reg_ten$ns <- NULL
reg_ten[,"pct_mort"]<- 100*(reg_ten[,"mort"]/ reg_ten[,"all"])
reg_ten$year<- as.numeric(reg_ten$year)


# Generate the plot
p <- ggplot(reg_ten, aes(x = year, y = pct_mort, color = region)) +
  geom_line(linewidth=2) +
  theme_bw() +
  scale_x_continuous(breaks=c(2011,2016,2022))+
  scale_y_continuous(breaks=c(26,28,30,32,34,36,38))+
  scale_color_manual(values = c("GDA" = "red", "wGDA" = "brown", "West" = "blue",
                                "South" = "orange","Border" = "pink","Other" = "darkgrey"), 
                     labels = c("GDA" = "Greater Dublin Area", "wGDA" = "Wider GDA", 
                                "West" = "West (incl. Gal, Cla, Lim)","South" = "South",
                                "Border" = "Border","Other" = "Other")) +
  theme(
    text = element_text(family = "Times", color = "black"), # Use Times font
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"), # Centre align the title
    axis.title = element_text(size = 12), # Set axis title size
    axis.text = element_text(size = 10), # Set axis text size
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  labs(
    x = "Census Year",
    y = "Percentage of Households",
    title = "% of Households with a Mortgage by Region",
    color = "Region",
    caption = "Source: Census 2022 (FY034B)"
  )

# Show the plot
ggsave("Output/mortgage_region.png", plot = p, width = 10, height = 6, dpi = 500)


#combine with 2002 and 2006
source("R/mortgage_02_06.R")

reg_ten_02_22 <- rbind(reg_ten,reg_ten_02)
reg_ten_02_22 <- rbind(reg_ten_02_22,reg_ten_06)


# Generate the plot - 2002 to 2022
p_mort0222 <- ggplot(reg_ten_02_22, aes(x = year, y = pct_mort, color = region)) +
  geom_line(linewidth=2) +
  theme_bw() +
  scale_x_continuous(breaks=c(2002,2006,2011,2016,2022))+
  scale_y_continuous(breaks=c(26,28,30,32,34,36,38,40,42,44,46,48))+
  scale_color_manual(values = c("GDA" = "red", "wGDA" = "brown", "West" = "blue",
                                "South" = "orange","Border" = "pink","Other" = "darkgrey"), 
                     labels = c("GDA" = "Greater Dublin Area", "wGDA" = "Wider GDA (contiguity+1)", 
                                "West" = "West (incl. Gal, Cla, Lim)","South" = "South (incl. Cork,Wat)",
                                "Border" = "Border","Other" = "Other")) +
  theme(
    text = element_text(family = "Times", color = "black"), # Use Times font
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"), # Centre align the title
    axis.title = element_text(size = 12), # Set axis title size
    axis.text = element_text(size = 10), # Set axis text size
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  labs(
    x = "Census Year",
    y = "Percentage of Households",
    title = "% of Households with a Mortgage by Region",
    color = "Region",
    caption = "Source: Census 2002,2006,2022 (B1322,C0622,FY034B)"
  )

# Show the plot
ggsave("Output/mortgage_region_02_22.png", plot = p_mort0222, width = 10, height = 6, dpi = 500)

