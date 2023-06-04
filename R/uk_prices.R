


uk_price <- read.csv("D:/TELE-Sim/House_prices/UK/UK-UCL-house-prices/tranall2011_19.csv")


#propertytype  and propertytype_epc

#House or apartment
uk_price[,"house"] <- 0
uk_price[,"house"][uk_price[,"propertytype"] %in% c("D", "S", "T")] <- 1

uk_price[,"apart"] <- 0
uk_price[,"apart"][uk_price[,"propertytype"] %in% c("F")] <- 1

uk_price[,"flat"] <- 0
uk_price[,"flat"][uk_price[,"propertytype_epc"] %in% c("Flat")] <- 1

uk_price[,"house_g"] <- 0
uk_price[,"house_g"][uk_price[,"propertytype_epc"] %in% c("House")] <- 1

uk_price[,"bunga"] <- 0
uk_price[,"bunga"][uk_price[,"propertytype_epc"] %in% c("Bungalow")] <- 1

uk_price[,"duplex"] <- 0
uk_price[,"duplex"][uk_price[,"propertytype_epc"] %in% c("Maisonette")] <- 1

uk_price[,"parkh"] <- 0
uk_price[,"parkh"][uk_price[,"propertytype_epc"] %in% c("Park home")] <- 1

uk_price[,"penthouse"] <- 0
uk_price[,"penthouse"][uk_price[,"FLAT_TOP_STOREY"] %in% c("Y")] <- 1

uk_price[,"grd_flat"] <- 0
uk_price[,"grd_flat"][uk_price[,"FLOOR_LEVEL"] %in% c("Ground")] <- 1

uk_price[,"semi_d"] <- 0
uk_price[,"semi_d"][uk_price[,"BUILT_FORM"] %in% c("Semi-Detached")] <- 1

uk_price[,"detached"] <- 0
uk_price[,"detached"][uk_price[,"BUILT_FORM"] %in% c("Detached")] <- 1

uk_price[,"terrace"] <- 0
uk_price[,"terrace"][uk_price[,"BUILT_FORM"] %in% c("Enclosed Mid-Terrace","Mid-Terrace")] <- 1

uk_price[,"endterrace"] <- 0
uk_price[,"endterrace"][uk_price[,"BUILT_FORM"] %in% c("End-Terrace","Enclosed End-Terrace")] <- 1

#oldnew
uk_price[,"new"] <- 0
uk_price[,"new"][uk_price[,"oldnew"] %in% c("Y")] <- 1

#duration
uk_price[,"freehold"] <- 0
uk_price[,"freehold"][uk_price[,"duration"] %in% c("F")] <- 1

#region
uk_price[,"london"] <- 0
uk_price[,"london"][uk_price[,"rgn11nm"] %in% c("London")] <- 1

uk_price[,"midlands"] <- 0
uk_price[,"midlands"][uk_price[,"rgn11nm"] %in% c("East Midlands","West Midlands")] <- 1

uk_price[,"south"] <- 0
uk_price[,"south"][uk_price[,"rgn11nm"] %in% c("South East","South West")] <- 1

uk_price[,"north"] <- 0
uk_price[,"north"][uk_price[,"rgn11nm"] %in% c("North East","North West")] <- 1

uk_price[,"wales"] <- 0
uk_price[,"wales"][uk_price[,"rgn11nm"] %in% c("Wales")] <- 1

uk_price[,"east"] <- 0
uk_price[,"east"][uk_price[,"rgn11nm"] %in% c("East of England")] <- 1

uk_price[,"york"] <- 0
uk_price[,"york"][uk_price[,"rgn11nm"] %in% c("Yorkshire and The Humber")] <- 1

#year
yr_list <- c("2014", "2017", "2013", "2018", "2015", "2019", "2012", "2016", "2011")
for (i in yr_list) {
  yr_name <- paste0("yr_",i)
  uk_price[,yr_name] <- 0
  uk_price[,yr_name][uk_price[,"year"] %in% c(i)] <- 1
}

#Area and Rooms
uk_price[,"size_m2"] <-  uk_price[,"tfarea"] 
uk_price[,"no_rooms"] <-  uk_price[,"numberrooms"] 

#energy rating
ener_list <- c("A", "B", "C", "D", "E", "F", "G")
for (j in ener_list) {
  ener_name <- paste0("eng_",j)
  uk_price[,ener_name] <- 0
  uk_price[,ener_name][uk_price[,"CURRENT_ENERGY_RATING"] %in% c(j)] <- 1
}

#transaction type
uk_price[,"sale"] <- 0
uk_price[,"sale"][uk_price[,"TRANSACTION_TYPE"] %in% c("marketed sale")] <- 1

#condition
uk_price[,"cond_good"] <- 0
uk_price[,"cond_good"][uk_price[,"WALLS_ENERGY_EFF"] %in% c("Good","Very Good")] <- 1


#subset variables
uk_sales <- uk_price[which(uk_price[["sale"]]==1),]
uk_sales <- uk_price[which(uk_price[["freehold"]]==1),]

uk_sales <- uk_sales[c("price", "house", "apart", "penthouse", "grd_flat", "semi_d", 
           "detached", "terrace", "endterrace", "new", "freehold", "london", 
           "midlands", "south", "north", "wales", "east", "york", 
           "yr_2019", "yr_2018", "yr_2017", "size_m2", "no_rooms", 
           "eng_A", "eng_B", "eng_C", "eng_D", "eng_E", "eng_F", "eng_G", "cond_good")]

#estimate rooms
# Compute robust standard errors
library(sandwich)
library(lmtest)

# Assuming 'data' is your dataframe containing the variables
model <- glm(no_rooms ~ price + house + apart + detached + terrace + endterrace + penthouse +
               grd_flat + new + london + midlands + south + north + wales + east + york +
               yr_2019 + yr_2018 + size_m2 + eng_A + eng_B + eng_C + eng_D + eng_E + eng_F +
               eng_G + cond_good, family = poisson(), data = uk_sales)

coeftest(model, vcov = vcovHC(model, type = "HC"))

uk_sales[,"no_rooms_predicted"] <- predict(model, type = "response")
