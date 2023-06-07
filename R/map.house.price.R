rm(list = ls())

source("R/load.cso.rppi.eir.R")

library(sf)
library(stringr)
library(ggplot2)
library(ggspatial)
library(BAMMtools)

#examine median and latest month only
colnames(hp_eir) <- tolower(colnames(hp_eir))
#factor to character
var_list <- c("statistic","month","dwelling.status","eircode.output",
              "stamp.duty.event","type.of.buyer")
hp_eir[var_list] <- lapply(hp_eir[var_list], as.character)
#subset data
hp_eir_sub <- hp_eir[which(hp_eir[["statistic"]]=="Moving 12 Month Median Sale Price" &
                             hp_eir[["month"]]=="2023 March" & 
                             hp_eir[["dwelling.status"]]=="New" & 
                             hp_eir[["stamp.duty.event"]]=="Executions" & 
                             hp_eir[["type.of.buyer"]]=="Household Buyer - First-Time Buyer Owner-Occupier"),]
#get national median
nat_med <- hp_eir_sub[which(hp_eir_sub[["eircode.output"]]=="All" ),]
nat_med <- as.numeric(nat_med[,"value"])

#edit eircode just need first three digits
hp_eir_sub[,"eircode"]<-str_sub(hp_eir_sub[,"eircode.output"],end=3)
hp_eir_sub <- hp_eir_sub[c("eircode","value")]
hp_eir_sub[,"value"] <- as.numeric(hp_eir_sub[,"value"])
hp_eir_sub[,"rat_value"] <- hp_eir_sub[,"value"]/nat_med
hp_eir_sub <- na.omit(hp_eir_sub, hp_eir_sub[,"value"])

#load eircode shape
eir.sf <- st_read("Data/Eircode/Routing_key.shp")
eir.sf <- st_make_valid(eir.sf)
eir.sf <- eir.sf[!st_is_empty(eir.sf), ]

eir.sf <- merge(eir.sf,hp_eir_sub,by.x="RoutingKey",by.y="eircode")
eir.sf <- st_transform(eir.sf,2157)
eir.sf <- na.omit(eir.sf, eir.sf[,"value"])

#all counties includes northern ireland
all_cty <- st_read("Data/all_counties/all_counties_ITM95.shp")
all_cty <- st_transform(all_cty,2157)

#create map of house prices at eircode
sf.object = eir.sf
num_break=5
type_break="quantile"
var_name="rat_value"
var_text="Ratio of median price to average"
#file name to save as
file_map <- "Output/ratio_median_nat_avg.png"
#text used for map title
ov_title_map <- "Ratio of Median New House Price to Average"
#source and credits
credits1 <- "Source data: CSO HPM08
Created: Paul Kilgarriff"
legend_title <- "Ratio of median price to average"

#
#map.var.cloro(eds,"pct_chg","Population Change 2016-2022")

map.var.cloro <- function(sf.object,  #sf object
                          var_name=NA, #variable
                          var_text=NA,
                          num_break = 5,
                          type_break = "quantile"){
  #set file and title names
  #file name to save as
  file_map1 <- as.character(paste0("Output/map_",var_name,"_q",num_break,".png"))
  
  #source and credits
  credits1 <- "Source data: CSO HPM08\n Created: Paul Kilgarriff"
  
  #get limits used for positioning legend and scale bar
  bbox <- st_bbox(sf.object)
  x_min1 <- bbox[1]
  x_max1<- bbox[3]
  y_min1 <- bbox[2]
  y_max1<- bbox[4]
  x_width <- x_max1 - x_min1
  y_width <- y_max1 - y_min1
  
  # Define your break values
  break_values <- getJenksBreaks(sf.object[[var_name]],num_break, subset = NULL)
  
  # Apply the breaks to your data
  sf.object[["br_var"]] <- cut(sf.object[[var_name]],breaks = break_values, include.lowest = TRUE, right = FALSE)
  #get labels
  #create labels
  labs <- round(break_values,digits=1)
  labs_plot <- paste0("(", labs[1:(num_break)], "% -", labs[2:(num_break+1)], "%)")
  pal <- hcl.colors(num_break, "Inferno", rev = TRUE, alpha = 0.7)
  
  # Plot the map using ggplot
  p_house_price<-ggplot() +
    geom_sf(data = all_cty, fill = "grey", colour = NA, show.legend = FALSE) +
    geom_sf(data = sf.object,aes(fill = br_var), color = NA) + 
    scale_fill_manual(values = pal,label = labs_plot)+
    theme_bw()+
    theme(legend.position = c(0.2,0.9),
          legend.text = element_text(size = 10),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          plot.background = element_rect(fill = "white"),
          plot.margin = unit(c(0, 0, 0, 0), "cm"),
          panel.background = element_blank(),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          axis.line = element_line(colour = "white"),
          panel.border = element_rect(colour = "black", fill=NA, linewidth=5))+
    annotation_scale( pad_x = unit(10, "cm"),pad_y = unit(1, "cm"))+
    theme(legend.text.align = 0)+
    theme(legend.title=element_text(size=10))+
    theme(legend.text=element_text(size=10))+
    xlim(x_min1, x_max1)+
    ylim(y_min1, y_max1)+
    theme(panel.grid=element_line(color = "white")) +
    labs(x = '', y = '')+
    labs(title = ov_title_map, 
         subtitle = "Eircode Routing Key", 
         caption = credits1)+
    guides(fill=guide_legend(title=legend_title))+
    theme(plot.title = element_text(hjust = 0, vjust = -1))+
    theme(plot.subtitle = element_text(hjust = 0, vjust = -1))+
    theme(plot.caption = element_text(hjust = 0, vjust = 6))
  
  ggsave(file_map1, plot = p_house_price, width = 20, height = 12, dpi = 500)
  return(p_house_price)
}

plot_hp <- map.var.cloro(eir.sf,"rat_value","Ratio of Median Price to Average")
  
map.var.cloro(eir.sf,"rat_value","Ratio of Median Price to Average")
