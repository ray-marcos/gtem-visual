# GLOBIOM check

# Required packages
x = c("forecast", "data.table", "lubridate", "ggplot2", "ggthemes",  "gridExtra", "cowplot")
lapply(x, library, character.only = TRUE);
# ,

# Set the working directory
setwd("C:/Users/mar77v/CSIRO/ABARES CSIRO Modeling - Visualization/V July_27_2017")

# Load the FAOSTAT data

# lu = read.csv("FAOSTAT_data_5-3-2017 Yield Area.csv")
# # Extract the ANZ  data
# ANZ_Pn_FAO = lu[lu["Area"]== "Australia & New Zealand",]

# load the GLOBIOM data
# The script can also run SSP1 
# ssp_lu = read.csv("SSP1_LU_Template.csv", stringsAsFactors = F)
ssp_lu = read.csv("SSP2_LU_Template.csv", stringsAsFactors = F)
regs = unique(ssp_lu$Region)
lu = unique(ssp_lu$Land.type)

nat_first = read.csv("C:/Users/mar77v/Dropbox/Australia/GLOBIOM/latest_results/FourDegrees/IIASA_GLOBIOM_emulator_interpolated_output 1990-2100.csv")
nat_first = nat_first[nat_first$Regions == "AustraliaReg",]
nat_first[,5:14] = NULL
worktog = read.csv("C:/Users/mar77v/Dropbox/Australia/GLOBIOM/latest_results/TwoDegrees/IIASA_GLOBIOM_emulator_interpolated_output 1990-2100.csv")
worktog = worktog[worktog$Regions == "AustraliaReg",]
worktog[,5:14] = NULL
#Subset for ANO2 technical report
regs = c("World", "AustraliaReg" )

## REGION

id_reg = "AustraliaReg"

for(id_reg in regs){
  # # Select data for the selected region
  tmp_reg =  worktog
  # # Delete text columns
  tmp_regNF = nat_first
  
  
  # LAND COVER
  
  # list of land cover items
  # items = c( "Land Cover", 
  #            "Land Cover|Cropland",
  #            "Land Cover|Cropland|Cereals", 
  #            "Land Cover|Cropland|Energy Crops", 
  #            "Land Cover|Forest",                                      
  #            "Land Cover|Forest|Forestry", 
  #            "Land Cover|Forest|Managed",
  #            "Land Cover|Forest|Natural Forest",
  #            "Land Cover|Forest|Afforestation and Reforestation",
  #            "Land Cover|Other Natural Land",
  #            "Land Cover|Pasture",
  #            "Land Cover|Cropland|Irrigated" )
  
  #Selected land uses: Cropland, Forest, Pasture, Other Natural Land
  id_item = c("Land Cover|Cropland", "Land Cover|Pasture", 
              "Land Cover|Forest", "Land Cover|Other Natural Land")
  
  # Create a dataframe with the selected categories
  df_lc = as.data.frame(seq(2000,2060,10))
  for(i in id_item){
    df_lc[i] = as.numeric(tmp_reg[tmp_reg$Group.1 == i,2:8])
  }
  names(df_lc) = c("year", "Cropland", "Pasture", "Forest", "ONL")
  
  df_lcNF = as.data.frame(seq(2000,2060,10))
  for(i in id_item){
    df_lcNF[i] = as.numeric(tmp_reg_mean[tmp_reg_mean$Group.1 == i,2:8])
  }
  names(df_lcNF) = c("year", "Cropland", "Pasture", "Forest", "ONL")
  
  
  land_cover = ggplot(data = df_lc, aes(year)) +
    geom_line(aes(y =  Cropland, colour = "Cropland"), size = 0.8) +
    geom_line(aes(y =  Forest, colour = "Forest"), size = 0.8) +
    geom_line(aes(y =  Pasture, colour = "Pasture"), size = 0.8) +
    geom_line(aes(y =  ONL, colour = "ONL"), size = 0.8) +
    theme_few() +  
    ggtitle("Land Cover") + xlab("Year") + ylab("million ha") +
    theme(legend.position="bottom",
          legend.justification="center",
          legend.margin=margin(0,-9.7,0,0)) +
    scale_color_manual(labels = c("Cropland", "Forest", "Other natural land", "Pasture"), 
                       values = c("#DF1895", "#38A800", "#1E22AA","#F48324"),
                       name = " ") +
    scale_x_continuous(breaks=seq(2000,2060,10)) +
    expand_limits(x = 2000, y = 0)
  
  # geom_line(aes(y =  OtherNaturalLand, colour = "Other Natural Land"), size = 0.8) +
  # FOOD DEMAND
  # 
  #  "Food Demand"                                            
  #  "Food Demand|Crops"                                      
  #  "Food Demand|Livestock"                                  
  #  "Food Energy Demand"                                     
  #  "Food Energy Demand|Livestock" 
  
  #Selected items
  id_item = c("Food Demand|Crops", "Food Demand|Livestock")
  
  # Create a dataframe with the selected categories
  df_fd = as.data.frame(seq(2000,2060,10))
  for(i in id_item){
    df_fd[i] = as.numeric(tmp_reg_mean[tmp_reg_mean$Group.1 == i,2:8])
  }
  
  names(df_fd) = c("year", "crops", "livestock")
  
  food_demand = ggplot(data = df_fd, aes(year)) +
    geom_line(aes(y =  crops, colour = "crops"), size = 0.8) +
    geom_line(aes(y =  livestock, colour = "livestock"), size = 0.8) +
    theme_few()  +   
    ggtitle("Food demand") + xlab("Year") + ylab("kcal/cap/day") +
    theme(legend.position="bottom",
          legend.justification="center",
          legend.margin=margin(0,-9.7,0,0)) +
    scale_color_manual(labels = c("crops", "livestock"), 
                       values = c("#DF1895", "#F48324"),
                       name = " ") +
    scale_x_continuous(breaks=seq(2000,2060,10)) +
    expand_limits(x = 2000, y = 0)
  
  # AGRICULTURAL PRODUCTION
  
  # [52] "Agricultural Production"                                
  # [53] "Agricultural Production|Livestock"                      
  # [54] "Agricultural Production|Non-Energy Crops"               
  # [55] "Agricultural Production|Non-Energy Crops|Cereals"       
  # [56] "Agricultural Production|Energy Crops"  
  
  #Selected items
  id_item = c("Agricultural Production|Livestock", "Agricultural Production|Non-Energy Crops", 
              "Agricultural Production|Energy Crops")
  
  # Create dataframe with the selected categories
  df_ag_pn = as.data.frame(seq(2000,2060,10))
  for(i in id_item){
    df_ag_pn[i] = as.numeric(tmp_reg_mean[tmp_reg_mean$Group.1 == i,2:8])
  }
  
  names(df_ag_pn) = c("year", "livestock", "Non_Energy_Crops", "Energy_Crops")
  df_ag_pn$Energy_Crops[2] = df_ag_pn$Energy_Crops[1] # Replace NaN
  
  ag_pn = ggplot(data = df_ag_pn, aes(year)) +
    geom_line(aes(y =  livestock, colour = "livestock"), size = 0.8) +
    geom_line(aes(y =  Energy_Crops, colour = "Energy_Crops"), size = 0.8) +
    geom_line(aes(y =  Non_Energy_Crops, colour = "Non_Energy_Crops"), size = 0.8) +
    theme_few()  +  
    ggtitle("Agricultural production") + xlab("Year") + ylab("million t DM/ha/yr") +
    theme(legend.position="bottom",
          legend.justification="center",
          legend.margin=margin(0,-9.7,0,0)) +
    scale_color_manual(labels = c("Energy crops", "Livestock", "Non energy crops" ), 
                       values = c("#3DD13B", "#F48324", "#6C1F77"),
                       name = " ") +
    scale_x_continuous(breaks=seq(2000,2060,10)) +
    expand_limits(x = 2000, y = 0)
  
  
  # AGRICULTURAL DEMAND
  
  # list of items
  #  "Agricultural Demand"                                    
  #  "Agricultural Demand|Food"                               
  #  "Agricultural Demand|Food|Crops"                         
  #  "Agricultural Demand|Food|Livestock"                     
  #  "Agricultural Demand|Non-Food"                           
  #  "Agricultural Demand|Non-Food|Crops"                     
  #  "Agricultural Demand|Non-Food|Livestock"                 
  #  "Agricultural Demand|Feed"                               
  #  "Agricultural Demand|Feed|Crops"                         
  #  "Agricultural Demand|Bioenergy"                          
  #  "Agricultural Demand|Bioenergy|1st generation"           
  #  "Agricultural Demand|Bioenergy|2nd generation"
  
  #Selected items
  id_item = c("Agricultural Demand|Food", "Agricultural Demand|Non-Food", 
              "Agricultural Demand|Feed", "Agricultural Demand|Bioenergy")
  
  # Create a dataframe with the selected categories
  df_agd = as.data.frame(seq(2000,2060,10))
  for(i in id_item){
    df_agd[i] = as.numeric(tmp_reg_mean[tmp_reg_mean$Group.1 == i,2:8])
  }
  names(df_agd) = c("year", "food", "non_food", "feed", "bioenergy")
  
  ag_demand = ggplot(data = df_agd, aes(year)) +
    geom_line(aes(y =  food,     colour = "food"), size = 0.8) +
    geom_line(aes(y =  non_food, colour = "non-food"), size = 0.8) +
    geom_line(aes(y =  feed,     colour = "feed"), size = 0.8) +
    geom_line(aes(y =  bioenergy, colour = "bioenergy"), size = 0.8) +
    theme_few() +  
    ggtitle("Agricultural demand") + xlab("Year") + ylab("million t DM/yr") +
    theme(legend.position="bottom",
          legend.justification="center",
          legend.margin=margin(0,-9.7,0,0)) +
    
    scale_color_manual(labels = c( "bioenergy", "feed", "food", "non-food"), 
                       values = c("#3DD13B", "#A87000", "#0085A8", "#666666"),
                       name = " ") +
    scale_x_continuous(breaks=seq(2000,2060,10)) +
    expand_limits(x = 2000, y = 0)
  
  
  # YIELD
  
  # [29] "Yield|cereal"                                           
  # [30] "Yield|Oilcrops"                                         
  # [31] "Yield|Sugarcrops" 
  
  #Selected items
  id_item = c("Yield|cereal", "Yield|Oilcrops", "Yield|Sugarcrops")
  
  # Create dataframe with the selected categories
  df_y = as.data.frame(seq(2000,2060,10))
  for(i in id_item){
    df_y[i] = as.numeric(tmp_reg_mean[tmp_reg_mean$Group.1 == i,2:8])
  }
  
  names(df_y) = c("year", "cereal", "Oilcrops", "Sugarcrops")
  
  yield = ggplot(data = df_y, aes(year)) +
    geom_line(aes(y =  cereal, colour = "cereal"), size = 0.8) +
    geom_line(aes(y =  Oilcrops, colour = "Oilcrops"), size = 0.8) +
    geom_line(aes(y =  Sugarcrops, colour = "Sugarcrops"), size = 0.8) +
    theme_few() +   
    ggtitle("Yields") + xlab("Year") + ylab("t DM/ha/yr") +
    theme(legend.position="bottom",
          legend.justification="center",
          legend.margin=margin(0,-9.7,0,0)) +
    scale_color_manual(labels = c("cereals", "oilcrops", "sugarcrops"), 
                       values = c("#898845", "#0085A8", "#2DCBD3"),
                       name = " ") +
    scale_x_continuous(breaks=seq(2000,2060,10)) +
    expand_limits(x = 2000, y = 0)
  
  
  # FORESTRY PRODUCTION AND DEMAND
  
  # [57] "Forestry Production|Forest Residues"                    
  # [58] "Forestry Demand|Roundwood"                              
  # [59] "Forestry Demand|Roundwood|Wood Fuel"                    
  # [60] "Forestry Demand|Roundwood|Industrial Roundwood"         
  # [61] "Forestry Production|Roundwood"                          
  # [62] "Forestry Production|Roundwood|Wood Fuel"                
  # [63] "Forestry Production|Roundwood|Industrial Roundwood" 
  
  #Selected items
  id_item = c("Forestry Production|Forest Residues", "Forestry Production|Roundwood" , 
              "Forestry Demand|Roundwood" )
  
  # Create dataframe with the selected categories
  df_for_pn = as.data.frame(seq(2000,2060,10))
  for(i in id_item){
    df_for_pn[i] = as.numeric(tmp_reg_mean[tmp_reg_mean$Group.1 == i,2:8])
  }
  
  names(df_for_pn) = c("year", "Forest_Residues", "Roundwood_pn", "Roundwood_demand")
  
  for_pn_dem = ggplot(data = df_for_pn, aes(year)) +
    geom_line(aes(y =  Forest_Residues, colour = "Forest_Residues"), size = 0.8) +
    geom_line(aes(y =  Roundwood_pn, colour = "Roundwood_pn"), size = 0.8) +
    geom_line(aes(y =  Roundwood_demand, colour = "Roundwood_demand"), linetype = "dashed", size = 0.8) +
    theme_few() +  
    ggtitle("Forestry production and demand") + xlab("Year") + ylab("million t DM/ha/yr") +
    theme(legend.position="bottom",
          legend.justification="center",
          legend.margin=margin(0,-9.7,0,0)) +
    scale_color_manual(labels = c("Forest Residues", "Roundwood demand", "Roundwood Pn"), 
                       values = c("#A87000", "#267300", "#A60733"),
                       name = " ") +
    scale_x_continuous(breaks=seq(2000,2060,10)) +
    expand_limits(x = 2000, y = 0)
  
  # FERTILIZER USE
  
  # "Fertilizer Use|Nitrogen"                                
  # "Fertilizer Use|Phosphorus"
  
  #Selected items
  id_item = c("Fertilizer Use|Nitrogen", "Fertilizer Use|Phosphorus")
  
  # Create a dataframe with the selected categories
  df_fu = as.data.frame(seq(2000,2060,10))
  for(i in id_item){
    df_fu[i] = as.numeric(tmp_reg_mean[tmp_reg_mean$Group.1 == i,2:8])
  }
  
  names(df_fu) = c("year", "Nitrogen", "Phosphorus")
  
  fert_use = ggplot(data = df_fu, aes(year)) +
    geom_line(aes(y =  Nitrogen, colour = "Nitrogen"), size = 0.8) +
    geom_line(aes(y =  Phosphorus, colour = "Phosphorus"), size = 0.8) +
    theme_few()  +   
    ggtitle("Fertilizer use") + xlab("Year") + ylab("Tg/yr") +
    theme(legend.position="bottom",
          legend.justification="center",
          legend.margin=margin(0,-9.7,0,0)) +
    scale_color_manual(labels = c("Nitrogen", "Phosphorus"), 
                       values = c("#3384CD", "#67000D"),
                       name = " ") +
    scale_x_continuous(breaks=seq(2000,2060,10)) +
    expand_limits(x = 2000, y = 0)
  
  
  # EMISSIONS
  
  # list of emission items
  # "Emissions|CH4|Land Use"                                 
  # "Emissions|CH4|Land Use|Agriculture"                     
  # "Emissions|CH4|Land Use|Agriculture|Rice"                
  # "Emissions|CH4|Land Use|Agriculture|AWM"                 
  # "Emissions|CH4|Land Use|Agriculture|Enteric Fermentation"
  # "Emissions|CO2|Land Use"                                 
  # "Emissions|CO2|Land Use|Positive"                        
  # "Emissions|CO2|Land Use|Negative"                        
  # "Emissions|N2O|Land Use"                                 
  # "Emissions|N2O|Land Use|Agriculture"                     
  # "Emissions|N2O|Land Use|Agriculture|AWM"                 
  # "Emissions|N2O|Land Use|Agriculture|Cropland Soils"      
  # "Emissions|N2O|Land Use|Agriculture|Pasture"
  
  #Selected land uses: Cropland, Forest, Pasture, Other Natural Land
  id_item = c("Emissions|CH4|Land Use", "Emissions|CO2|Land Use", "Emissions|N2O|Land Use")
  
  # Create a dataframe with the selected categories
  df_e = as.data.frame(seq(2000,2060,10))
  for(i in id_item){
    df_e[i] = as.numeric(tmp_reg_mean[tmp_reg_mean$Group.1 == i,2:8])
  }
  names(df_e) = c("year", "CH4", "CO2", "N2O")
  # Transform units to Mt of CO2 equivalent
  df_e["CH4"] = df_e["CH4"] * 25
  df_e["N2O"] = df_e["N2O"]/1000 * 298
  
  emissions = ggplot(data = df_e, aes(year)) +
    geom_line(aes(y =  CH4, colour = "CH4"), size = 0.8) +
    geom_line(aes(y =  CO2, colour = "CO2"), size = 0.8) +
    geom_line(aes(y =  N2O, colour = "N2O"), size = 0.8) +
    theme_few() +  
    ggtitle("Emissions") + xlab("Year") + ylab("Mt CO2e/yr") +
    theme(legend.position="bottom",
          legend.justification="center",
          legend.margin=margin(0,-9.7,0,0)) +
    scale_color_manual(labels = c("CH4", "CO2", "N2O"), 
                       values = c("#3384CD", "#6666CC", "#F48324"),
                       name = " ") +
    scale_x_continuous(breaks=seq(2000,2060,10))
  
  
  # PRICES 
  # list of items
  #  "Price|Primary Energy|Biomass"                           
  #  "Price|Agriculture|Non-Energy Crops and Livestock|Index" 
  #  "Price|Agriculture|Non-Energy Crops|Index"
  #  "Price|Carbon" 
  
  #Selected items
  id_item = c("Price|Primary Energy|Biomass", "Price|Agriculture|Non-Energy Crops and Livestock|Index", 
              "Price|Agriculture|Non-Energy Crops|Index", "Price|Carbon")
  
  # Create dataframe with the selected categories
  df_pric = as.data.frame(seq(2000,2060,10))
  for(i in id_item){
    df_pric[i] = as.numeric(tmp_reg_mean[tmp_reg_mean$Group.1 == i,2:8])
  }
  names(df_pric) = c("year", "biomass", "non_energy_crops_livestock_idx", "non_energy_crops_idx", "carbon")
  
  pric_bio_co2 = ggplot(data = df_pric, aes(year)) +
    geom_line(aes(y =  biomass,     colour = "biomass"), size = 0.8) +
    geom_line(aes(y =  carbon, colour = "carbon"), size = 0.8) +
    theme_few() +  
    ggtitle("Prices: Carbon and biomass") + xlab("Year") + ylab("US$2005 per GJ or t CO2") +
    theme(legend.position="bottom",
          legend.justification="center",
          legend.margin=margin(0,-9.7,0,0)) +
    scale_color_manual(labels = c("biomass", "carbon"), 
                       values = c("#F48324", "#6666CC"),
                       name = " ") +
    scale_x_continuous(breaks=seq(2000,2060,10)) +
    expand_limits(x = 2000, y = 0)
  
  pric_ag_liv = ggplot(data = df_pric, aes(year)) +
    geom_line(aes(y =  non_energy_crops_livestock_idx, colour = "non_energy_crops_livestock_idx"), size = 0.8) +
    geom_line(aes(y =  non_energy_crops_idx,     colour = "non_energy_crops_idx"), size = 0.8) +
    theme_few() + 
    ggtitle("Prices: livestock and crops") + xlab("Year") + ylab("Index (2005 = 1)") +
    theme(legend.position="bottom",
          legend.justification="center",
          legend.margin=margin(0,-9.7,0,0)) +
    scale_color_manual(labels = c("non-energy crops index", "non-energy crops & livestock index"), 
                       values = c( "#6C1F77", "#0085A8"),
                       name = " ") +
    scale_x_continuous(breaks=seq(2000,2060,10)) +
    expand_limits(x = 2000, y = 0) +
    expand_limits(x = 2000, y = 2)
  
  # GRID Using Cowplot
  prow2 <- plot_grid(land_cover, food_demand, ag_pn, ag_demand, 
                     yield, for_pn_dem, fert_use, emissions,
                     pric_ag_liv,
                     align = 'vh',
                     labels = "auto",
                     hjust = -1,
                     nrow = 5,
                     scale = 1) # increase or reduce the size of the map
  
  ggsave(paste(id_reg, "averages June2018 SSP2.tiff"), plot = prow2, dpi = 500, width = 250, height = 350, units = "mm",
         compression="lzw", type="cairo") 
  
  
}



# Summarize table
# library("xlsx", lib.loc="~/R/win-library/3.4")
# output = read.xlsx("output4csiro_SSP2_03072017_v2.xlsx", sheetName = "OUTPUT", stringsAsFactors=FALSE)

library("openxlsx", lib.loc="~/R/win-library/3.4")
output = read.xlsx("output4csiro_SSP2_03072017_v2.xlsx", sheet = 1, colNames = T)
output_W = output[output$X3 == "World",]



# unique(output$X3)

# Plot data
# dat <- matrix(runif(40,1,20),ncol=4) # make data
# matplot(dat, type = c("b"),pch=1,col = 1:4) #plot
# legend("topleft", legend = 1:4, col=1:4, pch=1) # optional legend

nat_first = read.csv("C:/Users/mar77v/Dropbox/Australia/GLOBIOM/latest_results/FourDegrees/IIASA_GLOBIOM_emulator_interpolated_output 1990-2100.csv")
nat_first = nat_first[nat_first$Regions == "AustraliaReg",]

worktog = read.csv("C:/Users/mar77v/Dropbox/Australia/GLOBIOM/latest_results/TwoDegrees/IIASA_GLOBIOM_emulator_interpolated_output 1990-2100.csv")
worktog = worktog[worktog$Regions == "AustraliaReg",]

# individual graphs

#Forest

forests = read.csv("forests_SSP2.csv")
forests[,1] = NULL
# forests[is.na(forests)] = 0

fmat = t(as.matrix(forests))

matplot(fmat[1:71, 2:97], type = "l", ylab = "Million hectares", xlab = "Year", 
        col = "#72CC98", axes = F, ylim=c(0,180)) #plot

axis(1, at = seq(0,71,10), labels = seq(1990,2060,10), cex.axis = 0.7)
axis(2, at = seq(0,180,20), labels = seq(0,180,20), cex.axis = 0.7)

lines(fmat[1:71, 1], col = "#2DCBD3", lty = 3, lwd = 2 )
#WT
wt_forests = as.numeric(worktog[worktog$ParameterName == "Land Cover|Forest",5:75])
lines(wt_forests, col = "#6666FF", lty = 1, lwd = 2 )
# NF
nf_forests = as.numeric(nat_first[nat_first$ParameterName == "Land Cover|Forest",5:75])
lines(nf_forests, col = "#C00000", lty = 1, lwd = 2 )


legend("bottomright", 
       legend = c("Historical", "GLOBIOM's outlooks", "Working Together", "Nations First"), 
       col= c("#2DCBD3","#72CC98", "#6666FF", "#C00000"), 
       lty = c(2,1,1,1), bty = "n") # optional legend
title(main = "a) Forests")



# Cropland
crops = read.csv("cropland_SSP2.csv")
crops[,1] = NULL
# grass[is.na(grass)] = 0

cmat = t(as.matrix(crops))

matplot(cmat[31:101, 2:97], type = "l", ylab = "Million hectares", xlab = "Year", 
        col = "#72CC98", axes = F, ylim=c(0,80)) #plot

axis(1, at = seq(0,71,10), labels = seq(1990,2060,10), cex.axis = 0.7)
axis(2, at = seq(0,80,20), labels = seq(0,80,20), cex.axis = 0.7)

lines(cmat[31:101, 1], col = "#2DCBD3", lty = 3, lwd = 2 )
legend("bottomright", legend = c("Historical", "GLOBIOM"), col= c("#2DCBD3","#72CC98" ), 
       lty = c(2,1), bty = "n") # optional legend
title(main = "b) Cropland")


# Grasslands

grass = read.csv("grasslands_SSP2.csv")
grass[,1] = NULL
# grass[is.na(grass)] = 0

gmat = t(as.matrix(grass))

matplot(gmat[31:101, 2:97], type = "l", ylab = "Million hectares", xlab = "Year", 
        col = "#72CC98", axes = F, ylim=c(0,450)) #plot

axis(1, at = seq(0,71,10), labels = seq(1990,2060,10), cex.axis = 0.7)
axis(2, at = seq(0,450,50), labels = seq(0,450,50), cex.axis = 0.7)

lines(gmat[31:101, 1], col = "#2DCBD3", lty = 3, lwd = 2 )
legend("bottomright", legend = c("Historical", "GLOBIOM"), col= c("#2DCBD3","#72CC98" ), 
       lty = c(2,1), bty = "n") # optional legend
title(main = "c) Grassland")





#--------------------------------------------------------------------------------------------
# Combined graphs
tiff("Land use Updated.tiff", compression = "lzw", res = 500, width = 190, height = 69, units = "mm")

par(mfrow=c(1,3), cex = 0.7)
# par(mfrow=c(1,3), mar=c(4,4,1,0), cex = 0.8)

forests = read.csv("forests_SSP2.csv")
forests[,1] = NULL
# forests[is.na(forests)] = 0


fmat = t(as.matrix(forests))
par(mar = c(4,4,1,0))
matplot(fmat[1:71, 2:97], type = "l", ylab = " ",
        col = "#72CC98", axes = F, ylim=c(0,450)) #plot
axis(1, at = seq(0,71,10), labels = seq(1990,2060,10), cex.axis = 0.7)
axis(2, at = seq(0,450,50), labels = seq(0,450,50), cex.axis = 0.7)

lines(fmat[1:71, 1], col = "#2DCBD3", lty = 3, lwd = 2 )

#WT
wt_forests = as.numeric(worktog[worktog$ParameterName == "Land Cover|Forest",5:75])
lines(wt_forests, col = "#6666FF", lty = 1, lwd = 2 )
# NF
nf_forests = as.numeric(nat_first[nat_first$ParameterName == "Land Cover|Forest",5:75])
lines(nf_forests, col = "#C00000", lty = 6, lwd = 2 )

# legend("bottomright", legend = c("Historical", "GLOBIOM"), col= c("#2DCBD3","#72CC98" ), 
#        lty = c(2,1), bty = "n") # optional legend
title(main = "a) Forests", ylab = "Million hectares" )

# Cropland
crops = read.csv("cropland_SSP2.csv")
crops[,1] = NULL
# grass[is.na(grass)] = 0

cmat = t(as.matrix(crops))

par(mar = c(4,1,1,0))
matplot(cmat[31:101, 2:97], type = "l",  xlab = "Year",  ylab = " ",
        col = "#72CC98", axes = F, ylim=c(0,450)) #plot

axis(1, at = seq(0,71,10), labels = seq(1990,2060,10), cex.axis = 0.7)

lines(cmat[31:101, 1], col = "#2DCBD3", lty = 3, lwd = 2 )

#WT
wt_crops = as.numeric(worktog[worktog$ParameterName == "Land Cover|Cropland",5:75])
lines(wt_crops, col = "#6666FF", lty = 1, lwd = 2 )
# NF
nf_crops = as.numeric(nat_first[nat_first$ParameterName == "Land Cover|Cropland",5:75])
lines(nf_crops, col = "#C00000", lty = 6, lwd = 2 )
# legend("bottomright", legend = c("Historical", "GLOBIOM"), col= c("#2DCBD3","#72CC98" ),
#        lty = c(2,1), bty = "n") # optional legend
title(main = "b) Cropland")

# Grasslands

grass = read.csv("grasslands_SSP2.csv")
grass[,1] = NULL
grass[2:98,41:141] = grass[2:98,41:141] + 182.9
# grass[is.na(grass)] = 0

gmat = t(as.matrix(grass))


par(mar = c(4,1,1,0))
matplot(gmat[31:101, 2:97], type = "l", ylab = " ",
        col = "#72CC98", axes = F, ylim=c(0,450)) #plot

axis(1, at = seq(0,71,10), labels = seq(1990,2060,10), cex.axis = 0.7)

lines(gmat[31:101, 1], col = "#2DCBD3", lty = 3, lwd = 2 )
#WT
wt_grass = as.numeric(worktog[worktog$ParameterName == "Land Cover|Pasture",5:75])
lines(wt_grass, col = "#6666FF", lty = 1, lwd = 2 )
# NF
nf_grass = as.numeric(nat_first[nat_first$ParameterName == "Land Cover|Pasture",5:75])
lines(nf_grass, col = "#C00000", lty = 6, lwd = 2 )

legend("bottomright", legend = c("Historical", "GLOBIOM's outlooks", "Working Together", "Nations First"), 
       col= c("#2DCBD3","#72CC98", "#6666FF", "#C00000"), 
       lty = c(2,1,1,6), bty = "n", cex = 0.75) # optional legend
title(main = "c) Grassland")


dev.off()



#----------------------------------------------------------------------------------------------------
# Meat and crop projections
#----------------------------------------------------------------------------------------------------

prices = read.csv("pricesSSP2.csv", stringsAsFactors = F)
pn = read.csv("productionSSP2.csv", stringsAsFactors = F)
area = read.csv("surfaceSSP2.csv", stringsAsFactors = F)

histpric = read.csv("historical prices.csv", stringsAsFactors = F)
rownames(histpric) = unlist(histpric$crop); histpric$crop = NULL
histpn = read.csv("historical_production.csv", stringsAsFactors = F)
rownames(histpn) = unlist(histpn$crop); histpn$crop = NULL
histarea = read.csv("historical_surface.csv", stringsAsFactors = F)
rownames(histarea) = unlist(histarea$crop); histarea$crop = NULL

# Meat production

tiff("meat updated.tiff", compression = "lzw", res = 500, width = 140, height = 100, units = "mm")

par(mfrow=c(2,2), cex = 0.6)

# Beef production

fmat = t(as.matrix(pn[pn$lu == "BVMEAT",]))
par(mar = c(4,4,1,0))
matplot(fmat[4:74, 1:96], type = "l", ylab = " ",
        col = "#72CC98", axes = F, ylim=c(0,3000)) #plot
axis(1, at = seq(0,71,10), labels = seq(1990,2060,10), cex.axis = 0.7)
axis(2, at = seq(0,3000,500), labels = seq(0,3000,500), cex.axis = 0.7)

lines(as.numeric(histpn["BVMEAT",]), col = "#2DCBD3", lty = 3, lwd = 2 )

#WT
wt_bmeat = worktog[worktog$ParameterName == "BVMEAT",]
wt_bmeat_pn = as.numeric(wt_bmeat[wt_bmeat$ParameterType == "PROD", 5:75])
lines(wt_bmeat_pn, col = "#6666FF", lty = 1, lwd = 2 )
# NF
nf_bmeat = nat_first[nat_first$ParameterName == "BVMEAT",]
nf_bmeat_pn = as.numeric(nf_bmeat[nf_bmeat$ParameterType == "PROD", 5:75])

lines(nf_bmeat_pn, col = "#C00000", lty = 6, lwd = 2 )

# legend("bottomright", legend = c("Historical", "GLOBIOM"), col= c("#2DCBD3","#72CC98" ), 
#        lty = c(2,1), bty = "n") # optional legend
title(main = "a) Beef production", ylab = "1000 tons" )

# Sheep production

fmat = t(as.matrix(pn[pn$lu == "SGMEAT",]))
par(mar = c(4,4,1,0))
matplot(fmat[4:74, 1:96], type = "l", ylab = " ",
        col = "#72CC98", axes = F, ylim=c(0,800)) #plot
axis(1, at = seq(0,71,10), labels = seq(1990,2060,10), cex.axis = 0.7)
axis(2, at = seq(0,800,200), labels = seq(0,800,200), cex.axis = 0.7)

lines(as.numeric(histpn["SGMEAT",]), col = "#2DCBD3", lty = 3, lwd = 2 )

#WT
wt_sgmeat = worktog[worktog$ParameterName == "SGMEAT",]
wt_sgmeat_pn = as.numeric(wt_sgmeat[wt_sgmeat$ParameterType == "PROD", 5:75])
lines(wt_sgmeat_pn, col = "#6666FF", lty = 1, lwd = 2 )
# NF
nf_sgmeat = nat_first[nat_first$ParameterName == "SGMEAT",]
nf_sgmeat_pn = as.numeric(nf_sgmeat[nf_sgmeat$ParameterType == "PROD", 5:75])
lines(nf_sgmeat_pn, col = "#C00000", lty = 6, lwd = 2 )

# legend("bottomright", legend = c("Historical", "GLOBIOM"), col= c("#2DCBD3","#72CC98" ), 
#        lty = c(2,1), bty = "n") # optional legend
title(main = "b) Sheep & goat production", ylab = "" )

# Beef price

fmat = t(as.matrix(prices[prices$lu == "BVMEAT",]))
par(mar = c(4,4,1,0))
matplot(fmat[4:74, 1:96], type = "l", ylab = " ",
        col = "#72CC98", axes = F, ylim=c(0,20000)) #plot
axis(1, at = seq(0,71,10), labels = seq(1990,2060,10), cex.axis = 0.7)
axis(2, at = seq(0,20000,5000), labels = seq(0,20000,5000), cex.axis = 0.7)

lines(as.numeric(histpric["BVMEAT",]), col = "#2DCBD3", lty = 3, lwd = 2 )

wt_bmeat_pn = as.numeric(wt_bmeat[wt_bmeat$ParameterType == "XPRP", 5:75])
nf_bmeat_pn = as.numeric(nf_bmeat[nf_bmeat$ParameterType == "XPRP", 5:75])

lines(nf_bmeat_pn, col = "#C00000", lty = 6, lwd = 2 )
lines(wt_bmeat_pn, col = "#6666FF", lty = 1, lwd = 2 )
# legend("bottomright", legend = c("Historical", "GLOBIOM"), col= c("#2DCBD3","#72CC98" ), 
#        lty = c(2,1), bty = "n") # optional legend
title(main = "c) Beef prices", ylab = "USD 2000 per ton" )


# Sheep price

fmat = t(as.matrix(prices[prices$lu == "SGMEAT",]))
par(mar = c(4,4,1,0))
matplot(fmat[4:74, 1:96], type = "l", ylab = " ",
        col = "#72CC98", axes = F, ylim=c(0,35000)) #plot
axis(1, at = seq(0,71,10), labels = seq(1990,2060,10), cex.axis = 0.7)
axis(2, at = seq(0,35000,5000), labels = seq(0,35000,5000), cex.axis = 0.7)

lines(as.numeric(histpric["SGMEAT",]), col = "#2DCBD3", lty = 3, lwd = 2 )
#WT
wt_sgmeat_pn = as.numeric(wt_sgmeat[wt_sgmeat$ParameterType == "XPRP", 5:75])
lines(wt_sgmeat_pn, col = "#6666FF", lty = 1, lwd = 2 )
# NF
nf_sgmeat_pn = as.numeric(nf_sgmeat[nf_sgmeat$ParameterType == "XPRP", 5:75])
lines(nf_sgmeat_pn, col = "#C00000", lty = 6, lwd = 2 )


legend("topright", legend = c("Historical", "GLOBIOM's outlooks", "Working Together", "Nations First"), 
       col= c("#2DCBD3","#72CC98", "#6666FF", "#C00000"), 
       lty = c(2,1,1,6), bty = "n", cex = 0.75) # optional legend
title(main = "d) Sheep & goat prices", ylab = " " )

dev.off()




# CROPS


tiff("Crops updated.tiff", compression = "lzw", res = 500, width = 190, height = 240, units = "mm")

par(mfrow=c(6,3), cex = 0.6)


# Wheat area

fmat = t(as.matrix(area[area$lu == "Whea",]))
par(mar = c(4,4,1,0))
matplot(fmat[4:74, 1:96], type = "l", ylab = " ",
        col = "#72CC98", axes = F, ylim=c(0,20000)) #plot
axis(1, at = seq(0,71,10), labels = seq(1990,2060,10), cex.axis = 0.7)
axis(2, at = seq(0,20000,5000), labels = seq(0,20000,5000), cex.axis = 0.7)

lines(as.numeric(histarea["Wheat",]), col = "#2DCBD3", lty = 3, lwd = 2 )

#WT
wt_lu = worktog[worktog$ParameterName == "Whea",]
wt_lu_par = as.numeric(wt_lu[wt_lu$ParameterType == "Area", 5:75])
lines(wt_lu_par, col = "#6666FF", lty = 1, lwd = 2 )
# NF
nf_lu = nat_first[nat_first$ParameterName == "Whea",]
nf_lu_par = as.numeric(nf_lu[nf_lu$ParameterType == "Area", 5:75])
lines(nf_lu_par, col = "#C00000", lty = 6, lwd = 2 )

# legend("bottomright", legend = c("Historical", "GLOBIOM"), col= c("#2DCBD3","#72CC98" ), 
#        lty = c(2,1), bty = "n") # optional legend
title(main = "a) Wheat: cultivated area", ylab = "1000 ha" )

# Wheat Pn

fmat = t(as.matrix(pn[pn$lu == "Whea",]))
par(mar = c(4,4,1,0))
matplot(fmat[4:74, 1:96], type = "l", ylab = " ",
        col = "#72CC98", axes = F, ylim=c(0,50000)) #plot
axis(1, at = seq(0,71,10), labels = seq(1990,2060,10), cex.axis = 0.7)
axis(2, at = seq(0,50000,5000), labels = seq(0,50000,5000), cex.axis = 0.7)

lines(as.numeric(histpn["Wheat",]), col = "#2DCBD3", lty = 3, lwd = 2 )

#WT
wt_lu = worktog[worktog$ParameterName == "Whea",]
wt_lu_par = as.numeric(wt_lu[wt_lu$ParameterType == "PROD", 5:75])
lines(wt_lu_par, col = "#6666FF", lty = 1, lwd = 2 )
# NF
nf_lu = nat_first[nat_first$ParameterName == "Whea",]
nf_lu_par = as.numeric(nf_lu[nf_lu$ParameterType == "PROD", 5:75])
lines(nf_lu_par, col = "#C00000", lty = 6, lwd = 2 )
# legend("bottomright", legend = c("Historical", "GLOBIOM"), col= c("#2DCBD3","#72CC98" ), 
#        lty = c(2,1), bty = "n") # optional legend
title(main = "b) Wheat: production", ylab = "1000 tons" )

# Wheat price

fmat = t(as.matrix(prices[prices$lu == "Whea",]))
par(mar = c(4,4,1,0))
matplot(fmat[4:74, 1:96], type = "l", ylab = " ",
        col = "#72CC98", axes = F, ylim=c(0,600)) #plot
axis(1, at = seq(0,71,10), labels = seq(1990,2060,10), cex.axis = 0.7)
axis(2, at = seq(0,600,100), labels = seq(0,600,100), cex.axis = 0.7)

lines(as.numeric(histpric["Wheat",]), col = "#2DCBD3", lty = 3, lwd = 2 )
#WT
wt_lu = worktog[worktog$ParameterName == "Whea",]
wt_lu_par = as.numeric(wt_lu[wt_lu$ParameterType == "XPRP", 5:75])
lines(wt_lu_par, col = "#6666FF", lty = 1, lwd = 2 )
# NF
nf_lu = nat_first[nat_first$ParameterName == "Whea",]
nf_lu_par = as.numeric(nf_lu[nf_lu$ParameterType == "XPRP", 5:75])
lines(nf_lu_par, col = "#C00000", lty = 6, lwd = 2 )

# legend("bottomright", legend = c("Historical", "GLOBIOM"), col= c("#2DCBD3","#72CC98" ), 
#        lty = c(2,1), bty = "n") # optional legend
title(main = "c) Wheat: prices", ylab = "USD 2000 per ton" )



# Barley area

fmat = t(as.matrix(area[area$lu == "Barl",]))
par(mar = c(4,4,1,0))
matplot(fmat[4:74, 1:96], type = "l", ylab = " ",
        col = "#72CC98", axes = F, ylim=c(0,5000)) #plot
axis(1, at = seq(0,71,10), labels = seq(1990,2060,10), cex.axis = 0.7)
axis(2, at = seq(0,5000,1000), labels = seq(0,5000,1000), cex.axis = 0.7)

lines(as.numeric(histarea["barley",]), col = "#2DCBD3", lty = 3, lwd = 2 )

#WT
wt_lu = worktog[worktog$ParameterName == "Barl",]
wt_lu_par = as.numeric(wt_lu[wt_lu$ParameterType == "Area", 5:75])
lines(wt_lu_par, col = "#6666FF", lty = 1, lwd = 2 )
# NF
nf_lu = nat_first[nat_first$ParameterName == "Barl",]
nf_lu_par = as.numeric(nf_lu[nf_lu$ParameterType == "Area", 5:75])
lines(nf_lu_par, col = "#C00000", lty = 6, lwd = 2 )

# legend("bottomright", legend = c("Historical", "GLOBIOM"), col= c("#2DCBD3","#72CC98" ), 
#        lty = c(2,1), bty = "n") # optional legend
title(main = "d) Barley: cultivated area", ylab = "1000 ha" )

# Barley Pn

fmat = t(as.matrix(pn[pn$lu == "Barl",]))
par(mar = c(4,4,1,0))
matplot(fmat[4:74, 1:96], type = "l", ylab = " ",
        col = "#72CC98", axes = F, ylim=c(0,15000)) #plot
axis(1, at = seq(0,71,10), labels = seq(1990,2060,10), cex.axis = 0.7)
axis(2, at = seq(0,15000,5000), labels = seq(0,15000,5000), cex.axis = 0.7)

lines(as.numeric(histpn["barley",]), col = "#2DCBD3", lty = 3, lwd = 2 )

#WT
wt_lu = worktog[worktog$ParameterName == "Barl",]
wt_lu_par = as.numeric(wt_lu[wt_lu$ParameterType == "PROD", 5:75])
lines(wt_lu_par, col = "#6666FF", lty = 1, lwd = 2 )
# NF
nf_lu = nat_first[nat_first$ParameterName == "Barl",]
nf_lu_par = as.numeric(nf_lu[nf_lu$ParameterType == "PROD", 5:75])
lines(nf_lu_par, col = "#C00000", lty = 6, lwd = 2 )

# legend("bottomright", legend = c("Historical", "GLOBIOM"), col= c("#2DCBD3","#72CC98" ), 
#        lty = c(2,1), bty = "n") # optional legend
title(main = "e) Barley: production", ylab = "1000 tons" )

# Barley price
fmat = t(as.matrix(prices[prices$lu == "Barl",]))
par(mar = c(4,4,1,0))
matplot(fmat[4:74, 1:96], type = "l", ylab = " ",
        col = "#72CC98", axes = F, ylim=c(0,800)) #plot
axis(1, at = seq(0,71,10), labels = seq(1990,2060,10), cex.axis = 0.7)
axis(2, at = seq(0,800,200), labels = seq(0,800,200), cex.axis = 0.7)

lines(as.numeric(histpric["barley",]), col = "#2DCBD3", lty = 3, lwd = 2 )

#WT
wt_lu = worktog[worktog$ParameterName == "Barl",]
wt_lu_par = as.numeric(wt_lu[wt_lu$ParameterType == "XPRP", 5:75])
lines(wt_lu_par, col = "#6666FF", lty = 1, lwd = 2 )
# NF
nf_lu = nat_first[nat_first$ParameterName == "Barl",]
nf_lu_par = as.numeric(nf_lu[nf_lu$ParameterType == "XPRP", 5:75])
lines(nf_lu_par, col = "#C00000", lty = 6, lwd = 2 )
# legend("bottomright", legend = c("Historical", "GLOBIOM"), col= c("#2DCBD3","#72CC98" ), 
#        lty = c(2,1), bty = "n") # optional legend
title(main = "f) Barley: prices", ylab = "USD 2000 per ton" )



# Rapeseed area

fmat = t(as.matrix(area[area$lu == "Rape",]))
par(mar = c(4,4,1,0))
matplot(fmat[4:74, 1:96], type = "l", ylab = " ",
        col = "#72CC98", axes = F, ylim=c(0,5000)) #plot
axis(1, at = seq(0,71,10), labels = seq(1990,2060,10), cex.axis = 0.7)
axis(2, at = seq(0,5000,1000), labels = seq(0,5000,1000), cex.axis = 0.7)

lines(as.numeric(histarea["Rapeseeds",]), col = "#2DCBD3", lty = 3, lwd = 2 )
#WT
wt_lu = worktog[worktog$ParameterName == "Rape",]
wt_lu_par = as.numeric(wt_lu[wt_lu$ParameterType == "Area", 5:75])
lines(wt_lu_par, col = "#6666FF", lty = 1, lwd = 2 )
# NF
nf_lu = nat_first[nat_first$ParameterName == "Rape",]
nf_lu_par = as.numeric(nf_lu[nf_lu$ParameterType == "Area", 5:75])
lines(nf_lu_par, col = "#C00000", lty = 6, lwd = 2 )

# legend("bottomright", legend = c("Historical", "GLOBIOM"), col= c("#2DCBD3","#72CC98" ), 
#        lty = c(2,1), bty = "n") # optional legend
title(main = "g) Rapeseed: cultivated area", ylab = "1000 ha" )

# Rapeseed Pn

fmat = t(as.matrix(pn[pn$lu == "Rape",]))
par(mar = c(4,4,1,0))
matplot(fmat[4:74, 1:96], type = "l", ylab = " ",
        col = "#72CC98", axes = F, ylim=c(0,10000)) #plot
axis(1, at = seq(0,71,10), labels = seq(1990,2060,10), cex.axis = 0.7)
axis(2, at = seq(0,10000,2000), labels = seq(0,10000,2000), cex.axis = 0.7)

lines(as.numeric(histpn["Rapeseeds",]), col = "#2DCBD3", lty = 3, lwd = 2 )
#WT
wt_lu = worktog[worktog$ParameterName == "Rape",]
wt_lu_par = as.numeric(wt_lu[wt_lu$ParameterType == "PROD", 5:75])
lines(wt_lu_par, col = "#6666FF", lty = 1, lwd = 2 )
# NF
nf_lu = nat_first[nat_first$ParameterName == "Rape",]
nf_lu_par = as.numeric(nf_lu[nf_lu$ParameterType == "PROD", 5:75])
lines(nf_lu_par, col = "#C00000", lty = 6, lwd = 2 )
# legend("bottomright", legend = c("Historical", "GLOBIOM"), col= c("#2DCBD3","#72CC98" ), 
#        lty = c(2,1), bty = "n") # optional legend
title(main = "h) Rapeseed: production", ylab = "1000 tons" )

# Rapeseed price
fmat = t(as.matrix(prices[prices$lu == "Rape",]))
par(mar = c(4,4,1,0))
matplot(fmat[4:74, 1:96], type = "l", ylab = " ",
        col = "#72CC98", axes = F, ylim=c(0,1000)) #plot
axis(1, at = seq(0,71,10), labels = seq(1990,2060,10), cex.axis = 0.7)
axis(2, at = seq(0,1000,200), labels = seq(0,1000,200), cex.axis = 0.7)

lines(as.numeric(histpric["Rapeseeds",]), col = "#2DCBD3", lty = 3, lwd = 2 )
#WT
wt_lu = worktog[worktog$ParameterName == "Rape",]
wt_lu_par = as.numeric(wt_lu[wt_lu$ParameterType == "XPRP", 5:75])
lines(wt_lu_par, col = "#6666FF", lty = 1, lwd = 2 )
# NF
nf_lu = nat_first[nat_first$ParameterName == "Rape",]
nf_lu_par = as.numeric(nf_lu[nf_lu$ParameterType == "XPRP", 5:75])
lines(nf_lu_par, col = "#C00000", lty = 6, lwd = 2 )
# legend("bottomright", legend = c("Historical", "GLOBIOM"), col= c("#2DCBD3","#72CC98" ), 
#        lty = c(2,1), bty = "n") # optional legend
title(main = "i) Rapeseed: prices", ylab = "USD 2000 per ton" )



# Sorghum area

fmat = t(as.matrix(area[area$lu == "Srgh",]))
par(mar = c(4,4,1,0))
matplot(fmat[4:74, 1:96], type = "l", ylab = " ",
        col = "#72CC98", axes = F, ylim=c(0,2000)) #plot
axis(1, at = seq(0,71,10), labels = seq(1990,2060,10), cex.axis = 0.7)
axis(2, at = seq(0,2000,500), labels = seq(0,2000,500), cex.axis = 0.7)

lines(as.numeric(histarea["Sorghum",]), col = "#2DCBD3", lty = 3, lwd = 2 )
#WT
wt_lu = worktog[worktog$ParameterName == "Srgh",]
wt_lu_par = as.numeric(wt_lu[wt_lu$ParameterType == "Area", 5:75])
lines(wt_lu_par, col = "#6666FF", lty = 1, lwd = 2 )
# NF
nf_lu = nat_first[nat_first$ParameterName == "Srgh",]
nf_lu_par = as.numeric(nf_lu[nf_lu$ParameterType == "Area", 5:75])
lines(nf_lu_par, col = "#C00000", lty = 6, lwd = 2 )

# legend("bottomright", legend = c("Historical", "GLOBIOM"), col= c("#2DCBD3","#72CC98" ), 
#        lty = c(2,1), bty = "n") # optional legend
title(main = "j) Sorghum: cultivated area", ylab = "1000 ha" )

# Sorghum Pn

fmat = t(as.matrix(pn[pn$lu == "Srgh",]))
par(mar = c(4,4,1,0))
matplot(fmat[4:74, 1:96], type = "l", ylab = " ",
        col = "#72CC98", axes = F, ylim=c(0,4000)) #plot
axis(1, at = seq(0,71,10), labels = seq(1990,2060,10), cex.axis = 0.7)
axis(2, at = seq(0,4000,1000), labels = seq(0,4000,1000), cex.axis = 0.7)

lines(as.numeric(histpn["Sorghum",]), col = "#2DCBD3", lty = 3, lwd = 2 )
#WT
wt_lu = worktog[worktog$ParameterName == "Srgh",]
wt_lu_par = as.numeric(wt_lu[wt_lu$ParameterType == "PROD", 5:75])
lines(wt_lu_par, col = "#6666FF", lty = 1, lwd = 2 )
# NF
nf_lu = nat_first[nat_first$ParameterName == "Srgh",]
nf_lu_par = as.numeric(nf_lu[nf_lu$ParameterType == "PROD", 5:75])
lines(nf_lu_par, col = "#C00000", lty = 6, lwd = 2 )

# legend("bottomright", legend = c("Historical", "GLOBIOM"), col= c("#2DCBD3","#72CC98" ), 
#        lty = c(2,1), bty = "n") # optional legend
title(main = "k) Sorghum: production", ylab = "1000 tons" )

# Sorghum price
fmat = t(as.matrix(prices[prices$lu == "Srgh",]))
par(mar = c(4,4,1,0))
matplot(fmat[4:74, 1:96], type = "l", ylab = " ",
        col = "#72CC98", axes = F, ylim=c(0,600)) #plot
axis(1, at = seq(0,71,10), labels = seq(1990,2060,10), cex.axis = 0.7)
axis(2, at = seq(0,600,100), labels = seq(0,600,100), cex.axis = 0.7)

lines(as.numeric(histpric["Sorghum",]), col = "#2DCBD3", lty = 3, lwd = 2 )

#WT
wt_lu = worktog[worktog$ParameterName == "Srgh",]
wt_lu_par = as.numeric(wt_lu[wt_lu$ParameterType == "XPRP", 5:75])
lines(wt_lu_par, col = "#6666FF", lty = 1, lwd = 2 )
# NF
nf_lu = nat_first[nat_first$ParameterName == "Srgh",]
nf_lu_par = as.numeric(nf_lu[nf_lu$ParameterType == "XPRP", 5:75])
lines(nf_lu_par, col = "#C00000", lty = 6, lwd = 2 )

# legend("bottomright", legend = c("Historical", "GLOBIOM"), col= c("#2DCBD3","#72CC98" ), 
#        lty = c(2,1), bty = "n") # optional legend
title(main = "l) Sorghum: prices", ylab = "USD 2000 per ton" )



# Sugar cane area

fmat = t(as.matrix(area[area$lu == "SugC",]))
par(mar = c(4,4,1,0))
matplot(fmat[4:74, 1:96], type = "l", ylab = " ",
        col = "#72CC98", axes = F, ylim=c(0,1000)) #plot
axis(1, at = seq(0,71,10), labels = seq(1990,2060,10), cex.axis = 0.7)
axis(2, at = seq(0,1000,200), labels = seq(0,1000,200), cex.axis = 0.7)

lines(as.numeric(histarea["Sugar cane",]), col = "#2DCBD3", lty = 3, lwd = 2 )
#WT
wt_lu = worktog[worktog$ParameterName == "SugC",]
wt_lu_par = as.numeric(wt_lu[wt_lu$ParameterType == "Area", 5:75])
lines(wt_lu_par, col = "#6666FF", lty = 1, lwd = 2 )
# NF
nf_lu = nat_first[nat_first$ParameterName == "SugC",]
nf_lu_par = as.numeric(nf_lu[nf_lu$ParameterType == "Area", 5:75])
lines(nf_lu_par, col = "#C00000", lty = 6, lwd = 2 )

# legend("bottomright", legend = c("Historical", "GLOBIOM"), col= c("#2DCBD3","#72CC98" ), 
#        lty = c(2,1), bty = "n") # optional legend
title(main = "m) Sugar cane: cultivated area", ylab = "1000 ha" )

# Sugar cane Pn

fmat = t(as.matrix(pn[pn$lu == "SugC",]))
par(mar = c(4,4,1,0))
matplot(fmat[4:74, 1:96], type = "l", ylab = " ",
        col = "#72CC98", axes = F, ylim=c(0,100000)) #plot
axis(1, at = seq(0,71,10), labels = seq(1990,2060,10), cex.axis = 0.7)
axis(2, at = seq(0,100000,20000), labels = seq(0,100000,20000), cex.axis = 0.7)

lines(as.numeric(histpn["Sugar cane",]), col = "#2DCBD3", lty = 3, lwd = 2 )
#WT
wt_lu = worktog[worktog$ParameterName == "SugC",]
wt_lu_par = as.numeric(wt_lu[wt_lu$ParameterType == "PROD", 5:75])
lines(wt_lu_par, col = "#6666FF", lty = 1, lwd = 2 )
# NF
nf_lu = nat_first[nat_first$ParameterName == "SugC",]
nf_lu_par = as.numeric(nf_lu[nf_lu$ParameterType == "PROD", 5:75])
lines(nf_lu_par, col = "#C00000", lty = 6, lwd = 2 )
# legend("bottomright", legend = c("Historical", "GLOBIOM"), col= c("#2DCBD3","#72CC98" ), 
#        lty = c(2,1), bty = "n") # optional legend
title(main = "n) Sugar cane: production", ylab = "1000 tons" )

# Sugar cane price
fmat = t(as.matrix(prices[prices$lu == "SugC",]))
par(mar = c(4,4,1,0))
matplot(fmat[4:74, 1:96], type = "l", ylab = " ",
        col = "#72CC98", axes = F, ylim=c(0,60)) #plot
axis(1, at = seq(0,71,10), labels = seq(1990,2060,10), cex.axis = 0.7)
axis(2, at = seq(0,60,20), labels = seq(0,60,20), cex.axis = 0.7)

lines(as.numeric(histpric["Sugar cane",]), col = "#2DCBD3", lty = 3, lwd = 2 )
#WT
wt_lu = worktog[worktog$ParameterName == "SugC",]
wt_lu_par = as.numeric(wt_lu[wt_lu$ParameterType == "XPRP", 5:75])
lines(wt_lu_par, col = "#6666FF", lty = 1, lwd = 2 )
# NF
nf_lu = nat_first[nat_first$ParameterName == "SugC",]
nf_lu_par = as.numeric(nf_lu[nf_lu$ParameterType == "XPRP", 5:75])
lines(nf_lu_par, col = "#C00000", lty = 6, lwd = 2 )
# legend("bottomright", legend = c("Historical", "GLOBIOM"), col= c("#2DCBD3","#72CC98" ), 
#        lty = c(2,1), bty = "n") # optional legend
title(main = "o) Sugar cane: prices", ylab = "USD 2000 per ton" )


# Cotton area

fmat = t(as.matrix(area[area$lu == "Cott",]))
par(mar = c(4,4,1,0))
matplot(fmat[4:74, 1:96], type = "l", ylab = " ",
        col = "#72CC98", axes = F, ylim=c(0,2500)) #plot
axis(1, at = seq(0,71,10), labels = seq(1990,2060,10), cex.axis = 0.7)
axis(2, at = seq(0,2500,500), labels = seq(0,2500,500), cex.axis = 0.7)

lines(as.numeric(histarea["Cotton",]), col = "#2DCBD3", lty = 3, lwd = 2 )
#WT
wt_lu = worktog[worktog$ParameterName == "Cott",]
wt_lu_par = as.numeric(wt_lu[wt_lu$ParameterType == "Area", 5:75])
lines(wt_lu_par, col = "#6666FF", lty = 1, lwd = 2 )
# NF
nf_lu = nat_first[nat_first$ParameterName == "Cott",]
nf_lu_par = as.numeric(nf_lu[nf_lu$ParameterType == "Area", 5:75])
lines(nf_lu_par, col = "#C00000", lty = 6, lwd = 2 )
# legend("bottomright", legend = c("Historical", "GLOBIOM"), col= c("#2DCBD3","#72CC98" ), 
#        lty = c(2,1), bty = "n") # optional legend
title(main = "p) Cotton: cultivated area", ylab = "1000 ha" )

# Cotton Pn

fmat = t(as.matrix(pn[pn$lu == "Cott",]))
par(mar = c(4,4,1,0))
matplot(fmat[4:74, 1:96], type = "l", ylab = " ",
        col = "#72CC98", axes = F, ylim=c(0,12000)) #plot
axis(1, at = seq(0,71,10), labels = seq(1990,2060,10), cex.axis = 0.7)
axis(2, at = seq(0,12000,2000), labels = seq(0,12000,2000), cex.axis = 0.7)

lines(as.numeric(histpn["Cotton",]), col = "#2DCBD3", lty = 3, lwd = 2 )
#WT
wt_lu = worktog[worktog$ParameterName == "Cott",]
wt_lu_par = as.numeric(wt_lu[wt_lu$ParameterType == "PROD", 5:75])
lines(wt_lu_par, col = "#6666FF", lty = 1, lwd = 2 )
# NF
nf_lu = nat_first[nat_first$ParameterName == "Cott",]
nf_lu_par = as.numeric(nf_lu[nf_lu$ParameterType == "PROD", 5:75])
lines(nf_lu_par, col = "#C00000", lty = 6, lwd = 2 )

# legend("bottomright", legend = c("Historical", "GLOBIOM"), col= c("#2DCBD3","#72CC98" ), 
#        lty = c(2,1), bty = "n") # optional legend
title(main = "q) Cotton: production", ylab = "1000 tons" )

# Cotton price
fmat = t(as.matrix(prices[prices$lu == "Cott",]))
par(mar = c(4,4,1,0))
matplot(fmat[4:74, 1:96], type = "l", ylab = " ",
        col = "#72CC98", axes = F, ylim=c(0,4000)) #plot
axis(1, at = seq(0,71,10), labels = seq(1990,2060,10), cex.axis = 0.7)
axis(2, at = seq(0,4000,500), labels = seq(0,4000,500), cex.axis = 0.7)

lines(as.numeric(histpric["Cotton",]), col = "#2DCBD3", lty = 3, lwd = 2 )
#WT
wt_lu = worktog[worktog$ParameterName == "Cott",]
wt_lu_par = as.numeric(wt_lu[wt_lu$ParameterType == "XPRP", 5:75])
lines(wt_lu_par, col = "#6666FF", lty = 1, lwd = 2 )
# NF
nf_lu = nat_first[nat_first$ParameterName == "Cott",]
nf_lu_par = as.numeric(nf_lu[nf_lu$ParameterType == "XPRP", 5:75])
lines(nf_lu_par, col = "#C00000", lty = 6, lwd = 2 )
# legend("bottomright", legend = c("Historical", "GLOBIOM"), col= c("#2DCBD3","#72CC98" ), 
#        lty = c(2,1), bty = "n") # optional legend

title(main = "r) Cotton: prices", ylab = "USD 2000 per ton" )


legend("topright", legend = c("Historical", "GLOBIOM's outlooks", "Working Together", "Nations First"), 
       col= c("#2DCBD3","#72CC98", "#6666FF", "#C00000"), 
       lty = c(2,1,1,6), bty = "n", cex = 1) # optional legend



dev.off()





















# 
# # CREATE GRID 
# 
# # grid
# # with this command you can plot a grid of two or more plots.
# # To add more than two plots just write their names and modify the columns and rows numbers
# figs = grid.arrange(land_cover, emissions, ag_demand, pric_bio_co2, pric_ag_liv, yield, food_demand, 
#                     fert_use, ag_pn, for_pn_dem ,  ncol=2, nrow =5)
#  
# ggsave("grid.tiff", plot = figs, dpi = 500, width = 300, height = 260, units = "mm",
#        compression="lzw", type="cairo")
# 
# # Using Cowplot
# prow2 <- plot_grid(land_cover, emissions, ag_demand, pric_bio_co2, pric_ag_liv, yield, food_demand, 
#                     fert_use, ag_pn, for_pn_dem,
#                    align = 'vh',
#                    labels = "auto",
#                    hjust = -1,
#                    nrow = 5,
#                    scale = 1) # increase or reduce the size of the map
# 
# ggsave("grid_cowplot.tiff", plot = prow2, dpi = 500, width = 172, height = 300, units = "mm",
#        compression="lzw", type="cairo") 
# 



# # grid
# # with this command you can plot a grid of two or more plots.
# # To add more than two plots just write their names and modyfy the columns and rows numbers
# maps = grid.arrange(DL_avg_E, EC_avg_E, leg,  ncol=3, nrow =1, widths = c(2.3, 2.3, 0.8))
#  
# ggsave("maps EC vs DL.tiff", plot = maps, dpi = 500, width = 190, height = 75, units = "mm",
#        compression="lzw", type="cairo")
#  
# # Example to create 2x2 grid
# mapsTest = grid.arrange(DL_avg_E, EC_avg_E, DL_avg_E, EC_avg_E, ncol=2, nrow =2)
#  
# ggsave("maps EC vs DL TEST.tiff", plot = mapsTest, dpi = 500, width = 190, height = 150, units = "mm",
#        compression="lzw", type="cairo")

# theme(axis.line.x = element_line(color="white", size = 1)) + 

# Dashed version 
# ggplot(data = df, aes(year)) + 
#   geom_line(aes(y =  Cropland, colour = "Cropland"), linetype = "dashed", size = 0.8) +
#   geom_line(aes(y =  Pasture, colour = "Pasture"), linetype = "dashed", size = 0.8) +
#   geom_line(aes(y =  Forest, colour = "Forest"), linetype = "dashed", size = 0.8) +
#   geom_line(aes(y =  OtherNaturalLand, colour = "Other Natural Land"), linetype = "dashed", size = 0.8) +
#   theme_few() + 
#   ggtitle("Land Cover") + xlab("Year") + ylab("million ha") +
#   theme(legend.position = "bottom") +
#   scale_color_manual(labels = c("Cropland", "Pasture", "Forest", "Other Natural Land"), 
#                      values = c("#DF1895", "#F48324", "#38A800", "#3D64B7"),
#                      name = " ")


# # Beautiful color combination
# 
# for_pn_dem = ggplot(data = df_for_pn, aes(year)) +
#   geom_line(aes(y =  Forest_Residues, colour = "Forest_Residues"), size = 0.8) +
#   geom_line(aes(y =  Roundwood_pn, colour = "Roundwood_pn"), size = 0.8) +
#   geom_line(aes(y =  Roundwood_demand, colour = "Roundwood_demand"), linetype = "dashed", size = 0.8) +
#    theme_few()+ ylim(0,10000) +
#   ggtitle("Forestry production and demand") + xlab("Year") + ylab("million t DM/ha/yr") +
#   theme(legend.position = "bottom") +
#   theme(plot.background = element_rect(fill="lightblue"),
#          panel.background = element_rect(fill='#D6E7EF'),
#          panel.grid.major = element_line(size = 0.7, colour = 'white'), 
#          panel.grid.minor = element_blank(), 
#          axis.ticks = element_blank()) +
#   panel_border(remove = T) +
#   scale_color_manual(labels = c("Forest Residues", "Roundwood production", "Roundwood demand"), 
#                      values = c("#3384CD", "#67000D", "#2DCBD3"),
#                      name = " ")