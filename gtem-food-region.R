
# This code generates graphs to compare price and outputs projections for selected commodities
# It relies on data from GLOBIOM, ANO 2019, and FAOSTAT.

# Required packages
x = c("forecast", "data.table", "lubridate", "ggplot2", "ggthemes",  "gridExtra", 
      "cowplot", "plotly", "matrixStats")
lapply(x, library, character.only = TRUE);



# -------------------------------------------------------------------------------------------
# Data preparation
# -------------------------------------------------------------------------------------------
# Clear workspace
rm(list=ls())

x =  c("readxl", "data.table")

lapply(x, library, character.only = TRUE);

# old data
setwd("C:/Users/mar77v/CSIRO/ABARES CSIRO Modeling - Visualization/gtem-visual") 

# Data Nov 19
setwd("C:/Users/mar77v/CSIRO/ABARES CSIRO Modeling - Visualization/results_nov") 

# -----------------------------------------------------------------------------
# Select country data and transpose it
regionID = "1 Australia"
regANO ="AustraliaReg" # ID in ANO scenarios - Globiom
price_output_ID = 2 
#-----------------------------------------------------------------------------




# Import data from excel files

read_excel_allsheets <- function(filename, tibble = FALSE) {
        # I prefer straight data.frames
        # but if you like tidyverse tibbles (the default with read_excel)
        # then just pass tibble = TRUE
        sheets <- readxl::excel_sheets(filename)
        x <- lapply(sheets, function(X) readxl::read_excel(filename, sheet = X))
        if(!tibble) x <- lapply(x, as.data.frame)
        names(x) <- sheets
        x
}

prices_gtem = read_excel_allsheets("sector price index.xlsx")



# Prices


# Select region specific data.

# column number
prices = as.data.frame(prices_gtem[1])[, c(1, price_output_ID)]

# Concatenate all the periods

for(j in 2:length(prices_gtem)){
        print(j)
        prices = cbind(prices, as.data.frame(prices_gtem[j])[,price_output_ID]) 
}

colnames(prices) = c("item", seq(2015,2060,5))

# Adjust data for inflation using CPI

cpi = read.csv("CPI.csv")

# Adjust the data based on phone chat with YL
cpi[,2:dim(cpi)[2]] =  (cpi[,2:dim(cpi)[2]] - 1) * 100


# select five-year data
# cpi5y = subset(cpi, select = c("L_CPI", "X2.year2015", "X7.year2020", 
#                                 "X12.year2025", "X17.year2030",
#                                 "X22.year2035", "X27.year2040", 
#                                 "X32.year2045", "X37.year2050", 
#                                 "X42.year2055", "X47.year2060"))

# select five-year data
cpi5y = subset(cpi, select = c("L_CPI", "X2015", "X2020", 
                               "X2025", "X2030",
                               "X2035", "X2040", 
                               "X2045", "X2050", 
                               "X2055", "X2060"))

cpi_region = cpi5y[cpi5y$L_CPI == regionID, ]
# make the data the same lenght as the price index data
cpi_region = cpi_region[rep(row.names(cpi_region), 
                            dim(prices)[1]),]

# remove inflation values from price index

prices.adj = prices
prices.adj[,2:dim(prices)[2]] =  prices.adj[,2:dim(prices)[2]] - cpi_region[,2:dim(cpi_region)[2]]

# Add 100 indicator to prices
prices.adj[,2:dim(prices)[2]]  = prices.adj[,2:dim(prices)[2]]  + 100

# Interpolate the data to generate a yearly dataset

## First, transform data from wide -> long format, clean year column
# or use reshape2::melt
prices.adj <- melt(as.data.table( prices.adj), id.vars='item', variable.name='year')  # wide -> long
prices.adj[, year := as.integer(sub('[[:alpha:]]', '', year))]                      # convert years to integers

## Function to interpolate at constant rate for each interval
interp <- function(yrs, values) {
        tt <- diff(yrs)               # interval lengths
        N <- head(values, -1L)     
        P <- tail(values, -1L)
        r <- (log(P) - log(N)) / tt   # rate for interval
        const_rate <- function(N, r, time) N*exp(r*(0:(time-1L)))
        list(year=seq.int(min(yrs), max(yrs), by=1L),
             value=c(unlist(Map(const_rate, N, r, tt)), tail(P, 1L)))
}

# The geometric interpolation does not work if there are neg vals

## geometric interpolation for each item
gtem.prices <- prices.adj[, interp(year, value), by= item]

# ## Plot

ggplot(gtem.prices, aes(year, value, color=item)) +
        geom_line(lwd=1) + theme_bw() 

ggplotly(
        p = ggplot2::last_plot(),
        width = NULL,
        height = NULL,
        tooltip = "all",
        dynamicTicks = FALSE,
        layerData = 1,
        originalData = TRUE,
        source = "A",
)


# Output


output.A = read_excel_allsheets("sector output index.xlsx")

# Select AU data. Note for other countries we need to indicate the corresponding
# column number
output = as.data.frame(output.A[1])[,c(1, price_output_ID )]
# Concatenate all the periods

for(j in 2:length( output.A)){
        # print(j)
        output = cbind(output, as.data.frame(output.A[j])[,price_output_ID]) 
}

colnames(output) = c("item", seq(2015,2060,5))

# Add 100 to be able to interpolate
output[, 2: dim(output)[2]] = output[, 2: dim(output)[2]] +100

# FTransform data from wide -> long format, clean year column
# or use reshape2::melt
output.adj <- melt(as.data.table( output), id.vars='item', variable.name='year')  # wide -> long
output.adj[, year := as.integer(sub('[[:alpha:]]', '', year))]                      # convert years to integers


## geometric interpolation for each item
gtem.output <- output.adj[, interp(year, value), by= item]

# ## Plot

p.output = ggplot(gtem.output, aes(year, value, color=item)) +
        geom_line(lwd=1) + theme_cowplot() 
ggplotly(
        p = p.output,
        width = NULL,
        height = NULL,
        tooltip = "all",
        dynamicTicks = FALSE,
        layerData = 1,
        originalData = TRUE,
        source = "A",
)



# write.csv(output, "sector output AU.csv")





# -------------------------------------------------------------------------------------------
# Visualization
# -------------------------------------------------------------------------------------------

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
nat_first = read.csv("F:/Dropbox/Australia/GLOBIOM/latest_results/FourDegrees/IIASA_GLOBIOM_emulator_interpolated_output 1990-2100.csv")

nat_first = nat_first[nat_first$Regions == regANO,]

# nat_first[,5:14] = NULL
worktog = read.csv("C:/Users/mar77v/Dropbox/Australia/GLOBIOM/latest_results/TwoDegrees/IIASA_GLOBIOM_emulator_interpolated_output 1990-2100.csv")
worktog = read.csv("F:/Dropbox/Australia/GLOBIOM/latest_results/TwoDegrees/IIASA_GLOBIOM_emulator_interpolated_output 1990-2100.csv")


worktog = worktog[worktog$Regions == regANO,]
# worktog[,5:14] = NULL


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



#------------------------------------------------------------------------
# Meat production
#------------------------------------------------------------------------

tiff("meat & dairy GTEM.tiff", compression = "lzw", res = 500, width = 160, height = 160, units = "mm")

par(mfrow=c(3,3), cex = 0.6)

#------------------------------------------------------------------------

# Function to plot GTEM, Globiom, ANO2 , and historical projections

gtem.Pn.chart <- function(comID.ANO, comID.hist, comID.GTEM, sm.factor){
  
  # Globiom
  
  globiom = t(as.matrix(pn[pn$lu == comID.ANO,]))
  colnames(globiom) = globiom["ghg",] # name of multiple GLOBIOM projections
  
  # Get median value over SSP2 projections from GLOBIOM data
  globiom.rep = (as.data.frame(globiom[4:74, 1:96]))
  # Convert character data to numeric
  globiom.rep[] <- lapply(globiom.rep, function(x) as.numeric(as.character(x)))
  # globiom.rep = rowMeans(globiom.rep, na.rm = T)
  globiom.rep = rowMedians(as.matrix(globiom.rep))
  
  #WT
  wt_meat = worktog[worktog$ParameterName == comID.ANO,]
  wt_meat_pn = as.numeric(wt_meat[wt_meat$ParameterType == "PROD", 5:75])
  # lines(wt_meat_pn, col = "#6666FF", lty = 1, lwd = 2 )
  # NF
  nf_meat = nat_first[nat_first$ParameterName == comID.ANO,]
  nf_meat_pn = as.numeric(nf_meat[nf_meat$ParameterType == "PROD", 5:75])
  
  # GTEM
  getm.com = gtem.output[gtem.output$item == comID.GTEM,]
  tmp =  as.data.frame(c(rep(NA, 2015-1990), getm.com$value))
  # Multiply the index by the average of historical FAOSTAT data
  base.com = mean(as.numeric(histpn[comID.hist, c("X2010", "X2011","X2012", "X2013", "X2014")]))
  # tmp$year = 1990:2060
  gtem.proj.pn = base.com * tmp/100
  colnames(gtem.proj.pn) = "GTEM"
  gtem.proj.pn[25,1] = base.com
  
  
  # Add historical trend analysis 
  
  jf = sm.factor
  #Smoothen the time series data to improve the analysis of long term trends
  sm = lowess(x =  1990:2014, y = as.numeric(histpn[comID.hist,1:25]), f = jf)
  # Format data as time series 
  ax = ts(sm$y, start = 1990, end = 2014, frequency = 1)
  # Fit a ETS model to the time series
  ax.fit = ets(ax, damped = T)
  # Project using the fitted model
  ax.proj = as.data.frame(forecast.ets(ax.fit, h = 46))
  
  # merge historical and projected data
  
  tF = 2060 # Final year
  tI = 2014 # initial year 
  
  # save the  point forecasts, bounds and fitted data
  t = tF - 1990 + 1
  df_shares = data.frame(matrix(vector(), t , 8))
  colnames(df_shares) =c("historical", "fitted", "LB80", "LB95", "trend", "UB80", "UB95", "year")
  df_shares["year"] = 1990:tF
  df_shares[1:(t - (tF-tI)),"historical"] = as.numeric(histpn[comID.hist,1:25])
  df_shares[1:(t - (tF-tI)),"fitted"] = as.numeric(fitted(ax.fit) )
  df_shares[(t - (tF - tI) + 1):t,"LB80"] = as.numeric(ax.proj[,"Lo 80"])
  df_shares[(t - (tF - tI) + 1):t,"LB95"] = as.numeric(ax.proj[,"Lo 95"])
  df_shares[(t - (tF - tI ) + 1):t,"trend"] = as.numeric( ax.proj[,"Point Forecast"])
  df_shares[(t - (tF - tI ) + 1):t,"UB80"] = as.numeric( ax.proj[,"Hi 80"])
  df_shares[(t - (tF - tI ) + 1):t,"UB95"] = as.numeric( ax.proj[,"Hi 95"])
  
  df_shares[df_shares < 0] = 0
  
  # Add the 2014 data to the projections to avoid a visual "gap"
  df_shares[(t - (tF - tI )), 3:7] =  as.numeric(df_shares[(t - (tF - tI )), 2])
  
  # Add ANO and globiom projections to the data
  df_shares$WT = wt_meat_pn
  df_shares$NF = nf_meat_pn
  df_shares$GTEM = gtem.proj.pn$GTEM
  df_shares$GLOBIOM = globiom.rep
  
  # map colors to create separate legends
  colores <- c("historical"="black","fitted"="grey80","trend"="grey40", 
               "95% C.I." = "#FFE5E5", "80% C.I." = "#E5CEE8",
               "Working Together" = "#1f78b4", 
               "Nations First" = "#ff7f00", 
               "GLOBIOM" = "#b2df8a",
               "GTEM-Food" = "#c51b8a")
  cis <- c("80% C.I."="blue","95% C.I."="red")
  
  # Plot
  comPlot = ggplot(data=df_shares, aes(x=year)) + 
    geom_line(aes(y=historical, color = "historical"), size = 0.8) +
    scale_x_continuous(breaks=c(seq(1990,tF, 10))) +
    scale_y_continuous(labels=function(x) format(x, big.mark = ",", scientific = FALSE)) +
    geom_line(aes(y = fitted, color = "fitted"), size = 0.8, linetype = "dashed") +
    #geom_ribbon(aes(ymin=LB95, ymax=UB95, fill = "95% C.I."), linetype=2, alpha=0.1) + 
    #geom_ribbon(aes(ymin=LB80, ymax=UB80, fill = "80% C.I."), linetype=2, alpha=0.1) + 
    geom_line(aes(y = trend, color = "trend"), size = 0.8, linetype = "dashed") +
    ylab( "1000 tonnes") + 
    # Add  projections
    geom_line(aes(y = WT, color = "Working Together"), size = 0.8, linetype = "dashed") + 
    geom_line(aes(y = NF, color = "Nations First" ), size = 0.8, linetype = "dashed") +  
    geom_line(aes(y = GLOBIOM, color = "GLOBIOM"), size = 0.8, linetype = "dashed") +
    geom_line(aes(y = GTEM, color = "GTEM-Food"), size = 1, linetype = "solid") +  
     
    # This changes the height of the line symbols
    guides(colour = guide_legend(override.aes = list(size=1))) + 
    scale_colour_manual(name= "" ,values=colores) +
    scale_fill_manual(name="",values=cis) +
    # annotate("text", label = as.character(round(df_shares$LB80[t]), 0), 
    #          x = tF + 2, y = df_shares$LB80[t], size = 3, colour = "grey20") +
    # annotate("text", label = as.character(round(df_shares$UB80[t]), 0), 
    #          x = tF + 2, y = df_shares$UB80[t], size = 3, colour = "grey20") +
    # annotate("text", label = as.character(round(df_shares$LB95[t]), 0), 
    #          x = tF + 2, y = df_shares$LB95[t], size = 3, colour = "grey20") +
    # annotate("text", label = as.character(round(df_shares$UB95[t]), 0), 
    #          x = tF + 2, y = df_shares$UB95[t], size = 3, colour = "grey20") +
    # annotate("text", label = as.character(round(df_shares$trend[t]), 0), 
    #          x = tF + 2, y = df_shares$trend[t], size = 3, colour = "grey20") +
    theme_clean() +
    theme(legend.position = "bottom",
          legend.box = "horizontal",
          legend.background = element_rect(color = NA),
          plot.background =  element_blank()) +
    labs(title = paste("ANO:", comID.ANO, "- Historical:", 
                       comID.hist, "- GTEM-Food:", comID.GTEM, sep = " "),
         subtitle = "1990 - 2060",
         caption = "GTEM-Food")
  
  graph = ggplotly(comPlot)
  
  return(graph)
  
}




# Historical classes
# [1] "barley"         "Bean dried"     "Chick peas"     "Corn"           "Cotton"        
# [6] "Groundnuts"     "Millets"        "Potatoes"       "Rapeseeds"      "Rice"          
# [11] "Soybeans"       "Sorghum"        "Sugar cane"     "Sunflower"      "Sweet potatoes"
# [16] "Wheat"          "BVMEAT"         "SGMEAT"         "PGMEAT"         "PTMEAT"        
# [21] "ALMILK"         "PTEGGS" 

# ANO
# "Pota"  "SGMEAT"  "Sunf"   "OPAL"   "ALMILK"   "Rice"  "Srgh"   "FW_Biomass"  
# "Cott", "ChkP"   "Gnut" "SwPo"  "PGMEAT" "Mill"  "Soya" "Barl" "Sawnwood" "OW_Biomass"                                             
# "Corn"  "Whea"  "Fiberboard" "Rape" "SugC" "IP_Biomass" "BeaD"  "PTEGGS"  "PTMEAT"   
# "PW_Biomass"   "MechPulp"  "Plywood"  "Cass"  "ChemPulp"   "SW_Biomass"   "BVMEAT"      

# Globiom classes (subset) TSame ids as ANO
# [1] "Barl"   "BeaD"   "ChkP"   "Corn"   "Cott"   "Gnut"   "Mill"  
# [8] "Pota"   "Rape"   "Rice"   "Soya"   "Srgh"   "SugC"   "Sunf"  
# [15] "SwPo"   "Whea"   "BVMEAT" "SGMEAT" "PGMEAT" "PTMEAT" "ALMILK"
# [22] "PTEGGS"

# GTEM classes

# [1] "1 Land"          "2 Labour"        "3 Capital"       "4 NatlRes"       "5 Rice"          "6 Wheat"        
# [7] "7 Oth_grains"    "8 Veg_fruit"     "9 Oil_seeds"     "10 Cane_beet"    "11 Fibres_crops" "12 Oth_crops"   
# [13] "13 Live_cattle"  "14 Live_pig"     "15 Live_poultry" "16 Raw_milk"     "17 Meat_cattle"  "18 Meat_pork"   
# [19] "19 Meat_poultry" "20 Fishery"      "21 Wool"         "22 Forestry"     "23 Veg_oils"     "24 Dairy_milk"  
# [25] "25 Proc_rice"    "26 Sugar"        "27 Oth_foods"    "28 Beve_tobacco" "29 Farm_Forest"  "30 COL"         
# [31] "31 OIL"          "32 GAS"          "33 P_C"          "34 ELY"          "35 I_S"          "36 OTP"         
# [37] "37 Oth_trans"    "38 Ene_Intens"   "39 Machi_equip"  "40 Services"     "41 oxt"          "42 chm"         
# [43] "43 wtr"          "44 CGDS"    


# Crops
gtem.Pn.chart(comID.ANO = "Rice", comID.hist = "Rice", comID.GTEM = "5 Rice", sm.factor = 0.01)
gtem.Pn.chart(comID.ANO = "Whea", comID.hist = "Wheat", comID.GTEM = "6 Wheat" , sm.factor = 0.01)
gtem.Pn.chart(comID.ANO = "SugC" , comID.hist = "Sugar cane" , comID.GTEM = "10 Cane_beet"  , sm.factor = 0.01)


# Livestock
gtem.Pn.chart(comID.ANO = "BVMEAT", comID.hist = "BVMEAT", comID.GTEM = "17 Meat_cattle", sm.factor =0.4)
gtem.Pn.chart(comID.ANO = "PGMEAT", comID.hist = "PGMEAT", comID.GTEM = "18 Meat_pork" , sm.factor =0.4)
gtem.Pn.chart(comID.ANO = "PTMEAT", comID.hist = "PTMEAT", comID.GTEM = "19 Meat_poultry" , sm.factor =0.4)
gtem.Pn.chart(comID.ANO = "ALMILK", comID.hist = "ALMILK", comID.GTEM = "24 Dairy_milk" , sm.factor =0.4)






# 
# 
# 
# 
# 
# #------------------------------------------------------------------------
# # Beef price
# 
# 
# 
# globiom = t(as.matrix(prices[prices$lu == comID,]))
# 
# colnames(globiom) = globiom["ghg",]
# 
# # Get mean value
# globiom.rep = (as.data.frame(globiom[4:74, 1:96]))
# # Convert character data to numeric
# globiom.rep[] <- lapply(globiom.rep, function(x) as.numeric(as.character(x)))
# # globiom.rep = rowMeans(globiom.rep, na.rm = T)
# globiom.rep = rowMedians(as.matrix(globiom.rep))
# 
# 
# par(mar = c(4,4,1,0))
# # matplot(globiom[4:74, 1:96], type = "l", ylab = " ",  xlab = " ",
# #         col = "#72CC98", axes = F, ylim=c(0,3000)) #plot
# # plot(globiom[4:74, "GHG020_BIO0N"], type = "l", ylab = " ",  xlab = " ",
# #              col = "#72CC98", axes = F, ylim=c(0,3000))
# 
# plot(globiom.rep, type = "l", ylab = " ",  xlab = " ",
#      col = "#72CC98", axes = F, ylim=c(0,3000),  lwd = 2)
# 
# # matplot(globiom[4:74, 1:96], type = "l", ylab = " ",  xlab = " ",
# #         col = "#72CC98", axes = F, ylim=c(0,20000)) #plot
# 
# axis(1, at = seq(0,71,10), labels = seq(1990,2060,10), cex.axis = 0.7)
# axis(2, at = seq(0,3000,500), labels = seq(0,3000,500), cex.axis = 0.7)
# 
# lines(as.numeric(histpric[comID,]), col = "black", lty = 3, lwd = 2 )
# 
# wt_meat = worktog[worktog$ParameterName == comID,]
# wt_meat_pr = as.numeric(wt_meat[wt_meat$ParameterType == "XPRP", 5:75])
# 
# nf_meat = nat_first[nat_first$ParameterName == comID,]
# nf_meat_pr = (nf_meat[nf_meat$ParameterType == "XPRP", 5:75])
# 
# lines(nf_meat_pr, col = "#C00000", lty = 1, lwd = 2 )
# lines(wt_meat_pr, col = "#6666FF", lty = 1, lwd = 2 )
# 
# #GTEM
# getm.com.price = as.numeric(gtem.prices[gtem.prices$item == comID.GTEM, 32:dim(gtem.prices)[2]])
# 
# getm.com.price  = 1452.81 + getm.com.price * 1452.81/100 # 1452.81 is the beef price in 2014 from FAOSTAT
# 
# lines(getm.com.price, col = "orange", lty = 1, lwd = 2 )
# 
# # legend("bottomright", legend = c("Historical", "GLOBIOM"), col= c("#2DCBD3","#72CC98" ), 
# #        lty = c(2,1), bty = "n") # optional legend
# title(main = "d) Beef prices", ylab = "USD 2000 per ton",  xlab = " ", cex=0.9)
# 
# #------------------------------------------------------------------------
# # Pork price
# 
# globiom = t(as.matrix(prices[prices$lu == "PGMEAT",]))
# colnames(globiom) = globiom["ghg",]
# 
# # Get mean value
# globiom.rep = (as.data.frame(globiom[4:74, 1:96]))
# # Convert character data to numeric
# globiom.rep[] <- lapply(globiom.rep, function(x) as.numeric(as.character(x)))
# # globiom.rep = rowMeans(globiom.rep, na.rm = T)
# globiom.rep = rowMedians(as.matrix(globiom.rep))
# 
# par(mar = c(4,4,1,0))
# # matplot(globiom[4:74, 1:96], type = "l", ylab = " ",  xlab = " ",
# #         col = "#72CC98", axes = F, ylim=c(0,3000)) #plot
# # plot(globiom[4:74, "GHG020_BIO0N"], type = "l", ylab = " ",  xlab = " ",
# #              col = "#72CC98", axes = F, ylim=c(0,3000))
# 
# plot(globiom.rep, type = "l", ylab = " ",  xlab = " ",
#      col = "#72CC98", axes = F, ylim=c(0,3000),  lwd = 2)
# # matplot(globiom[4:74, 1:96], type = "l", ylab = " ",  xlab = " ",
# #         col = "#72CC98", axes = F, ylim=c(0,20000)) #plot
# axis(1, at = seq(0,71,10), labels = seq(1990,2060,10), cex.axis = 0.7)
# axis(2, at = seq(0,3000,500), labels = seq(0,3000,500), cex.axis = 0.7)
# 
# lines(as.numeric(histpric["PGMEAT",]), col = "black", lty = 3, lwd = 2 )
# 
# wt_meat = worktog[worktog$ParameterName == "PGMEAT",]
# wt_meat_pr = as.numeric(wt_meat[wt_meat$ParameterType == "XPRP", 5:75])
# 
# nf_meat = nat_first[nat_first$ParameterName == "PGMEAT",]
# nf_meat_pr = as.numeric(nf_meat[nf_meat$ParameterType == "XPRP", 5:75])
# 
# lines(nf_meat_pr, col = "#C00000", lty = 1, lwd = 2 )
# lines(wt_meat_pr, col = "#6666FF", lty = 1, lwd = 2 )
# 
# #GTEM
# gtem.pork.price = as.numeric(gtem.prices[gtem.prices$item == "18 Meat_pork", 32:dim(gtem.prices)[2]])
# 
# gtem.pork.price  = 1242.13 + gtem.pork.price * 1242.13/100 # 1242.13 is the pork price in 2014 from FAOSTAT
# 
# lines(gtem.pork.price, col = "orange", lty = 1, lwd = 2 )
# 
# title(main = "e) Pork prices", ylab = "", cex=0.9)
# 
# #------------------------------------------------------------------------
# # # Dairy  price
# 
# globiom = t(as.matrix(prices[prices$lu == "ALMILK",]))
# colnames(globiom) = globiom["ghg",]
# 
# # Get mean value
# globiom.rep = (as.data.frame(globiom[4:74, 1:96]))
# # Convert character data to numeric
# globiom.rep[] <- lapply(globiom.rep, function(x) as.numeric(as.character(x)))
# # globiom.rep = rowMeans(globiom.rep, na.rm = T)
# globiom.rep = rowMedians(as.matrix(globiom.rep))
# 
# par(mar = c(4,4,1,0))
# # matplot(globiom[4:74, 1:96], type = "l", ylab = " ",  xlab = " ",
# #         col = "#72CC98", axes = F, ylim=c(0,3000)) #plot
# # plot(globiom[4:74, "GHG020_BIO0N"], type = "l", ylab = " ",  xlab = " ",
# #              col = "#72CC98", axes = F, ylim=c(0,3000))
# 
# plot(globiom.rep, type = "l", ylab = " ",  xlab = " ",
#      col = "#72CC98", axes = F, ylim=c(0,500),  lwd = 2)
# # matplot(globiom[4:74, 1:96], type = "l", ylab = " ",  xlab = " ",
# #         col = "#72CC98", axes = F, ylim=c(0,20000)) #plot
# axis(1, at = seq(0,71,10), labels = seq(1990,2060,10), cex.axis = 0.7)
# axis(2, at = seq(0,500,100), labels = seq(0,500,100), cex.axis = 0.7)
# 
# lines(as.numeric(histpric["ALMILK",]), col = "black", lty = 3, lwd = 2 )
# 
# wt_meat = worktog[worktog$ParameterName == "ALMILK",]
# wt_meat_pr = as.numeric(wt_meat[wt_meat$ParameterType == "XPRP", 5:75])
# 
# nf_meat = nat_first[nat_first$ParameterName == "ALMILK",]
# nf_meat_pr = as.numeric(nf_meat[nf_meat$ParameterType == "XPRP", 5:75])
# 
# lines(nf_meat_pr, col = "#C00000", lty = 1, lwd = 2 )
# lines(wt_meat_pr, col = "#6666FF", lty = 1, lwd = 2 )
# 
# #GTEM
# gtem.milk.price = as.numeric(gtem.prices[gtem.prices$item == "24 Dairy_milk", 32:dim(gtem.prices)[2]])
# 
# gtem.milk.price  = 177.57 + gtem.milk.price * 177.57/100 # 177.57 is the milk price in 2014 from FAOSTAT
# 
# lines(gtem.milk.price, col = "orange", lty = 1, lwd = 2 )
# title(main = "f) Milk prices", ylab = " ",  xlab = " ",cex=0.9)
# 
# 
# plot(NULL ,xaxt='n',yaxt='n',bty='n',ylab='',xlab='', xlim=0:1, ylim=0:1)
# 
# 
# plot(NULL ,xaxt='n',yaxt='n',bty='n',ylab='',xlab='', xlim=0:1, ylim=0:1)
# 
# legend("topright", legend = c("Historical (FAOSTAT)", "GLOBIOM - SSP2", "Working Together (ANO 2019)", "Nations First (ANO 2019)", "GTEM-Food"), 
#        col= c("black","#72CC98", "#6666FF", "#C00000", "orange"), 
#        lty = c(2,1,1,1,1), lwd = c(2,2,2,2,2), bty = "n", cex = 1) # optional legend
# 
# mtext("Projections", at=0.1, cex=0.75)
# 
# dev.off()
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# #---------------------------------------------------------------------------------------
# # Crops
# #---------------------------------------------------------------------------------------
# 
# tiff("crops GTEM.tiff", compression = "lzw", res = 500, width = 160, height = 160, units = "mm")
# 
# par(mfrow=c(3,3), cex = 0.6)
# 
# #------------------------------------------------------------------------
# # Wheat production
# 
# globiom = t(as.matrix(pn[pn$lu == "Whea",]))
# colnames(globiom) = globiom["ghg",]
# 
# # Get mean value
# globiom.rep = (as.data.frame(globiom[4:74, 1:96]))
# # Convert character data to numeric
# globiom.rep[] <- lapply(globiom.rep, function(x) as.numeric(as.character(x)))
# # globiom.rep = rowMeans(globiom.rep, na.rm = T)
# globiom.rep = rowMedians(as.matrix(globiom.rep))
# 
# par(mar = c(4,4,1,0))
# # matplot(globiom[4:74, 1:96], type = "l", ylab = " ",  xlab = " ",
# #         col = "#72CC98", axes = F, ylim=c(0,3000)) #plot
# # plot(globiom[4:74, "GHG020_BIO0N"], type = "l", ylab = " ",  xlab = " ",
# #              col = "#72CC98", axes = F, ylim=c(0,3000))
# 
# plot(globiom.rep, type = "l", ylab = " ",  xlab = " ",
#      col = "#72CC98", axes = F, ylim=c(0,60000),  lwd = 2)
# 
# axis(1, at = seq(0,71,10), labels = seq(1990,2060,10), cex.axis = 0.7)
# axis(2, at = seq(0,60000,20000), labels = seq(0,60000,20000), cex.axis = 0.7)
# 
# lines(as.numeric(histpn["Whea",]), col = "black", lty = 3, lwd = 2)
# 
# #WT
# wt_crop = worktog[worktog$ParameterName == "Whea",]
# wt_crop_pn = as.numeric(wt_crop[wt_crop$ParameterType == "PROD", 5:75])
# lines(wt_crop_pn, col = "#6666FF", lty = 1, lwd = 2 )
# # NF
# nf_crop = nat_first[nat_first$ParameterName == "Whea",]
# nf_crop_pn = as.numeric(nf_crop[nf_crop$ParameterType == "PROD", 5:75])
# 
# lines(nf_crop_pn, col = "#C00000", lty = 1, lwd = 2 )
# 
# # GTEM
# gtem.C = as.numeric(gtem.output[gtem.output$item == "6 Wheat", 32:dim(gtem.output)[2]])
# lines(gtem.C, col = "orange", lty = 1, lwd = 2 )
# 
# # legend("bottomright", legend = c("Historical", "GLOBIOM"), col= c("#2DCBD3","#72CC98" ), 
# #        lty = c(2,1), bty = "n") # optional legend
# title(main = "a) Wheat output", ylab = "1000 tons",  xlab = " ", cex=0.9 )
# 
# #------------------------------------------------------------------------
# # Rice production
# 
# globiom = t(as.matrix(pn[pn$lu == "Rice",]))
# colnames(globiom) = globiom["ghg",]
# 
# # Get mean value
# globiom.rep = (as.data.frame(globiom[4:74, 1:96]))
# # Convert character data to numeric
# globiom.rep[] <- lapply(globiom.rep, function(x) as.numeric(as.character(x)))
# # globiom.rep = rowMeans(globiom.rep, na.rm = T)
# globiom.rep = rowMedians(as.matrix(globiom.rep))
# 
# 
# par(mar = c(4,4,1,0))
# # matplot(globiom[4:74, 1:96], type = "l", ylab = " ",  xlab = " ",
# #         col = "#72CC98", axes = F, ylim=c(0,3000)) #plot
# # plot(globiom[4:74, "GHG020_BIO0N"], type = "l", ylab = " ",  xlab = " ",
# #              col = "#72CC98", axes = F, ylim=c(0,3000))
# 
# plot(globiom.rep, type = "l", ylab = " ",   xlab = " ",
#      col = "#72CC98", axes = F, ylim=c(0, 10000),  lwd = 2)
# 
# axis(1, at = seq(0,71,10), labels = seq(1990,2060,10), cex.axis = 0.7)
# axis(2, at = seq(0,10000,2500), labels = seq(0,10000,2500), cex.axis = 0.7)
# 
# lines(as.numeric(histpn["Rice",]), col = "black", lty = 3, lwd = 2 )
# 
# #WT
# wt_crop = worktog[worktog$ParameterName == "Rice",]
# wt_crop_pn = as.numeric(wt_crop[wt_crop$ParameterType == "PROD", 5:75])
# lines(wt_crop_pn, col = "#6666FF", lty = 1, lwd = 2 )
# # NF
# nf_crop = nat_first[nat_first$ParameterName == "Rice",]
# nf_crop_pn = as.numeric(nf_crop[nf_crop$ParameterType == "PROD", 5:75])
# 
# lines(nf_crop_pn, col = "#C00000", lty = 1, lwd = 2 )
# 
# # GTEM
# gtem.Rice = as.numeric(gtem.output[gtem.output$item == "5 Rice", 32:dim(gtem.output)[2]])
# lines(gtem.Rice, col = "orange", lty = 1, lwd = 2 )
# 
# # legend("bottomright", legend = c("Historical", "GLOBIOM"), col= c("#2DCBD3","#72CC98" ), 
# #        lty = c(2,1), bty = "n") # optional legend
# title(main = "b) Rice output", ylab = " ",  xlab = " ", cex=0.9 )
# 
# 
# #------------------------------------------------------------------------
# # Sugar crops production
# 
# globiom = t(as.matrix(pn[pn$lu == "SugC",]))
# colnames(globiom) = globiom["ghg",]
# 
# # Get mean value
# globiom.rep = (as.data.frame(globiom[4:74, 1:96]))
# # Convert character data to numeric
# globiom.rep[] <- lapply(globiom.rep, function(x) as.numeric(as.character(x)))
# # globiom.rep = rowMeans(globiom.rep, na.rm = T)
# globiom.rep = rowMedians(as.matrix(globiom.rep))
# 
# par(mar = c(4,4,1,0))
# # matplot(globiom[4:74, 1:96], type = "l", ylab = " ",  xlab = " ",
# #         col = "#72CC98", axes = F, ylim=c(0,3000)) #plot
# # plot(globiom[4:74, "GHG020_BIO0N"], type = "l", ylab = " ",  xlab = " ",
# #              col = "#72CC98", axes = F, ylim=c(0,3000))
# 
# plot(globiom.rep, type = "l", ylab = " ",  xlab = " ",
#      col = "#72CC98", axes = F, ylim=c(0,80000),  lwd = 2)
# 
# axis(1, at = seq(0,71,10), labels = seq(1990,2060,10), cex.axis = 0.7)
# axis(2, at = seq(0,80000,20000), labels = seq(0,80000,20000), cex.axis = 0.7)
# 
# lines(as.numeric(histpn["Sugar cane",]), col = "black", lty = 3, lwd = 2 )
# 
# #WT
# wt_crop = worktog[worktog$ParameterName == "SugC",]
# wt_crop_pn = as.numeric(wt_crop[wt_crop$ParameterType == "PROD", 5:75])
# lines(wt_crop_pn, col = "#6666FF", lty = 1, lwd = 2 )
# # NF
# nf_crop = nat_first[nat_first$ParameterName == "SugC",]
# nf_crop_pn = as.numeric(nf_crop[nf_crop$ParameterType == "PROD", 5:75])
# 
# lines(nf_crop_pn, col = "#C00000", lty = 1, lwd = 2 )
# 
# # GTEM
# gtem.sugar = as.numeric(gtem.output[gtem.output$item == "10 Cane_beet", 32:dim(gtem.output)[2]])
# lines(gtem.sugar, col = "orange", lty = 1, lwd = 2 )
# 
# # legend("bottomright", legend = c("Historical", "GLOBIOM"), col= c("#2DCBD3","#72CC98" ), 
# #        lty = c(2,1), bty = "n") # optional legend
# title(main = "c) Sugar crops output", ylab = " ",  xlab = " ", cex=0.9 )
# 
# 
# 
# #------------------------------------------------------------------------
# # Wheat price
# 
# globiom = t(as.matrix(prices[prices$lu == "Whea",]))
# 
# colnames(globiom) = globiom["ghg",]
# 
# # Get mean value
# globiom.rep = (as.data.frame(globiom[4:74, 1:96]))
# # Convert character data to numeric
# globiom.rep[] <- lapply(globiom.rep, function(x) as.numeric(as.character(x)))
# # globiom.rep = rowMeans(globiom.rep, na.rm = T)
# globiom.rep = rowMedians(as.matrix(globiom.rep))
# 
# 
# par(mar = c(4,4,1,0))
# # matplot(globiom[4:74, 1:96], type = "l", ylab = " ",  xlab = " ",
# #         col = "#72CC98", axes = F, ylim=c(0,3000)) #plot
# # plot(globiom[4:74, "GHG020_BIO0N"], type = "l", ylab = " ",  xlab = " ",
# #              col = "#72CC98", axes = F, ylim=c(0,3000))
# 
# plot(globiom.rep, type = "l", ylab = " ",  xlab = " ",
#      col = "#72CC98", axes = F, ylim=c(0,500),  lwd = 2)
# 
# # matplot(globiom[4:74, 1:96], type = "l", ylab = " ",  xlab = " ",
# #         col = "#72CC98", axes = F, ylim=c(0,20000)) #plot
# 
# axis(1, at = seq(0,71,10), labels = seq(1990,2060,10), cex.axis = 0.7)
# axis(2, at = seq(0,500,100), labels = seq(0,500,100), cex.axis = 0.7)
# 
# lines(as.numeric(histpric["Whea",]), col = "black", lty = 3, lwd = 2 )
# 
# wt_crop = worktog[worktog$ParameterName == "Whea",]
# wt_crop_pr = as.numeric(wt_crop[wt_crop$ParameterType == "XPRP", 5:75])
# 
# nf_crop = nat_first[nat_first$ParameterName == "Whea",]
# nf_crop_pr = as.numeric(nf_crop[nf_crop$ParameterType == "XPRP", 5:75])
# 
# lines(nf_crop_pr, col = "#C00000", lty = 1, lwd = 2 )
# lines(wt_crop_pr, col = "#6666FF", lty = 1, lwd = 2 )
# 
# #GTEM
# gtem.Wheat.price = as.numeric(gtem.prices[gtem.prices$item == "6 Wheat", 32:dim(gtem.prices)[2]])
# 
# gtem.Wheat.price  = 149.22 + gtem.Wheat.price * 149.22/100 # 149.22 is the Wheat price in 2014 from FAOSTAT
# 
# lines(gtem.Wheat.price, col = "orange", lty = 1, lwd = 2 )
# 
# # legend("bottomright", legend = c("Historical", "GLOBIOM"), col= c("#2DCBD3","#72CC98" ), 
# #        lty = c(2,1), bty = "n") # optional legend
# title(main = "d) Wheat price", ylab = "USD 2000 per ton",  xlab = " ", cex=0.9)
# 
# #------------------------------------------------------------------------
# # Rice price
# 
# globiom = t(as.matrix(prices[prices$lu == "Rice",]))
# colnames(globiom) = globiom["ghg",]
# 
# # Get mean value
# globiom.rep = (as.data.frame(globiom[4:74, 1:96]))
# # Convert character data to numeric
# globiom.rep[] <- lapply(globiom.rep, function(x) as.numeric(as.character(x)))
# # globiom.rep = rowMeans(globiom.rep, na.rm = T)
# globiom.rep = rowMedians(as.matrix(globiom.rep))
# 
# par(mar = c(4,4,1,0))
# # matplot(globiom[4:74, 1:96], type = "l", ylab = " ",  xlab = " ",
# #         col = "#72CC98", axes = F, ylim=c(0,3000)) #plot
# # plot(globiom[4:74, "GHG020_BIO0N"], type = "l", ylab = " ",  xlab = " ",
# #              col = "#72CC98", axes = F, ylim=c(0,3000))
# 
# plot(globiom.rep, type = "l", ylab = " ",  xlab = " ",
#      col = "#72CC98", axes = F, ylim=c(0,500),  lwd = 2)
# # matplot(globiom[4:74, 1:96], type = "l", ylab = " ",  xlab = " ",
# #         col = "#72CC98", axes = F, ylim=c(0,20000)) #plot
# axis(1, at = seq(0,71,10), labels = seq(1990,2060,10), cex.axis = 0.7)
# axis(2, at = seq(0,500,100), labels = seq(0,500,100), cex.axis = 0.7)
# 
# lines(as.numeric(histpric["Rice",]), col = "black", lty = 3, lwd = 2 )
# 
# wt_crop = worktog[worktog$ParameterName == "Rice",]
# wt_crop_pr = as.numeric(wt_crop[wt_crop$ParameterType == "XPRP", 5:75])
# 
# nf_crop = nat_first[nat_first$ParameterName == "Rice",]
# nf_crop_pr = as.numeric(nf_crop[nf_crop$ParameterType == "XPRP", 5:75])
# 
# lines(nf_crop_pr, col = "#C00000", lty = 1, lwd = 2 )
# lines(wt_crop_pr, col = "#6666FF", lty = 1, lwd = 2 )
# 
# #GTEM
# gtem.Rice.price = as.numeric(gtem.prices[gtem.prices$item == "5 Rice", 32:dim(gtem.prices)[2]])
# 
# gtem.Rice.price  = 160.59 + gtem.Rice.price * 160.59/100 # 160.59 is the Rice price in 2014 from FAOSTAT
# 
# lines(gtem.Rice.price, col = "orange", lty = 1, lwd = 2 )
# 
# title(main = "e) Rice price", ylab = "", cex=0.9)
# 
# #------------------------------------------------------------------------
# # # Sugar crops  price
# 
# globiom = t(as.matrix(prices[prices$lu == "SugC",]))
# colnames(globiom) = globiom["ghg",]
# 
# # Get mean value
# globiom.rep = (as.data.frame(globiom[4:74, 1:96]))
# # Convert character data to numeric
# globiom.rep[] <- lapply(globiom.rep, function(x) as.numeric(as.character(x)))
# # globiom.rep = rowMeans(globiom.rep, na.rm = T)
# globiom.rep = rowMedians(as.matrix(globiom.rep))
# 
# par(mar = c(4,4,1,0))
# # matplot(globiom[4:74, 1:96], type = "l", ylab = " ",  xlab = " ",
# #         col = "#72CC98", axes = F, ylim=c(0,3000)) #plot
# # plot(globiom[4:74, "GHG020_BIO0N"], type = "l", ylab = " ",  xlab = " ",
# #              col = "#72CC98", axes = F, ylim=c(0,3000))
# 
# plot(globiom.rep, type = "l", ylab = " ",  xlab = " ",
#      col = "#72CC98", axes = F, ylim=c(0,50),  lwd = 2)
# # matplot(globiom[4:74, 1:96], type = "l", ylab = " ",  xlab = " ",
# #         col = "#72CC98", axes = F, ylim=c(0,20000)) #plot
# axis(1, at = seq(0,71,10), labels = seq(1990,2060,10), cex.axis = 0.7)
# axis(2, at = seq(0,50,10), labels = seq(0,50,10), cex.axis = 0.7)
# 
# lines(as.numeric(histpric["Sugar cane",]), col = "black", lty = 3, lwd = 2 )
# 
# wt_crop = worktog[worktog$ParameterName == "SugC",]
# wt_crop_pr = as.numeric(wt_crop[wt_crop$ParameterType == "XPRP", 5:75])
# 
# nf_crop = nat_first[nat_first$ParameterName == "SugC",]
# nf_crop_pr = as.numeric(nf_crop[nf_crop$ParameterType == "XPRP", 5:75])
# 
# lines(nf_crop_pr, col = "#C00000", lty = 1, lwd = 2 )
# lines(wt_crop_pr, col = "#6666FF", lty = 1, lwd = 2 )
# 
# #GTEM
# gtem.sc.price = as.numeric(gtem.prices[gtem.prices$item == "10 Cane_beet", 32:dim(gtem.prices)[2]])
# 
# gtem.sc.price  = 18.91 + gtem.sc.price * 18.91/100 # 160.59 is the Rice price in 2014 from FAOSTAT
# 
# lines(gtem.sc.price, col = "orange", lty = 1, lwd = 2 )
# title(main = "f) Sugar crops price", ylab = " ",  xlab = " ",cex=0.9)
# 
# 
# plot(NULL ,xaxt='n',yaxt='n',bty='n',ylab='',xlab='', xlim=0:1, ylim=0:1)
# 
# 
# plot(NULL ,xaxt='n',yaxt='n',bty='n',ylab='',xlab='', xlim=0:1, ylim=0:1)
# 
# legend("topright", legend = c("Historical (FAOSTAT)", "GLOBIOM - SSP2", "Working Together (ANO 2019)", "Nations First (ANO 2019)", "GTEM-Food"), 
#        col= c("black","#72CC98", "#6666FF", "#C00000", "orange"), 
#        lty = c(2,1,1,1,1), lwd = c(2,2,2,2,2), bty = "n", cex = 1) # optional legend
# 
# mtext("Projections", at=0.1, cex=0.75)
# 
# dev.off()





