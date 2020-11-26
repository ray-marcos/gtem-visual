


# Required packages
x = c("forecast", "data.table", "lubridate", "ggplot2", "ggthemes",  "gridExtra", "cowplot", "dplyr",
      "matrixStats")
lapply(x, library, character.only = TRUE);


# Set the working directory
setwd("C:/Users/mar77v/CSIRO/ABARES CSIRO Modeling - Visualization/V July_27_2017")



# GLOBIOM data
# The script can also run SSP1 
# ssp_lu = read.csv("SSP1_LU_Template.csv", stringsAsFactors = F)
# Aggregated data
# ssp_lu = read.csv("SSP2_LU_Template.csv", stringsAsFactors = F)
ssp_lu = read.csv("globiom_output_SSP2_03072017_v2.csv", stringsAsFactors = F)
regs = unique(ssp_lu$Region)
lu = unique(ssp_lu$lu)

region = "World"
# Replace NA with zeros
ssp_lu[is.na(ssp_lu)] = 0
# production

pn = ssp_lu %>%
        filter(Region  == region) %>%
        filter(unit == "1000 t" ) %>%
        group_by(lu) %>%
        summarise_if(is.numeric, sum, na.rm = TRUE) %>%
        arrange(desc(lu))

pn[pn == 0] = NA

# Prices
#
# THIS PRICES NEED TO BE UPDATED TO THE SAME BASE AS GTEM
# CHeck that projections after 2040 are included. 
prices = ssp_lu %>%
        filter(Region  == region) %>%
        filter(variable == "XPRP" ) %>%
        filter(unit == "USD 2000 per ton") %>%
        group_by(lu) %>%
        summarise_if(is.numeric, sum, na.rm = TRUE) %>%
        arrange(desc(lu))

prices[prices == 0] = NA



# ANO data

nat_first = read.csv("C:/Users/mar77v/Dropbox/Australia/GLOBIOM/latest_results/FourDegrees/IIASA_GLOBIOM_emulator_interpolated_output 1990-2100.csv")
# nat_first = nat_first[nat_first$Regions == "AustraliaReg",]

worktog = read.csv("C:/Users/mar77v/Dropbox/Australia/GLOBIOM/latest_results/TwoDegrees/IIASA_GLOBIOM_emulator_interpolated_output 1990-2100.csv")
# worktog = worktog[worktog$Regions == "AustraliaReg",]

# Aggregate data to report global trends
nat_first_pn <- nat_first %>%
        filter(ParameterType == "PROD") %>%
        group_by(ParameterName) %>%
        summarise_if(is.numeric, sum, na.rm = TRUE) %>%
        arrange(desc(ParameterName))

nat_first_pric <- nat_first %>%
        filter(ParameterType == "XPRP") %>%
        group_by(ParameterName) %>%
        summarise_if(is.numeric, mean, na.rm = TRUE) %>%
        arrange(desc(ParameterName))

worktog_pn <- worktog %>%
        filter(ParameterType == "PROD") %>%
        group_by(ParameterName) %>%
        summarise_if(is.numeric, sum, na.rm = TRUE) %>%
        arrange(desc(ParameterName))

worktog_pric <- worktog %>%
        filter(ParameterType == "XPRP") %>%
        group_by(ParameterName) %>%
        summarise_if(is.numeric, mean, na.rm = TRUE) %>%
        arrange(desc(ParameterName))


# Historical data

# FAOSTAT producers price data
histpric = read.csv("Prices_E_All_Data_NOFLAG.csv")
# Select annual PP data in USD and estimate global mean values 
histpric<- histpric %>%
        filter(Element == "Producer Price (USD/tonne)") %>%
        filter(Months == "Annual value") %>%
        group_by(Item) %>%
        summarise_if(is.numeric, mean, na.rm = TRUE) %>%
        arrange(desc(Item))

histpric[is.na(histpric)] = 0

# Transform data to real prices

def = read.csv("US GDP deflator.csv", stringsAsFactors = F)

t1 = c("Y1991","Y1992", "Y1993","Y1994","Y1995","Y1996", 
       "Y1997","Y1998","Y1999","Y2000","Y2001","Y2002","Y2003","Y2004", "Y2005","Y2006","Y2007",
       "Y2008", "Y2009","Y2010","Y2011","Y2012","Y2013","Y2014","Y2015","Y2016", "Y2017", "Y2018" )

t2 = c(1991:2018)

histpric.def = histpric

for(j in 1:dim(histpric)[1]){
        
        histpric.def[j, 6:33] =  histpric[j, 6:33] / def[def$Year >= 1991, "GDP_def_2005"]

}


# FAOSTAT production
histcrop = read.csv("Production_Crops_E_All_Data_NOFLAG.csv")

histcrop<- histcrop %>%
        filter(Element == "Production") %>%
        group_by(Item) %>%
        summarise_if(is.numeric, mean, na.rm = TRUE) %>%
        arrange(desc(Item))


histliv = read.csv("Production_LivestockPrimary_E_All_Data_NOFLAG.csv")

histliv<- histliv %>%
        filter(Element == "Production") %>%
        group_by(Item) %>%
        summarise_if(is.numeric, mean, na.rm = TRUE) %>%
        arrange(desc(Item))


# prices = read.csv("pricesSSP2.csv", stringsAsFactors = F)
# pn = read.csv("productionSSP2.csv", stringsAsFactors = F)
# area = read.csv("surfaceSSP2.csv", stringsAsFactors = F)
# 
# 
# # Aggregate price data
# pric_ag<- prices %>%
#         group_by(lu) %>%
#         summarise_if(is.numeric, mean, na.rm = TRUE) %>%
#         arrange(desc(lu))
# 
# 
# histpric = read.csv("historical prices.csv", stringsAsFactors = F)
# rownames(histpric) = unlist(histpric$crop); histpric$crop = NULL
# histpn = read.csv("historical_production.csv", stringsAsFactors = F)
# rownames(histpn) = unlist(histpn$crop); histpn$crop = NULL
# histarea = read.csv("historical_surface.csv", stringsAsFactors = F)
# rownames(histarea) = unlist(histarea$crop); histarea$crop = NULL


# GTEM

gtem.prices = read.csv("prices_adj_AU_annual.csv")
gtem.output = read.csv("global.output.csv")




#----------------------------------------------------------------------------------------------------
# Meat and crop projections
#----------------------------------------------------------------------------------------------------


#------------------------------------------------------------------------
# Meat production
#------------------------------------------------------------------------

tiff("meat & dairy GTEM global.tiff", compression = "lzw", res = 500, width = 160, height = 160, units = "mm")

par(mfrow=c(3,3), cex = 0.6)

#------------------------------------------------------------------------
# Beef production

fmat = t(as.matrix(pn[pn$lu == "BVMEAT",]))
# colnames(fmat) = fmat["ghg",]
fmat.rep = as.numeric(as.character(fmat))

# # Get mean value
# fmat.rep = (as.data.frame(fmat[4:74, 1:96]))
# # Convert character data to numeric
# fmat.rep[] <- lapply(fmat.rep, function(x) as.numeric(as.character(x)))
# # fmat.rep = rowMeans(fmat.rep, na.rm = T)
# fmat.rep = rowMedians(as.matrix(fmat.rep))

par(mar = c(4,4,1,0))
# matplot(fmat[4:74, 1:96], type = "l", ylab = " ",  xlab = " ",
#         col = "#72CC98", axes = F, ylim=c(0,3000)) #plot
# plot(fmat[4:74, "GHG020_BIO0N"], type = "l", ylab = " ",  xlab = " ",
#              col = "#72CC98", axes = F, ylim=c(0,3000))

plot(fmat.rep, type = "l", ylab = " ",  xlab = " ",
     col = "#72CC98", axes = F,  lwd = 2)

axis(1, at = seq(0,71,10), labels = seq(1990,2060,10), cex.axis = 0.7)
axis(2, at = seq(0,3000,500), labels = seq(0,3000,500), cex.axis = 0.7)

lines(as.numeric(histpn["BVMEAT",]), col = "black", lty = 3, lwd = 2)

#WT
wt_meat = worktog[worktog$ParameterName == "BVMEAT",]
wt_meat_pn = as.numeric(wt_meat[wt_meat$ParameterType == "PROD", 5:75])
lines(wt_meat_pn, col = "#6666FF", lty = 1, lwd = 2 )
# NF
nf_meat = nat_first[nat_first$ParameterName == "BVMEAT",]
nf_meat_pn = as.numeric(nf_meat[nf_meat$ParameterType == "PROD", 5:75])

lines(nf_meat_pn, col = "#C00000", lty = 1, lwd = 2 )

# GTEM
gtem.beef = as.numeric(gtem.output[gtem.output$item == "17 Meat_cattle", 32:dim(gtem.output)[2]])
lines(gtem.beef, col = "orange", lty = 1, lwd = 2 )

# legend("bottomright", legend = c("Historical", "GLOBIOM"), col= c("#2DCBD3","#72CC98" ), 
#        lty = c(2,1), bty = "n") # optional legend
title(main = "a) Beef output", ylab = "1000 tons",  xlab = " ", cex=0.9 )

#------------------------------------------------------------------------
# Meat_pork production

fmat = t(as.matrix(pn[pn$lu == "PGMEAT",]))
colnames(fmat) = fmat["ghg",]

# Get mean value
fmat.rep = (as.data.frame(fmat[4:74, 1:96]))
# Convert character data to numeric
fmat.rep[] <- lapply(fmat.rep, function(x) as.numeric(as.character(x)))
# fmat.rep = rowMeans(fmat.rep, na.rm = T)
fmat.rep = rowMedians(as.matrix(fmat.rep))


par(mar = c(4,4,1,0))
# matplot(fmat[4:74, 1:96], type = "l", ylab = " ",  xlab = " ",
#         col = "#72CC98", axes = F, ylim=c(0,3000)) #plot
# plot(fmat[4:74, "GHG020_BIO0N"], type = "l", ylab = " ",  xlab = " ",
#              col = "#72CC98", axes = F, ylim=c(0,3000))

plot(fmat.rep, type = "l", ylab = " ",   xlab = " ",
     col = "#72CC98", axes = F, ylim=c(0,1500),  lwd = 2)

axis(1, at = seq(0,71,10), labels = seq(1990,2060,10), cex.axis = 0.7)
axis(2, at = seq(0,1500,500), labels = seq(0,1500,500), cex.axis = 0.7)

lines(as.numeric(histpn["PGMEAT",]), col = "black", lty = 3, lwd = 2 )

#WT
wt_meat = worktog[worktog$ParameterName == "PGMEAT",]
wt_meat_pn = as.numeric(wt_meat[wt_meat$ParameterType == "PROD", 5:75])
lines(wt_meat_pn, col = "#6666FF", lty = 1, lwd = 2 )
# NF
nf_meat = nat_first[nat_first$ParameterName == "PGMEAT",]
nf_meat_pn = as.numeric(nf_meat[nf_meat$ParameterType == "PROD", 5:75])

lines(nf_meat_pn, col = "#C00000", lty = 1, lwd = 2 )

# GTEM
gtem.pork = as.numeric(gtem.output[gtem.output$item == "18 Meat_pork", 32:dim(gtem.output)[2]])
lines(gtem.pork, col = "orange", lty = 1, lwd = 2 )

# legend("bottomright", legend = c("Historical", "GLOBIOM"), col= c("#2DCBD3","#72CC98" ), 
#        lty = c(2,1), bty = "n") # optional legend
title(main = "b) Pork output", ylab = " ",  xlab = " ", cex=0.9 )


#------------------------------------------------------------------------
# Dairy production

fmat = t(as.matrix(pn[pn$lu == "ALMILK",]))
colnames(fmat) = fmat["ghg",]

# Get mean value
fmat.rep = (as.data.frame(fmat[4:74, 1:96]))
# Convert character data to numeric
fmat.rep[] <- lapply(fmat.rep, function(x) as.numeric(as.character(x)))
# fmat.rep = rowMeans(fmat.rep, na.rm = T)
fmat.rep = rowMedians(as.matrix(fmat.rep))

par(mar = c(4,4,1,0))
# matplot(fmat[4:74, 1:96], type = "l", ylab = " ",  xlab = " ",
#         col = "#72CC98", axes = F, ylim=c(0,3000)) #plot
# plot(fmat[4:74, "GHG020_BIO0N"], type = "l", ylab = " ",  xlab = " ",
#              col = "#72CC98", axes = F, ylim=c(0,3000))

plot(fmat.rep, type = "l", ylab = " ",  xlab = " ",
     col = "#72CC98", axes = F, ylim=c(0,20000),  lwd = 2)

axis(1, at = seq(0,71,10), labels = seq(1990,2060,10), cex.axis = 0.7)
axis(2, at = seq(0,20000,5000), labels = seq(0,20000,5000), cex.axis = 0.7)

lines(as.numeric(histpn["ALMILK",]), col = "black", lty = 3, lwd = 2 )

#WT
wt_meat = worktog[worktog$ParameterName == "ALMILK",]
wt_meat_pn = as.numeric(wt_meat[wt_meat$ParameterType == "PROD", 5:75])
lines(wt_meat_pn, col = "#6666FF", lty = 1, lwd = 2 )
# NF
nf_meat = nat_first[nat_first$ParameterName == "ALMILK",]
nf_meat_pn = as.numeric(nf_meat[nf_meat$ParameterType == "PROD", 5:75])

lines(nf_meat_pn, col = "#C00000", lty = 1, lwd = 2 )

# GTEM
gtem.milk = as.numeric(gtem.output[gtem.output$item == "24 Dairy_milk", 32:dim(gtem.output)[2]])
lines(gtem.milk, col = "orange", lty = 1, lwd = 2 )

# legend("bottomright", legend = c("Historical", "GLOBIOM"), col= c("#2DCBD3","#72CC98" ), 
#        lty = c(2,1), bty = "n") # optional legend
title(main = "c) Dairy output", ylab = " ",  xlab = " ", cex=0.9 )



#------------------------------------------------------------------------
# Beef price

fmat = t(as.matrix(prices[prices$lu == "BVMEAT",]))

colnames(fmat) = fmat["ghg",]

# Get mean value
fmat.rep = (as.data.frame(fmat[4:74, 1:96]))
# Convert character data to numeric
fmat.rep[] <- lapply(fmat.rep, function(x) as.numeric(as.character(x)))
# fmat.rep = rowMeans(fmat.rep, na.rm = T)
fmat.rep = rowMedians(as.matrix(fmat.rep))


par(mar = c(4,4,1,0))
# matplot(fmat[4:74, 1:96], type = "l", ylab = " ",  xlab = " ",
#         col = "#72CC98", axes = F, ylim=c(0,3000)) #plot
# plot(fmat[4:74, "GHG020_BIO0N"], type = "l", ylab = " ",  xlab = " ",
#              col = "#72CC98", axes = F, ylim=c(0,3000))

plot(fmat.rep, type = "l", ylab = " ",  xlab = " ",
     col = "#72CC98", axes = F, ylim=c(0,3000),  lwd = 2)

# matplot(fmat[4:74, 1:96], type = "l", ylab = " ",  xlab = " ",
#         col = "#72CC98", axes = F, ylim=c(0,20000)) #plot

axis(1, at = seq(0,71,10), labels = seq(1990,2060,10), cex.axis = 0.7)
axis(2, at = seq(0,3000,500), labels = seq(0,3000,500), cex.axis = 0.7)

lines(as.numeric(histpric["BVMEAT",]), col = "black", lty = 3, lwd = 2 )

wt_meat = worktog[worktog$ParameterName == "BVMEAT",]
wt_meat_pr = as.numeric(wt_meat[wt_meat$ParameterType == "XPRP", 5:75])

nf_meat = nat_first[nat_first$ParameterName == "BVMEAT",]
nf_meat_pr = as.numeric(nf_meat[nf_meat$ParameterType == "XPRP", 5:75])

lines(nf_meat_pr, col = "#C00000", lty = 1, lwd = 2 )
lines(wt_meat_pr, col = "#6666FF", lty = 1, lwd = 2 )

#GTEM
gtem.beef.price = as.numeric(gtem.prices[gtem.prices$item == "17 Meat_cattle", 32:dim(gtem.prices)[2]])

gtem.beef.price  = 1452.81 + gtem.beef.price * 1452.81/100 # 1452.81 is the beef price in 2014 from FAOSTAT

lines(gtem.beef.price, col = "orange", lty = 1, lwd = 2 )

# legend("bottomright", legend = c("Historical", "GLOBIOM"), col= c("#2DCBD3","#72CC98" ), 
#        lty = c(2,1), bty = "n") # optional legend
title(main = "d) Beef prices", ylab = "USD 2000 per ton",  xlab = " ", cex=0.9)

#------------------------------------------------------------------------
# Pork price

fmat = t(as.matrix(prices[prices$lu == "PGMEAT",]))
colnames(fmat) = fmat["ghg",]

# Get mean value
fmat.rep = (as.data.frame(fmat[4:74, 1:96]))
# Convert character data to numeric
fmat.rep[] <- lapply(fmat.rep, function(x) as.numeric(as.character(x)))
# fmat.rep = rowMeans(fmat.rep, na.rm = T)
fmat.rep = rowMedians(as.matrix(fmat.rep))

par(mar = c(4,4,1,0))
# matplot(fmat[4:74, 1:96], type = "l", ylab = " ",  xlab = " ",
#         col = "#72CC98", axes = F, ylim=c(0,3000)) #plot
# plot(fmat[4:74, "GHG020_BIO0N"], type = "l", ylab = " ",  xlab = " ",
#              col = "#72CC98", axes = F, ylim=c(0,3000))

plot(fmat.rep, type = "l", ylab = " ",  xlab = " ",
     col = "#72CC98", axes = F, ylim=c(0,3000),  lwd = 2)
# matplot(fmat[4:74, 1:96], type = "l", ylab = " ",  xlab = " ",
#         col = "#72CC98", axes = F, ylim=c(0,20000)) #plot
axis(1, at = seq(0,71,10), labels = seq(1990,2060,10), cex.axis = 0.7)
axis(2, at = seq(0,3000,500), labels = seq(0,3000,500), cex.axis = 0.7)

lines(as.numeric(histpric["PGMEAT",]), col = "black", lty = 3, lwd = 2 )

wt_meat = worktog[worktog$ParameterName == "PGMEAT",]
wt_meat_pr = as.numeric(wt_meat[wt_meat$ParameterType == "XPRP", 5:75])

nf_meat = nat_first[nat_first$ParameterName == "PGMEAT",]
nf_meat_pr = as.numeric(nf_meat[nf_meat$ParameterType == "XPRP", 5:75])

lines(nf_meat_pr, col = "#C00000", lty = 1, lwd = 2 )
lines(wt_meat_pr, col = "#6666FF", lty = 1, lwd = 2 )

#GTEM
gtem.pork.price = as.numeric(gtem.prices[gtem.prices$item == "18 Meat_pork", 32:dim(gtem.prices)[2]])

gtem.pork.price  = 1242.13 + gtem.pork.price * 1242.13/100 # 1242.13 is the pork price in 2014 from FAOSTAT

lines(gtem.pork.price, col = "orange", lty = 1, lwd = 2 )

title(main = "e) Pork prices", ylab = "", cex=0.9)

#------------------------------------------------------------------------
# # Dairy  price

fmat = t(as.matrix(prices[prices$lu == "ALMILK",]))
colnames(fmat) = fmat["ghg",]

# Get mean value
fmat.rep = (as.data.frame(fmat[4:74, 1:96]))
# Convert character data to numeric
fmat.rep[] <- lapply(fmat.rep, function(x) as.numeric(as.character(x)))
# fmat.rep = rowMeans(fmat.rep, na.rm = T)
fmat.rep = rowMedians(as.matrix(fmat.rep))

par(mar = c(4,4,1,0))
# matplot(fmat[4:74, 1:96], type = "l", ylab = " ",  xlab = " ",
#         col = "#72CC98", axes = F, ylim=c(0,3000)) #plot
# plot(fmat[4:74, "GHG020_BIO0N"], type = "l", ylab = " ",  xlab = " ",
#              col = "#72CC98", axes = F, ylim=c(0,3000))

plot(fmat.rep, type = "l", ylab = " ",  xlab = " ",
     col = "#72CC98", axes = F, ylim=c(0,500),  lwd = 2)
# matplot(fmat[4:74, 1:96], type = "l", ylab = " ",  xlab = " ",
#         col = "#72CC98", axes = F, ylim=c(0,20000)) #plot
axis(1, at = seq(0,71,10), labels = seq(1990,2060,10), cex.axis = 0.7)
axis(2, at = seq(0,500,100), labels = seq(0,500,100), cex.axis = 0.7)

lines(as.numeric(histpric["ALMILK",]), col = "black", lty = 3, lwd = 2 )

wt_meat = worktog[worktog$ParameterName == "ALMILK",]
wt_meat_pr = as.numeric(wt_meat[wt_meat$ParameterType == "XPRP", 5:75])

nf_meat = nat_first[nat_first$ParameterName == "ALMILK",]
nf_meat_pr = as.numeric(nf_meat[nf_meat$ParameterType == "XPRP", 5:75])

lines(nf_meat_pr, col = "#C00000", lty = 1, lwd = 2 )
lines(wt_meat_pr, col = "#6666FF", lty = 1, lwd = 2 )

#GTEM
gtem.milk.price = as.numeric(gtem.prices[gtem.prices$item == "24 Dairy_milk", 32:dim(gtem.prices)[2]])

gtem.milk.price  = 177.57 + gtem.milk.price * 177.57/100 # 177.57 is the milk price in 2014 from FAOSTAT

lines(gtem.milk.price, col = "orange", lty = 1, lwd = 2 )
title(main = "f) Milk prices", ylab = " ",  xlab = " ",cex=0.9)


plot(NULL ,xaxt='n',yaxt='n',bty='n',ylab='',xlab='', xlim=0:1, ylim=0:1)


plot(NULL ,xaxt='n',yaxt='n',bty='n',ylab='',xlab='', xlim=0:1, ylim=0:1)

legend("topright", legend = c("Historical (FAOSTAT)", "GLOBIOM - SSP2", "Working Together (ANO 2019)", "Nations First (ANO 2019)", "GTEM-Food"), 
       col= c("black","#72CC98", "#6666FF", "#C00000", "orange"), 
       lty = c(2,1,1,1,1), lwd = c(2,2,2,2,2), bty = "n", cex = 1) # optional legend

mtext("Projections", at=0.1, cex=0.75)

dev.off()











#---------------------------------------------------------------------------------------
# Crops
#---------------------------------------------------------------------------------------

tiff("crops GTEM.tiff", compression = "lzw", res = 500, width = 160, height = 160, units = "mm")

par(mfrow=c(3,3), cex = 0.6)

#------------------------------------------------------------------------
# Wheat production

fmat = t(as.matrix(pn[pn$lu == "Whea",]))
colnames(fmat) = fmat["ghg",]

# Get mean value
fmat.rep = (as.data.frame(fmat[4:74, 1:96]))
# Convert character data to numeric
fmat.rep[] <- lapply(fmat.rep, function(x) as.numeric(as.character(x)))
# fmat.rep = rowMeans(fmat.rep, na.rm = T)
fmat.rep = rowMedians(as.matrix(fmat.rep))

par(mar = c(4,4,1,0))
# matplot(fmat[4:74, 1:96], type = "l", ylab = " ",  xlab = " ",
#         col = "#72CC98", axes = F, ylim=c(0,3000)) #plot
# plot(fmat[4:74, "GHG020_BIO0N"], type = "l", ylab = " ",  xlab = " ",
#              col = "#72CC98", axes = F, ylim=c(0,3000))

plot(fmat.rep, type = "l", ylab = " ",  xlab = " ",
     col = "#72CC98", axes = F, ylim=c(0,60000),  lwd = 2)

axis(1, at = seq(0,71,10), labels = seq(1990,2060,10), cex.axis = 0.7)
axis(2, at = seq(0,60000,20000), labels = seq(0,60000,20000), cex.axis = 0.7)

lines(as.numeric(histpn["Whea",]), col = "black", lty = 3, lwd = 2)

#WT
wt_crop = worktog[worktog$ParameterName == "Whea",]
wt_crop_pn = as.numeric(wt_crop[wt_crop$ParameterType == "PROD", 5:75])
lines(wt_crop_pn, col = "#6666FF", lty = 1, lwd = 2 )
# NF
nf_crop = nat_first[nat_first$ParameterName == "Whea",]
nf_crop_pn = as.numeric(nf_crop[nf_crop$ParameterType == "PROD", 5:75])

lines(nf_crop_pn, col = "#C00000", lty = 1, lwd = 2 )

# GTEM
gtem.C = as.numeric(gtem.output[gtem.output$item == "6 Wheat", 32:dim(gtem.output)[2]])
lines(gtem.C, col = "orange", lty = 1, lwd = 2 )

# legend("bottomright", legend = c("Historical", "GLOBIOM"), col= c("#2DCBD3","#72CC98" ), 
#        lty = c(2,1), bty = "n") # optional legend
title(main = "a) Wheat output", ylab = "1000 tons",  xlab = " ", cex=0.9 )

#------------------------------------------------------------------------
# Rice production

fmat = t(as.matrix(pn[pn$lu == "Rice",]))
colnames(fmat) = fmat["ghg",]

# Get mean value
fmat.rep = (as.data.frame(fmat[4:74, 1:96]))
# Convert character data to numeric
fmat.rep[] <- lapply(fmat.rep, function(x) as.numeric(as.character(x)))
# fmat.rep = rowMeans(fmat.rep, na.rm = T)
fmat.rep = rowMedians(as.matrix(fmat.rep))


par(mar = c(4,4,1,0))
# matplot(fmat[4:74, 1:96], type = "l", ylab = " ",  xlab = " ",
#         col = "#72CC98", axes = F, ylim=c(0,3000)) #plot
# plot(fmat[4:74, "GHG020_BIO0N"], type = "l", ylab = " ",  xlab = " ",
#              col = "#72CC98", axes = F, ylim=c(0,3000))

plot(fmat.rep, type = "l", ylab = " ",   xlab = " ",
     col = "#72CC98", axes = F, ylim=c(0, 10000),  lwd = 2)

axis(1, at = seq(0,71,10), labels = seq(1990,2060,10), cex.axis = 0.7)
axis(2, at = seq(0,10000,2500), labels = seq(0,10000,2500), cex.axis = 0.7)

lines(as.numeric(histpn["Rice",]), col = "black", lty = 3, lwd = 2 )

#WT
wt_crop = worktog[worktog$ParameterName == "Rice",]
wt_crop_pn = as.numeric(wt_crop[wt_crop$ParameterType == "PROD", 5:75])
lines(wt_crop_pn, col = "#6666FF", lty = 1, lwd = 2 )
# NF
nf_crop = nat_first[nat_first$ParameterName == "Rice",]
nf_crop_pn = as.numeric(nf_crop[nf_crop$ParameterType == "PROD", 5:75])

lines(nf_crop_pn, col = "#C00000", lty = 1, lwd = 2 )

# GTEM
gtem.Rice = as.numeric(gtem.output[gtem.output$item == "5 Rice", 32:dim(gtem.output)[2]])
lines(gtem.Rice, col = "orange", lty = 1, lwd = 2 )

# legend("bottomright", legend = c("Historical", "GLOBIOM"), col= c("#2DCBD3","#72CC98" ), 
#        lty = c(2,1), bty = "n") # optional legend
title(main = "b) Rice output", ylab = " ",  xlab = " ", cex=0.9 )


#------------------------------------------------------------------------
# Sugar crops production

fmat = t(as.matrix(pn[pn$lu == "SugC",]))
colnames(fmat) = fmat["ghg",]

# Get mean value
fmat.rep = (as.data.frame(fmat[4:74, 1:96]))
# Convert character data to numeric
fmat.rep[] <- lapply(fmat.rep, function(x) as.numeric(as.character(x)))
# fmat.rep = rowMeans(fmat.rep, na.rm = T)
fmat.rep = rowMedians(as.matrix(fmat.rep))

par(mar = c(4,4,1,0))
# matplot(fmat[4:74, 1:96], type = "l", ylab = " ",  xlab = " ",
#         col = "#72CC98", axes = F, ylim=c(0,3000)) #plot
# plot(fmat[4:74, "GHG020_BIO0N"], type = "l", ylab = " ",  xlab = " ",
#              col = "#72CC98", axes = F, ylim=c(0,3000))

plot(fmat.rep, type = "l", ylab = " ",  xlab = " ",
     col = "#72CC98", axes = F, ylim=c(0,80000),  lwd = 2)

axis(1, at = seq(0,71,10), labels = seq(1990,2060,10), cex.axis = 0.7)
axis(2, at = seq(0,80000,20000), labels = seq(0,80000,20000), cex.axis = 0.7)

lines(as.numeric(histpn["Sugar cane",]), col = "black", lty = 3, lwd = 2 )

#WT
wt_crop = worktog[worktog$ParameterName == "SugC",]
wt_crop_pn = as.numeric(wt_crop[wt_crop$ParameterType == "PROD", 5:75])
lines(wt_crop_pn, col = "#6666FF", lty = 1, lwd = 2 )
# NF
nf_crop = nat_first[nat_first$ParameterName == "SugC",]
nf_crop_pn = as.numeric(nf_crop[nf_crop$ParameterType == "PROD", 5:75])

lines(nf_crop_pn, col = "#C00000", lty = 1, lwd = 2 )

# GTEM
gtem.sugar = as.numeric(gtem.output[gtem.output$item == "10 Cane_beet", 32:dim(gtem.output)[2]])
lines(gtem.sugar, col = "orange", lty = 1, lwd = 2 )

# legend("bottomright", legend = c("Historical", "GLOBIOM"), col= c("#2DCBD3","#72CC98" ), 
#        lty = c(2,1), bty = "n") # optional legend
title(main = "c) Sugar crops output", ylab = " ",  xlab = " ", cex=0.9 )



#------------------------------------------------------------------------
# Wheat price

fmat = t(as.matrix(prices[prices$lu == "Whea",]))

colnames(fmat) = fmat["ghg",]

# Get mean value
fmat.rep = (as.data.frame(fmat[4:74, 1:96]))
# Convert character data to numeric
fmat.rep[] <- lapply(fmat.rep, function(x) as.numeric(as.character(x)))
# fmat.rep = rowMeans(fmat.rep, na.rm = T)
fmat.rep = rowMedians(as.matrix(fmat.rep))


par(mar = c(4,4,1,0))
# matplot(fmat[4:74, 1:96], type = "l", ylab = " ",  xlab = " ",
#         col = "#72CC98", axes = F, ylim=c(0,3000)) #plot
# plot(fmat[4:74, "GHG020_BIO0N"], type = "l", ylab = " ",  xlab = " ",
#              col = "#72CC98", axes = F, ylim=c(0,3000))

plot(fmat.rep, type = "l", ylab = " ",  xlab = " ",
     col = "#72CC98", axes = F, ylim=c(0,500),  lwd = 2)

# matplot(fmat[4:74, 1:96], type = "l", ylab = " ",  xlab = " ",
#         col = "#72CC98", axes = F, ylim=c(0,20000)) #plot

axis(1, at = seq(0,71,10), labels = seq(1990,2060,10), cex.axis = 0.7)
axis(2, at = seq(0,500,100), labels = seq(0,500,100), cex.axis = 0.7)

lines(as.numeric(histpric["Whea",]), col = "black", lty = 3, lwd = 2 )

wt_crop = worktog[worktog$ParameterName == "Whea",]
wt_crop_pr = as.numeric(wt_crop[wt_crop$ParameterType == "XPRP", 5:75])

nf_crop = nat_first[nat_first$ParameterName == "Whea",]
nf_crop_pr = as.numeric(nf_crop[nf_crop$ParameterType == "XPRP", 5:75])

lines(nf_crop_pr, col = "#C00000", lty = 1, lwd = 2 )
lines(wt_crop_pr, col = "#6666FF", lty = 1, lwd = 2 )

#GTEM
gtem.Wheat.price = as.numeric(gtem.prices[gtem.prices$item == "6 Wheat", 32:dim(gtem.prices)[2]])

gtem.Wheat.price  = 149.22 + gtem.Wheat.price * 149.22/100 # 149.22 is the Wheat price in 2014 from FAOSTAT

lines(gtem.Wheat.price, col = "orange", lty = 1, lwd = 2 )

# legend("bottomright", legend = c("Historical", "GLOBIOM"), col= c("#2DCBD3","#72CC98" ), 
#        lty = c(2,1), bty = "n") # optional legend
title(main = "d) Wheat price", ylab = "USD 2000 per ton",  xlab = " ", cex=0.9)

#------------------------------------------------------------------------
# Rice price

fmat = t(as.matrix(prices[prices$lu == "Rice",]))
colnames(fmat) = fmat["ghg",]

# Get mean value
fmat.rep = (as.data.frame(fmat[4:74, 1:96]))
# Convert character data to numeric
fmat.rep[] <- lapply(fmat.rep, function(x) as.numeric(as.character(x)))
# fmat.rep = rowMeans(fmat.rep, na.rm = T)
fmat.rep = rowMedians(as.matrix(fmat.rep))

par(mar = c(4,4,1,0))
# matplot(fmat[4:74, 1:96], type = "l", ylab = " ",  xlab = " ",
#         col = "#72CC98", axes = F, ylim=c(0,3000)) #plot
# plot(fmat[4:74, "GHG020_BIO0N"], type = "l", ylab = " ",  xlab = " ",
#              col = "#72CC98", axes = F, ylim=c(0,3000))

plot(fmat.rep, type = "l", ylab = " ",  xlab = " ",
     col = "#72CC98", axes = F, ylim=c(0,500),  lwd = 2)
# matplot(fmat[4:74, 1:96], type = "l", ylab = " ",  xlab = " ",
#         col = "#72CC98", axes = F, ylim=c(0,20000)) #plot
axis(1, at = seq(0,71,10), labels = seq(1990,2060,10), cex.axis = 0.7)
axis(2, at = seq(0,500,100), labels = seq(0,500,100), cex.axis = 0.7)

lines(as.numeric(histpric["Rice",]), col = "black", lty = 3, lwd = 2 )

wt_crop = worktog[worktog$ParameterName == "Rice",]
wt_crop_pr = as.numeric(wt_crop[wt_crop$ParameterType == "XPRP", 5:75])

nf_crop = nat_first[nat_first$ParameterName == "Rice",]
nf_crop_pr = as.numeric(nf_crop[nf_crop$ParameterType == "XPRP", 5:75])

lines(nf_crop_pr, col = "#C00000", lty = 1, lwd = 2 )
lines(wt_crop_pr, col = "#6666FF", lty = 1, lwd = 2 )

#GTEM
gtem.Rice.price = as.numeric(gtem.prices[gtem.prices$item == "5 Rice", 32:dim(gtem.prices)[2]])

gtem.Rice.price  = 160.59 + gtem.Rice.price * 160.59/100 # 160.59 is the Rice price in 2014 from FAOSTAT

lines(gtem.Rice.price, col = "orange", lty = 1, lwd = 2 )

title(main = "e) Rice price", ylab = "", cex=0.9)

#------------------------------------------------------------------------
# # Sugar crops  price

fmat = t(as.matrix(prices[prices$lu == "SugC",]))
colnames(fmat) = fmat["ghg",]

# Get mean value
fmat.rep = (as.data.frame(fmat[4:74, 1:96]))
# Convert character data to numeric
fmat.rep[] <- lapply(fmat.rep, function(x) as.numeric(as.character(x)))
# fmat.rep = rowMeans(fmat.rep, na.rm = T)
fmat.rep = rowMedians(as.matrix(fmat.rep))

par(mar = c(4,4,1,0))
# matplot(fmat[4:74, 1:96], type = "l", ylab = " ",  xlab = " ",
#         col = "#72CC98", axes = F, ylim=c(0,3000)) #plot
# plot(fmat[4:74, "GHG020_BIO0N"], type = "l", ylab = " ",  xlab = " ",
#              col = "#72CC98", axes = F, ylim=c(0,3000))

plot(fmat.rep, type = "l", ylab = " ",  xlab = " ",
     col = "#72CC98", axes = F, ylim=c(0,50),  lwd = 2)
# matplot(fmat[4:74, 1:96], type = "l", ylab = " ",  xlab = " ",
#         col = "#72CC98", axes = F, ylim=c(0,20000)) #plot
axis(1, at = seq(0,71,10), labels = seq(1990,2060,10), cex.axis = 0.7)
axis(2, at = seq(0,50,10), labels = seq(0,50,10), cex.axis = 0.7)

lines(as.numeric(histpric["Sugar cane",]), col = "black", lty = 3, lwd = 2 )

wt_crop = worktog[worktog$ParameterName == "SugC",]
wt_crop_pr = as.numeric(wt_crop[wt_crop$ParameterType == "XPRP", 5:75])

nf_crop = nat_first[nat_first$ParameterName == "SugC",]
nf_crop_pr = as.numeric(nf_crop[nf_crop$ParameterType == "XPRP", 5:75])

lines(nf_crop_pr, col = "#C00000", lty = 1, lwd = 2 )
lines(wt_crop_pr, col = "#6666FF", lty = 1, lwd = 2 )

#GTEM
gtem.sc.price = as.numeric(gtem.prices[gtem.prices$item == "10 Cane_beet", 32:dim(gtem.prices)[2]])

gtem.sc.price  = 18.91 + gtem.sc.price * 18.91/100 # 160.59 is the Rice price in 2014 from FAOSTAT

lines(gtem.sc.price, col = "orange", lty = 1, lwd = 2 )
title(main = "f) Sugar crops price", ylab = " ",  xlab = " ",cex=0.9)


plot(NULL ,xaxt='n',yaxt='n',bty='n',ylab='',xlab='', xlim=0:1, ylim=0:1)


plot(NULL ,xaxt='n',yaxt='n',bty='n',ylab='',xlab='', xlim=0:1, ylim=0:1)

legend("topright", legend = c("Historical (FAOSTAT)", "GLOBIOM - SSP2", "Working Together (ANO 2019)", "Nations First (ANO 2019)", "GTEM-Food"), 
       col= c("black","#72CC98", "#6666FF", "#C00000", "orange"), 
       lty = c(2,1,1,1,1), lwd = c(2,2,2,2,2), bty = "n", cex = 1) # optional legend

mtext("Projections", at=0.1, cex=0.75)

dev.off()





