


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
# nat_first[,5:14] = NULL
worktog = read.csv("C:/Users/mar77v/Dropbox/Australia/GLOBIOM/latest_results/TwoDegrees/IIASA_GLOBIOM_emulator_interpolated_output 1990-2100.csv")
worktog = worktog[worktog$Regions == "AustraliaReg",]
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

# GTEM

gtem.prices = read.csv("prices_adj_AU_annual.csv")
gtem.output = read.csv("sector output AU physical.csv")

# Meat production
tiff("meat GTEM.tiff", compression = "lzw", res = 500, width = 140, height = 100, units = "mm")

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

# GTEM
gtem.beef = as.numeric(gtem.output[gtem.output$item == "17 Meat_cattle", 32:dim(gtem.output)[2]])
lines(gtem.beef, col = "orange", lty = 1, lwd = 2 )

# legend("bottomright", legend = c("Historical", "GLOBIOM"), col= c("#2DCBD3","#72CC98" ), 
#        lty = c(2,1), bty = "n") # optional legend
title(main = "a) Beef output", ylab = "1000 tons" )


# Meat_pork production

fmat = t(as.matrix(pn[pn$lu == "PGMEAT",]))
colnames(fmat) = fmat["ghg",]

# Get mean value
fmat.mean = (as.data.frame(fmat[4:74, 1:96]))
# Convert numeric data to 
fmat.mean[] <- lapply(fmat.mean, function(x) as.numeric(as.character(x)))



par(mar = c(4,4,1,0))
# matplot(fmat[4:74, 1:96], type = "l", ylab = " ",
#         col = "#72CC98", axes = F, ylim=c(0,3000)) #plot

plot(fmat[4:74, "GHG020_BIO0N"], type = "l", ylab = " ",
             col = "#72CC98", axes = F, ylim=c(0,3000))
axis(1, at = seq(0,71,10), labels = seq(1990,2060,10), cex.axis = 0.7)
axis(2, at = seq(0,3000,500), labels = seq(0,3000,500), cex.axis = 0.7)

lines(as.numeric(histpn["PGMEAT",]), col = "#2DCBD3", lty = 3, lwd = 2 )

#WT
wt_bmeat = worktog[worktog$ParameterName == "PGMEAT",]
wt_bmeat_pn = as.numeric(wt_bmeat[wt_bmeat$ParameterType == "PROD", 5:75])
lines(wt_bmeat_pn, col = "#6666FF", lty = 1, lwd = 2 )
# NF
nf_bmeat = nat_first[nat_first$ParameterName == "PGMEAT",]
nf_bmeat_pn = as.numeric(nf_bmeat[nf_bmeat$ParameterType == "PROD", 5:75])

lines(nf_bmeat_pn, col = "#C00000", lty = 6, lwd = 2 )

# GTEM
gtem.beef = as.numeric(gtem.output[gtem.output$item == "18 Meat_pork", 32:dim(gtem.output)[2]])
lines(gtem.beef, col = "orange", lty = 1, lwd = 2 )

# legend("bottomright", legend = c("Historical", "GLOBIOM"), col= c("#2DCBD3","#72CC98" ), 
#        lty = c(2,1), bty = "n") # optional legend
title(main = "b) Pork output", ylab = "1000 tons" )




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

#GTEM
gtem.beef.price = as.numeric(gtem.prices[gtem.prices$item == "17 Meat_cattle", 32:dim(gtem.prices)[2]])

gtem.beef.price  = 1452.81 + gtem.beef.price * 1452.81/100 # 1452.81 is the beef price in 2014 from FAOSTAT

lines(gtem.beef.price, col = "orange", lty = 1, lwd = 2 )

# legend("bottomright", legend = c("Historical", "GLOBIOM"), col= c("#2DCBD3","#72CC98" ), 
#        lty = c(2,1), bty = "n") # optional legend
title(main = "c) Beef prices", ylab = "USD 2000 per ton" )


# Pork price
fmat = t(as.matrix(prices[prices$lu == "PGMEAT",]))
par(mar = c(4,4,1,0))
matplot(fmat[4:74, 1:96], type = "l", ylab = " ",
        col = "#72CC98", axes = F, ylim=c(0,20000)) #plot
axis(1, at = seq(0,71,10), labels = seq(1990,2060,10), cex.axis = 0.7)
axis(2, at = seq(0,20000,5000), labels = seq(0,20000,5000), cex.axis = 0.7)

lines(as.numeric(histpric["PGMEAT",]), col = "#2DCBD3", lty = 3, lwd = 2 )

wt_bmeat_pn = as.numeric(wt_bmeat[wt_bmeat$ParameterType == "XPRP", 5:75])
nf_bmeat_pn = as.numeric(nf_bmeat[nf_bmeat$ParameterType == "XPRP", 5:75])

lines(nf_bmeat_pn, col = "#C00000", lty = 6, lwd = 2 )
lines(wt_bmeat_pn, col = "#6666FF", lty = 1, lwd = 2 )

#GTEM
gtem.pork.price = as.numeric(gtem.prices[gtem.prices$item == "18 Meat_pork", 32:dim(gtem.prices)[2]])

gtem.pork.price  = 1452.81 + gtem.pork.price * 1242.13/100 # 1242.13 is the pork price in 2014 from FAOSTAT

lines(gtem.pork.price, col = "orange", lty = 1, lwd = 2 )


legend("topright", legend = c("Historical", "GLOBIOM's outlooks", "Working Together", "Nations First", "GTEM-Food"), 
       col= c("#2DCBD3","#72CC98", "#6666FF", "#C00000", "orange"), 
       lty = c(2,1,1,6, 6), bty = "n", cex = 0.75) # optional legend
title(main = "d) Pork prices", ylab = " " )

dev.off()




# CROPS


tiff("Crops GTEM.tiff", compression = "lzw", res = 500, width = 190, height = 240, units = "mm")

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
