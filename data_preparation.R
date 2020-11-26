# Plots based on GTEM-Food data
# Raymundo Marcos Martinez
# ray.mmm@gmail.com

# October 28 2020


# x = c("forecast", "ggplot2", "RColorBrewer", "scales",  "reshape", "cowplot", "ggthemes", 
#       "dplyr", "randomcoloR", "patchwork", "waffle", "treemap", "foreign", "tidyr")
# 
x =  c("readxl", "data.table")

lapply(x, library, character.only = TRUE);

# old data
setwd("C:/Users/mar77v/CSIRO/ABARES CSIRO Modeling - Visualization/gtem-visual") 

# Data Nov 19
setwd("C:/Users/mar77v/CSIRO/ABARES CSIRO Modeling - Visualization/results_nov") 


# Clear workspace
rm(list=ls())

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

results = read_excel_allsheets("sector price index.xlsx")

# Select AU data. Note for other countries we need to indicate the corresponding

# column number
prices = as.data.frame(results[1])[,1:2]
# Concatenate all the periods

for(j in 2:length(results)){
  print(j)
  prices = cbind(prices, as.data.frame(results[j])[,2]) 
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

# -----------------------------------------------------------------------------
# Select country data and transpose it
  cpi_region = cpi5y[cpi5y$L_CPI == "1 Australia", ]
#-----------------------------------------------------------------------------
# make the data the same lenght as the price index data
  cpi_region = cpi_region[rep(row.names(cpi_region), 
                                       dim(prices)[1]),]
  
# remove inflation values from price index
  
  prices.adj = prices
  prices.adj[,2:dim(prices)[2]] =  prices.adj[,2:dim(prices)[2]] - cpi_region[,2:dim(cpi_region)[2]]
 
  
# Need to expand the data to annual 
  
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
  
# Does not work due to neg vals

  ## geometric interpolation for each item
  res <- prices.adj[, interp(year, value), by= item]
  
  ## Plot
  library(ggplot2)
  ggplot(res, aes(year, value, color=name)) +
    geom_line(lwd=1.3) + theme_bw() +
    geom_point(data=towns, cex=2, color='black') +  # add points interpolated between
    scale_color_brewer(palette='Pastel1')  
  
  # ---------------------------------------------------------------------
  # Output
  # ---------------------------------------------------------------------
  
  output.A = read_excel_allsheets("sector output index.xlsx")
  
  # Select AU data. Note for other countries we need to indicate the corresponding
    # column number
  output = as.data.frame(output.A[1])[,1:2]
  # Concatenate all the periods
  
  for(j in 2:length( output.A)){
    print(j)
    output = cbind(output, as.data.frame(output.A[j])[,2]) 
  }
  
  colnames(output) = c("item", seq(2015,2060,5))
  
  write.csv(output, "sector output AU.csv")
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  # GLOBAL DATA

  setwd("C:/Users/mar77v/CSIRO/ABARES CSIRO Modeling - Visualization/gtem-visual")
  
  
  # Adjust data for inflation using CPI
  
  cpi = read.csv("CPI.csv")
  
  # Adjust the data based on phone chat with YL
  cpi[,2:dim(cpi)[2]] =  (cpi[,2:dim(cpi)[2]] - 1) * 100
  
  # select five-year data
  cpi5y = subset(cpi, select = c("L_CPI", "X2.year2015", "X7.year2020", 
                                 "X12.year2025", "X17.year2030",
                                 "X22.year2035", "X27.year2040", 
                                 "X32.year2045", "X37.year2050", 
                                 "X42.year2055", "X47.year2060"))
  
  
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
  
  results_nom = read_excel_allsheets("sector price index.xlsx")
  
  # remove inflation values from the price index data
  results = results_nom

  for(t in 1:length(results)){
    cpitmp =  results_nom[[t]][,2:36]
    for(j in 1: dim(cpitmp)[1]){ cpitmp[j,] = as.vector(cpi5y[1:35,t + 1])}
    results[[t]][,2:36] = results_nom[[t]][,2:36] - cpitmp
  }
  
  # Create an average price index table
  
  avpric = as.data.frame(results[[1]][,1])
  avpric["2015"] = rowMeans(results[[1]][,2:36])
  avpric["2020"] = rowMeans(results[[2]][,2:36])
  avpric["2025"] = rowMeans(results[[3]][,2:36])
  avpric["2030"] = rowMeans(results[[4]][,2:36])
  avpric["2035"] = rowMeans(results[[5]][,2:36])
  avpric["2040"] = rowMeans(results[[6]][,2:36])
  avpric["2045"] = rowMeans(results[[7]][,2:36])
  avpric["2050"] = rowMeans(results[[8]][,2:36])
  avpric["2055"] = rowMeans(results[[9]][,2:36])
  avpric["2060"] = rowMeans(results[[10]][,2:36])

write.csv(avpric, "average global prices.csv")

  

# production


output.A = read_excel_allsheets("sector output index.xlsx")


# Create an average price index table

globoutput = as.data.frame(output.A [[1]][,1])
globoutput["2015"] = rowSums(output.A [[1]][,2:36])
globoutput["2020"] = rowSums(output.A [[2]][,2:36])
globoutput["2025"] = rowSums(output.A [[3]][,2:36])
globoutput["2030"] = rowSums(output.A [[4]][,2:36])
globoutput["2035"] = rowSums(output.A [[5]][,2:36])
globoutput["2040"] = rowSums(output.A [[6]][,2:36])
globoutput["2045"] = rowSums(output.A [[7]][,2:36])
globoutput["2050"] = rowSums(output.A [[8]][,2:36])
globoutput["2055"] = rowSums(output.A [[9]][,2:36])
globoutput["2060"] = rowSums(output.A [[10]][,2:36])

write.csv(globoutput, "global sector output.csv")












  
  # # Need to expand the data to annual 
  # 
  # ## First, transform data from wide -> long format, clean year column
  # # or use reshape2::melt
  # prices.adj <- melt(as.data.table( prices.adj), id.vars='item', variable.name='year')  # wide -> long
  # prices.adj[, year := as.integer(sub('[[:alpha:]]', '', year))]                      # convert years to integers
  # 
  # ## Function to interpolate at constant rate for each interval
  # interp <- function(yrs, values) {
  #   tt <- diff(yrs)               # interval lengths
  #   N <- head(values, -1L)     
  #   P <- tail(values, -1L)
  #   r <- (log(P) - log(N)) / tt   # rate for interval
  #   const_rate <- function(N, r, time) N*exp(r*(0:(time-1L)))
  #   list(year=seq.int(min(yrs), max(yrs), by=1L),
  #        value=c(unlist(Map(const_rate, N, r, tt)), tail(P, 1L)))
  # }  
  # 