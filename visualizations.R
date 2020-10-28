# Plots based on GTEM-Food data
# Raymundo Marcos Martinez
# ray.mmm@gmail.com

# October 28 2020


x = c("forecast", "ggplot2", "RColorBrewer", "scales",  "reshape", "cowplot", "ggthemes", 
      "dplyr", "randomcoloR", "patchwork", "waffle", "treemap", "foreign", "tidyr")

x =  c("readxl")

lapply(x, library, character.only = TRUE);

setwd("C:/Users/mar77v/CSIRO/ABARES CSIRO Modeling - Visualization/gtem-visual")

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

results = read_excel("sector price index.xlsx")