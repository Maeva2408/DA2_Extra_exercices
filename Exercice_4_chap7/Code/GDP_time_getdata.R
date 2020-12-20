########################
## Extra exercices 4 ##
## Chap 7             ##
##                    ##
##   NO. 1            ##
## Get the data       ##
########################


# Clear memory and call packages
rm(list=ls())
library(WDI)
library(tidyverse)


# Download GDP and Time to start a business data for 2019
gdp <- WDI(indicator=c('NY.GDP.PCAP.PP.CD'), 
               country="all", start=2019, end=2019)
time <- WDI(indicator=c('IC.REG.DURS'), 
            country="all", start=2019, end=2019)

# Save the raw files
my_path <- "C:/Users/mbrae/OneDrive/Bureau/CEU/DA2/DA2_Extra_exercices/Exercices/exercice_4_chap7/Data/"
# GDP
write_csv(gdp, paste0(my_path,'Raw/GDP_data.csv'))
# Time
write_csv(time, paste0(my_path,'Raw/time_data.csv'))
