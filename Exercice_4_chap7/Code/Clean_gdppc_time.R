########################
## Assignment for DA2 ##
##  and for Coding    ##
##                    ##
##   NO. 2            ##
## Clean   data       ##
########################


# Clear memory and call packages
rm(list=ls())
library(tidyverse)

# Read the raw files
# covid data
my_path_GDP <- "https://raw.githubusercontent.com/Maeva2408/DA2_Extra_exercices/main/Exercice_4_chap7/Data/Raw/GDP_data.csv"
gdp <- read_csv(paste0(my_path_GDP))
# population data
my_path_time <- "https://raw.githubusercontent.com/Maeva2408/DA2_Extra_exercices/main/Exercice_4_chap7/Data/Raw/time_data.csv"
time <- read_csv(paste0(my_path_time))


####
# CLEANING
# GDP
# Rename variables
gdp <- gdp %>% rename(GDPPC = NY.GDP.PCAP.PP.CD)
# Time
time <- time %>% rename(days = IC.REG.DURS)

## Check the observations:
# 1) Filter out grouping observations based on using digits
gdp <- gdp %>% filter( !grepl("[[:digit:]]", gdp$iso2c) )
time <- time %>% filter( !grepl("[[:digit:]]", time$iso2c) )

# Some grouping observations are still there, check each of them
#   HK - Hong Kong, China
#   OE - OECD members
#   all with starting X, except XK which is Kosovo
#   all with starting Z, except ZA-South Africa, ZM-Zambia and ZW-Zimbabwe

# 2nd drop specific values
drop_id <- c("EU","HK","OE")
gdp<- gdp %>% filter( !grepl( paste( drop_id , collapse="|"), gdp$iso2c ) ) 
time <- time %>% filter( !grepl( paste( drop_id , collapse="|"), time$iso2c ) ) 

# 3rd drop values with certain starting char
# Get the first letter from iso2c
fl_iso2c <- substr(gdp$iso2c, 1, 1)
fl_iso2c2 <- substr(time$iso2c, 1, 1)

retain_id <- c("XK","ZA","ZM","ZW")
# Filter out everything which starts X or Z except countries in retain_id
gdp <- gdp %>% filter( !( grepl( "X", fl_iso2c ) | grepl( "Z", fl_iso2c ) & 
                            !grepl( paste( retain_id , collapse="|"), gdp$iso2c ) ) ) 
time <- time %>% filter( !( grepl( "X", fl_iso2c2 ) | grepl( "Z", fl_iso2c2 ) & 
                            !grepl( paste( retain_id , collapse="|"), time$iso2c ) ) ) 
rm( drop_id, fl_iso2c, fl_iso2c2 , retain_id )

# Retain and rename variables which are going to be used later
pop <-pop %>% transmute( country = country,
                         population=SP.POP.TOTL )

################
# MERGE the two data table
##


df <- full_join(gdp,time)

## both of my table comes from the world bank so there is not problem for
## matching countries

#####
# Handle missing values:
# Drop if population, confirmed cases or death is missing
df <- df %>% filter( !( is.na( GDPPC ) | is.na( days ) ))


#####
# Save clean data
my_path <- "C:/Users/mbrae/OneDrive/Bureau/CEU/DA2/DA2_Extra_exercices/Exercices/exercice_4_chap7/Data/"
# GDPPPC_TIME data
write_csv( df , paste0(my_path,'clean/GDPPC_TIME.csv'))

