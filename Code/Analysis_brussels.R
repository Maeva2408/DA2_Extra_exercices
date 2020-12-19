#######################
## Assignement for   ##
## DA2 and COD1      ##
## Analysis of       ##
##  number of        ##
## registered death  ##
## and number of     ##
## registered case $ ##
## due to covid on   ##
##  the 25/10/2020   ##
##                   ##
## Analysis of       ##
##   the data        ##
##                   ##
#######################

### Task : Analyse the pattern of association between registered covid-19 
### cases and registered number of death du to covid-19 on the 26 october 2020.

# Clear memory
rm(list=ls())

# Packages to use
library(tidyverse)
# For scaling ggplots
require(scales)
# Estimate piecewise linear splines
#install.packages("lspline")
library(lspline)
# Estimate robust SE
#install.packages("estimatr")
library(estimatr)
# Compare models with robust SE
#install.packages("texreg")
library(texreg)
# For different themes
#install.packages(ggthemes)
library(ggthemes)
library(tidyverse)
library(geosphere)
library(moments)
library(dplyr)
library(knitr)
library(pander)
install.packages("jtools")
install.packages("huxtable")
library(huxtable)
library(jtools)
# First, I will import ly clean data from github
my_url <- "https://raw.githubusercontent.com/Maeva2408/DA2_Extra_exercices/main/data/clean/hotel_Brussels.csv"
brussels <- read_csv( my_url )



######

# I will examine the distribution of the variable and drop observation if needed
brussels %>% 
  keep(is.numeric) %>%
  ggplot(aes(x= distance))+
  geom_histogram(fill= "orangered4", col= "salmon")+
  theme_bw()+
  scale_fill_wsj()+
  labs(x = "Distance from the city center (miles))",y = "count of hotels")

# I Will filter out the hotels with distance that have more that 5 miles and price more than 400$
hotels_brussels <- filter( brussels , 
                          year == 2017 & month == 11 & weekend == 0,
                          accommodation_type == "Hotel" , 
                          stars >= 3 & stars <= 4, distance <= 5,
                          price <= 400)

df_hotels_brussels <- summarise(hotels_brussels,
                             variable = "price",
                             mean = mean(x = price),
                             median = median(x = price),
                             min= min(x = price),
                             max = max(x = price),
                             sd = sd(x = price),
                             skew = skewness(x = price))


kable(df_hotels_brussels)




hotels_brussels %>% 
  keep(is.numeric) %>%
  ggplot(aes(x= distance))+
  geom_histogram(fill= "orangered4", col= "salmon")+
  theme_bw()+
  scale_fill_wsj()+
  labs(x = "Distance from the city center (miles))",y = "count of hotels")

## Non parametric regression
# Bin scatter with four bins
hotels_brussels %>% ggplot(aes(x = distance, y = price)) +
  geom_point( size = 3)+
  stat_summary_bin(fun ='mean', bins = 4, color='salmon', size= 7, geom='point')
  labs(x='Distance (miles)',y='price ($)')

## Lowess regression
# Scatter plot
scatter <- ggplot( hotels_brussels , aes(x = distance, y = price )) +
  geom_point(fill= "orangered4", col= "salmon"  )+
  theme_bw()+
  scale_fill_wsj()+
  labs(x='Distance (miles)',y='Price ($)')


# Approximate with a lo(w)ess non-parametric line
scatter + geom_smooth(method=loess,se=F,formula='y~x',color='black')


## Simple Linear regression
reg1 <- lm_robust( price ~ distance , data = hotels_brussels , se_type = "HC2" )
reg1

## Summary statistics
summary( reg1 )
## Vizualition
ggplot( data = hotels_brussels, aes( x = distance, y = price ) ) + 
  geom_point( color='salmon') +
  theme_bw()+
  geom_smooth( method = lm , color = 'orangered4' )+
  labs(x = "distance",y = "price") 

df_brussels_stat <- export_summs(reg1,
                              model.names = c( "Linear"),
                              caption= "model summary statistics")
as_hux(df_brussels_stat)

