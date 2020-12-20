#######################
## Extra exercices   ##
## DA2               ##
## exercices 3 chap7 ##
##  Brussels hotels  ##
##                   ##
#######################

### Task : Analyse the pattern of association between the price of hotels in 
## Brussels with the distance from the city center

# Clear memory
rm(list=ls())

# Packages to use
library(ggthemes)
library(tidyverse)
library(geosphere)
library(moments)
library(dplyr)
library(knitr)
library(pander)
library(huxtable)
library(jtools)
library(ggplot2)
library(texreg)
library(estimatr)

# First, I will import ly clean data from github
my_url <- "https://raw.githubusercontent.com/Maeva2408/DA2_Extra_exercices/main/Exercice_3_chap7/data/clean/hotel_Brussels.csv"
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

hotels_brussels %>% 
  keep(is.numeric) %>%
  ggplot(aes(x= distance))+
  geom_histogram(fill= "orangered4", col= "salmon")+
  theme_bw()+
  scale_fill_wsj()+
  labs(x = "Distance from the city center (miles))",y = "count of hotels")

# Summary statistics of the price
df_hotels_brussels <- summarise(hotels_brussels,
                             variable = "price",
                             mean = mean(x = price),
                             median = median(x = price),
                             min= min(x = price),
                             max = max(x = price),
                             sd = sd(x = price),
                             skew = skewness(x = price))


kable(df_hotels_brussels)


## Non parametric regression
# Bin scatter with four bins
# Scatter plot

scatter_brussels <- ggplot(data = hotels_brussels) +
  geom_point(aes(x = distance, y = price), color = 'salmon') + 
  expand_limits(x = 0.01, y = 0.01) +
  scale_x_continuous(expand = c(0.01,0.01), limits=c(0, 7), breaks=seq(0, 7, by=1)) + 
  scale_y_continuous(expand = c(0.01,0.01),limits = c(0, 400), breaks = seq(0, 400, by = 50)) +
  labs(x = "Distance to city center (miles)",y = "Price (dollars)")+
  theme_bw() 
scatter_brussels

# Four bins : between 0,5 and 1,5 miles, between 1,5 and 2,5, between 2,5 and 3,5, between 3,5 and 4,5;
hotels_brussels <-hotels_brussels %>% mutate(dist4=0.5+ 1*as.numeric(hotels_brussels$distance>=1) 
                + 1*as.numeric(hotels_brussels$distance>=2) + 1*as.numeric(hotels_brussels$distance>=3))
dist4 <- hotels_brussels %>% group_by(dist4) %>% dplyr::summarize(Eprice_cat4=mean(price))
hotels_brussels<-left_join(hotels_brussels,dist4)

hotels_brussels$xend <- c(hotels_brussels$dist4+1)
hotels_brussels$yend <- c(hotels_brussels$Eprice_cat4)

bins_hotels <-  scatter_brussels+
  geom_segment(data=hotels_brussels, aes(x = dist4, y=yend, xend=xend, yend=yend)) 
bins_hotels
  
## Lowess regression
# Approximate with a lo(w)ess non-parametric line
scatter <- ggplot( hotels_brussels , aes(x = distance, y = price )) +
  geom_point(fill= "orangered4", col= "salmon"  )+
  theme_bw()+
  scale_fill_wsj()+
  labs(x='Distance (miles)',y='Price ($)')

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

