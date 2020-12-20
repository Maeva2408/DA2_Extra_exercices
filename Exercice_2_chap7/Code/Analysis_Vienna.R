#######################
## Extra exercices   ##
## DA2               ##
## exercices 2 chap7 ##
##  vienna hotels  ##
##                   ##
#######################

### Task : Analyse the pattern of association between the price of hotels in 
## Vienna with the number of stars

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
my_url <- "https://raw.githubusercontent.com/Maeva2408/DA2_Extra_exercices/main/Exercice_2_chap7/Data/Clean/hotel_vienna.csv"
Vienna <- read_csv( my_url )


## Non parametric regression

## Lowess regression
# Approximate with a lo(w)ess non-parametric line
scatter_vienna <- ggplot(  Vienna , aes(x = ratings, y = price )) +
  geom_point(fill= "orangered4", col= "salmon"  )+
  theme_bw()+
  scale_fill_wsj()+
  labs(x='Number of stars',y='Price ($)')

scatter_vienna

scatter_vienna + geom_smooth(method=loess,se=F,formula='y~x',color='black')


## Simple Linear regression
reg1 <- lm_robust( price ~ ratings , data = Vienna , se_type = "HC2" )
reg1

## Summary statistics
summary( reg1 )
## Vizualition
ggplot( data = Vienna, aes( x = ratings, y = price ) ) + 
  geom_point( color='salmon') +
  theme_bw()+
  geom_smooth( method = lm , color = 'orangered4' )+
  labs(x = "number if stars",y = "price in $") 

df_vienna_stat <- export_summs(reg1,
                               model.names = c( "Linear"),
                               caption= "model summary statistics")
as_hux(df_vienna_stat)

