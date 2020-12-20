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
my_url <- "https://raw.githubusercontent.com/Maeva2408/DA2_Extra_exercices/main/Exercice_4_chap7/Data/Clean/GDPPC_TIME.csv"
df <- read_csv( my_url )


# New scale, GDPPC in thousands
df <- df %>% transmute( iso2c = iso2c,
                                    country = country,
                                    GDPPC   =  GDPPC/1000,
                                    year = year,
                                    days   = days)


## Non parametric regression

## Lowess regression
# Approximate with a lo(w)ess non-parametric line
scatter <- ggplot(df , aes(x = days, y = GDPPC )) +
  geom_point(fill= "orangered4", col= "salmon"  )+
  geom_smooth(method="loess",color = 'orangered4' )+
  theme_bw()+
  scale_fill_wsj()+
  labs(x='Number of days',y='GDPPC in thousand ($)')

scatter

## I know this chapter is before we learn the log transformation but so I will not
## take it and compare it because I think it could be interesting, So I will take log-log


scatter_ln <- ggplot( df , aes(x = days , y = GDPPC)) +
  geom_point(color = 'salmon') +
  geom_smooth(method="loess",color = 'orangered4' )+
  theme_bw()+
  labs(x = "Number of days (ln scale)",y = "GDPPC in thousand ($) (ln scale)") +
  scale_y_continuous( trans = log_trans(),  breaks = c(1,2,5,10,20,50,100,200,500,1000,10000) )+
  scale_x_continuous( trans = log_trans(),  breaks = c(1,2,5,10,20,50,100,200,500,1000,10000) )


scatter_ln

## Simple Linear regression
df<- df %>% mutate( ln_gdppc = log( GDPPC), ln_days = log(days))


reg1 <- lm_robust( GDPPC ~ days , data = df , se_type = "HC2" )
reg1
reg2 <- lm_robust( ln_gdppc ~ ln_days , data = df , se_type = "HC2" )
reg2
## Summary statistics
summary( reg1 )
summary( reg2 )

## Vizualition

## level-level
ggplot( data = df, aes( x = days, y = GDPPC ) ) + 
  geom_point( color='salmon') +
  theme_bw()+
  geom_smooth( method = lm , color = 'orangered4' )+
  labs(x = "Number of days",y = "GDPPC in thousand ($)") 
## log-log
ggplot( data = df, aes( x = ln_days, y = ln_gdppc ) ) + 
  geom_point( color='salmon') +
  theme_bw()+
  geom_smooth( method = lm , color = 'orangered4' )+
  labs(x = "Number of days (ln scale)",y = "GDPPC in thousand ($) (ln scale")
  
df_stat <- export_summs(reg1, reg2,
                               model.names = c( "Linear", "Linear log-log"),
                               caption= "model summary statistics")
as_hux(df_stat)

