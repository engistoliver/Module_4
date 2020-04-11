#####################################
##          Assignment 4           ##
##    Albert Planting-Gyllenbaga   ##
##           ID: 41699             ##
#####################################


# Clear the working space
rm(list = ls())

# Load packages
library(tidyverse)
library(rio)
library(haven)

# Import datasets
df <- read_dta("basic.dta")
genold <- read_dta("genold108.dta")

# Create subset of 108th congress and join to genold dataset
genold <- left_join(genold, subset(df, congress == 108))




# Data preparation

## Drop unnecessary variables
vars <- c("party", "genold", "ngirls", "totchi", "female", "white", "age",
          "srvlng","rgroup", "region")
genold <- genold[vars]

## Change variable classes
## + generate variables for agesq and srvlnsq
## + generate factor variable for number of children
genold <- mutate(genold,
                 genold = as_factor(na_if(genold, "")),
                 party = factor(party, labels = c("D", "R", "I")),
                 rgroup = as_factor(rgroup),
                 region = as_factor(region),
                 agesq = age^2,
                 srvlngsq = srvlng ^2,
                 totchi_f = as_factor(totchi))
str(genold)




# Replicating Table 1

betas <- matrix(nrow = 2, ncol = 6)

## Run LS regression for all representatives: total children
reg <- lm(totchi ~ genold + female + white + age + agesq +
            srvlng + srvlngsq + rgroup + region,
          genold)

## Save beta1 and standard error
beta_all <- c(coef(summary(reg))[2,1], coef(summary(reg))[2,2])


## Democrats regression: total children
dem_genold <- subset(genold, party == "D")
reg <- lm(totchi ~ genold + female + white + age + agesq +
                srvlng + srvlngsq + rgroup + region,
              dem_genold)
beta_dem <- c(coef(summary(reg))[2,1], coef(summary(reg))[2,2])


## Republicans regression: total children
rep_genold <- subset(genold, party == "R")
reg <- lm(totchi ~ genold + female + white + age + agesq +
                srvlng + srvlngsq + rgroup + region,
              rep_genold)
beta_rep <- c(coef(summary(reg))[2,1], coef(summary(reg))[2,2])




## Run LS regression for all reps: daughters
reg <- lm(ngirls ~ genold + female + white + age + agesq +
            srvlng + srvlngsq + rgroup + region,
          genold)
beta_all_d <- c(coef(summary(reg))[2,1], coef(summary(reg))[2,2])

## Democrats regression: daughters
reg <- lm(ngirls ~ genold + female + white + age + agesq +
            srvlng + srvlngsq + rgroup + region,
          dem_genold)
beta_dem_d <- c(coef(summary(reg))[2,1], coef(summary(reg))[2,2])

## Republicans regression: daughters
reg <- lm(ngirls ~ genold + female + white + age + agesq +
            srvlng + srvlngsq + rgroup + region,
          rep_genold)
beta_rep_d <- c(coef(summary(reg))[2,1], coef(summary(reg))[2,2])


cbind(beta)

