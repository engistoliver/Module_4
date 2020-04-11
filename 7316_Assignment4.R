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

## Run LS regression
reg <- lm(totchi ~ genold + female + white + age + agesq +
            srvlng + srvlngsq + rgroup + region,
          genold)

## Save beta1
beta1 <- coefficients(reg)[2]
beta1

## Democrats regression
dem_genold <- subset(genold, party == "D")
dem_reg <- lm(totchi ~ genold + female + white + age + agesq +
                srvlng + srvlngsq + rgroup + region,
              dem_genold)

## Republicans regression
rep_genold <- subset(genold, party == "R")
rep_reg <- lm(totchi ~ genold + female + white + age + agesq +
                srvlng + srvlngsq + rgroup + region,
              rep_genold)