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

## Change variables classes
genold <- mutate(genold,
                 genold = as_factor(genold),
                 party = as_factor(party),
                 rgroup = as_factor(rgroup),
                 region = as_factor(region))
str(genold)
