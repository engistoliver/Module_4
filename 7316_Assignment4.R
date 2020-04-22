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
library(gt)

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

## Change variable classes (+ reclassify independent as democrat)
## + generate variables for agesq and srvlnsq
## + generate factor variable for number of children
genold <- mutate(genold,
                 genold = factor(na_if(genold, ""), levels = c("B", "G")),
                 party = factor(party, labels = c("D", "R", "D")),
                 rgroup = as_factor(rgroup),
                 region = as_factor(region),
                 agesq = age^2,
                 srvlngsq = srvlng ^2,
                 totchi_f = as_factor(totchi))
str(genold)

# Drop observations for which we don't know genold
genold <- drop_na(genold, genold)



# Replicating Table 1

## Filter republicans and democrats
dem_genold <- subset(genold, party == "D")
rep_genold <- subset(genold, party == "R")

str(genold)

## Run LS regression for all representatives: total children
reg <- lm(totchi ~ genold + female + white + age + agesq +
            srvlng + srvlngsq + rgroup + region + party,
          genold)
summary(reg)

## Save beta1 and standard error
betas <- c(coef(summary(reg))[2,1], coef(summary(reg))[2,2])


## Democrats regression: total children
reg <- lm(totchi ~ genold + female + white + age + agesq +
                srvlng + srvlngsq + rgroup + region,
              dem_genold)
betas <- cbind(betas, c(coef(summary(reg))[2,1], coef(summary(reg))[2,2]))

## Republicans regression: total children
reg <- lm(totchi ~ genold + female + white + age + agesq +
                srvlng + srvlngsq + rgroup + region,
              rep_genold)
betas <- cbind(betas, c(coef(summary(reg))[2,1], coef(summary(reg))[2,2]))




## Run LS regression for all reps: daughters
reg <- lm(ngirls ~ genold + female + white + age + agesq +
            srvlng + srvlngsq + rgroup + region + party,
          genold)
betas <- cbind(betas, c(coef(summary(reg))[2,1], coef(summary(reg))[2,2]))

## Democrats regression: daughters
reg <- lm(ngirls ~ genold + female + white + age + agesq +
            srvlng + srvlngsq + rgroup + region,
          dem_genold)
betas <- cbind(betas, c(coef(summary(reg))[2,1], coef(summary(reg))[2,2]))

## Republicans regression: daughters
reg <- lm(ngirls ~ genold + female + white + age + agesq +
            srvlng + srvlngsq + rgroup + region,
          rep_genold)
betas <- cbind(betas, c(coef(summary(reg))[2,1], coef(summary(reg))[2,2]))



# Calculate total observations
obs <- genold %>% group_by(party) %>% summarise(n())
obs <- append(as.numeric(count(genold)), t(obs)[2,])
obs <- append(obs[-4], obs[-4])


# Change table column names and row names
colnames(betas) <- c("Full Congress: Number of Children",
                     "Democrats: Number of Children",
                     "Republicans: Number of Children",
                     "Full Congress: Number of Daughters",
                     "Democrats: Number of Daughters",
                     "Republicans: Number of Daughters"
                     )
betas <- round(betas, 3)
betas <- rbind(betas, obs)
betas <- as.data.frame(betas) %>% mutate(Metric = c("First child female", "Standard error", "N"))

# Rearrange columns
betas <- betas[,c(7,4,1,5,2,6,3)]

# Print table
betas

# Create spanning headers
betas %>% gt() %>% tab_spanner_delim(delim = ":")