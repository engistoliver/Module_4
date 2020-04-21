# setup

rm(list=ls())
library(rio)
library(tidyverse)
library(texreg)
library(xtable)

# import data
basic <- import("basic.dta")
genold <- import("genold108.dta")

# convert to tibble 
basic <- as_tibble(basic)
genold <- as_tibble(genold)

# take subset, order alphabetcally by congressman
IO8 <- basic[basic$congress == 108,]
genold <- genold[order(genold$name),]

# drop identical columns and merge the two
genold <- subset(genold, select = -c(name, statenam, district))
comb <- bind_cols(genold, IO8)

# keep only relevant variables
comb <- comb[, c("name", "genold", "party", "ngirls", "nboys", "female", 
                 "age", "srvlng", "white", "region", "totchi", "rgroup")]

# gender -> factor, missing -> NA
comb$genold <- as.factor(comb$genold)
comb$genold[comb$genold == ''] <- NA

# party, rgroup, region -> factor
comb$party <- factor(comb$party, levels = c(1,2,3), labels=c("D", "R", "I"))
comb$rgroup <- factor(comb$rgroup)
comb$region <- factor(comb$region)

# square variables
comb$sqage <- comb$age ^ 2
comb$sqsrv <- comb$srvlng ^ 2 

# drop if children unknown
comb <- subset(comb, !is.na(comb$totchi))
comb <- subset(comb, !is.na(comb$genold))

# total children 
comb$totchi <- as.numeric(comb$totchi)

# regression
lfit <- lm(formula = totchi ~ 1 + genold + party + female + age + sqage + srvlng
          + sqsrv + white + region + rgroup, data = comb)

beta1all <- summary(lfit)[["coefficients"]][2,1]
sbeta1all <- summary(lfit)[["coefficients"]][2,2]

# by party
dcomb <- subset(comb, comb$party == "D")
rcomb <- subset(comb, comb$party == "R" | comb$party == "I")

dfit <- lm(formula = totchi ~ 1 + genold + female + age + sqage + srvlng
           + sqsrv + white + region + rgroup, data = dcomb)

rfit <- lm(formula = totchi ~ 1 + genold + female + age + sqage + srvlng
           + sqsrv + white + region + rgroup, data = rcomb)

beta1d <- summary(dfit)[["coefficients"]][2,1]
sbeta1d <- summary(dfit)[["coefficients"]][2,2]

beta1r <- summary(rfit)[["coefficients"]][2,1]
sbeta1r <- summary(rfit)[["coefficients"]][2,2]

# form coefficients into a table
nm <- c("All", "Dems", "Reps + Ind")
b <- c(beta1all, beta1d, beta1r)
names(b) <- nm
s <- c(sbeta1all, sbeta1d, sbeta1r)
names(s) <- nm

tab <- bind_rows(b,s)
print(tab)