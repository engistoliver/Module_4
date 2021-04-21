# Assignment 4: Github Instructions for Module 4
# Course 7316: Introduction to Data Analysis in R
# 19/04/2021
# Sophie Adamiok - 41908

# Load knitr package
library(knitr)


---
  title: "Module 4 - Instructions"
author: "Oliver Engist"
date: "04/04/2021"
output: pdf_document
---

# Setup
# Load the libraries "Rio" and "tidyverse"
# Change the path of the working directory to your working directory.

```{r, message=FALSE}

library(rio)
library(tidyverse)
library(magrittr)
setwd("/Users/SophieAdamiok/Documents/Stockholm/7316 Introduction to Data Analytics in R/Assignments/Assignment 4 files")

```
  
# Import the data sets *basic.dta* and *genold108.dta*
import("~/Documents/Stockholm/7316 Introduction to Data Analytics in R/Assignments/Assignment 4 files/basic.dta")
import("~/Documents/Stockholm/7316 Introduction to Data Analytics in R/Assignments/Assignment 4 files/genold108.dta")

# Create a subset of the 108th congress from the *basic* dataset
Basic_108 <- basic %>% filter(congress == "108")

# Join this subset with the *genold* dataset
Joined_dataset <- left_join(Basic_108, genold108, by ="name") 

# Data preparation
# Check table 1 in the appendix of the paper and decide which variables are necessary for the analysis (check the footnote for control variables)
# I will drop the variables race. I think it is sensible to drop the variable race as race may exhibit collinearity with other remaining
# variables such as region (certain regions in the US are likely to be more multiethnical than others: e.g., Idaho vs Hawaii). Consequently, I have also deleted
# the associated variable of percentage of white inhabitants. 

# Furthermore, I have decided to dropyear and congress as these variables are the same for all observations and thus do not add additional information and I will also drop servlng
# for similar collinearity reasons as for the race variable

# Drop all other variables.
Joined_dataset_clear <- Joined_dataset %>% select(name, ngirls, anygirls, genold, totchi, party, rgroup, region, age, female)

# Recode *genold* such that gender is a factor variable
Joined_dataset_clear$genold <- as.factor(Joined_dataset_clear$genold)
# Check whether recoding was successful
is.factor(Joined_dataset_clear$genold)
# Recode genold so that missing values are coded as NAs.
Joined_dataset_clear$genold [Joined_dataset_clear$genold != "B" & Joined_dataset_clear$genold != "G"] <- NA

# Recode *party* as a factor
Joined_dataset_clear$party <- as.factor(Joined_dataset_clear$party)
# Check whether recoding was successful
is.factor(Joined_dataset_clear$party)
# Generate three levels (D, R, I)
print(Joined_dataset_clear$party)
Joined_dataset_clear$party <- recode(Joined_dataset_clear$party, "1" = "D", "2" = "R", "3" = "I")

# Recode *rgroup* and *region* as factors.
Joined_dataset_clear$rgroup <- as.factor(Joined_dataset_clear$rgroup)
# Check whether recoding was successful
is.factor(Joined_dataset_clear$rgroup)

# Generate variables for age squared and service length squared
Joined_dataset_clear %<>% mutate(age_squared = age^2)

# Create an additional variable of the number of children as factor variable
Joined_dataset_clear %<>% mutate(totchi_factor = totchi)
Joined_dataset_clear$totchi_factor <- as.factor(Joined_dataset_clear$totchi_factor)
# Check whether recoding was successful
is.factor(Joined_dataset_clear$totchi_factor)

# Replicating Table 1 from the Appendix
# We haven't covered regressions in R yet. Use the function *lm()*. The function takes the regression model (formula) and the data as an input. 
# The model is written as $y \sim x$, where $x$ stands for any linear combination of regressors (e.g. $y \sim x_1 + x_2 + female$). 
# Use the help file to understand the function.
# Run the regression $total.children = \beta_0 + \beta_1 gender.oldest + \gamma'X$ where $\gamma$ stands for a vector of coefficients and $X$ is a 
# matrix that contains all columns that are control variables.\footnote{This is just a short notation instead of writing the full model with all control 
# variables $totchi = \beta_0 + \beta_1 genold + \gamma_1 age + \gamma_2 age^2 + \gamma_3 Democrat + ... + \epsilon$ which quickly gets out of hand for 
# large models.}

# Regressions with totchi as dependent variable
# Total Congress
lm.reg <- lm(Joined_dataset_clear$totchi ~ Joined_dataset_clear$genold + Joined_dataset_clear$region + Joined_dataset_clear$rgroup + Joined_dataset_clear$age + Joined_dataset_clear$age_squared + Joined_dataset_clear$female)
# Show coefficient of genold
summary(lm.reg)$coefficients

# Run the same regression separately for Democrats and Republicans (assign the independent to one of the parties). 
# Democrats
# Filter for Democrats
Democrats <- filter(Joined_dataset_clear, party == "D")
# Run the regression for the Democrats
lm.reg_Democrats <- lm(Democrats$totchi ~ Democrats$genold + Democrats$region + Democrats$rgroup + Democrats$age + Democrats$age_squared + Democrats$female)
# Show coefficient of genold
summary(lm.reg_Democrats)$coefficients
# Republicans
# Filter for Republicans
Republicans <- filter(Joined_dataset_clear, party == "R")
# Run the regression for the Republicans
lm.reg_Republicans <- lm(Republicans$totchi ~ Republicans$genold + Republicans$region + Republicans$rgroup + Republicans$age + Republicans$age_squared + Republicans$female)
# Show coefficient of genold
summary(lm.reg_Republicans)$coefficients

# Regressions with ngirls as dependent variable
# Total Congress
lm.reg_ngirls <- lm(Joined_dataset_clear$ngirls ~ Joined_dataset_clear$genold + Joined_dataset_clear$region + Joined_dataset_clear$rgroup + Joined_dataset_clear$age + Joined_dataset_clear$age_squared + Joined_dataset_clear$female)
# Show coefficient of genold
summary(lm.reg_ngirls)$coefficients

# Run the same regression separately for Democrats and Republicans (assign the independent to one of the parties). 
# Democrats
# Run the regression for the Democrats
lm.reg_Democrats_ngirls <- lm(Democrats$ngirls ~ Democrats$genold + Democrats$region + Democrats$rgroup + Democrats$age + Democrats$age_squared + Democrats$female)
# Show coefficient of genold
summary(lm.reg_Democrats_ngirls)$coefficients
# Republicans
# Run the regression for the Republicans
lm.reg_Republicans_ngirls <- lm(Republicans$ngirls ~ Republicans$genold + Republicans$region + Republicans$rgroup + Republicans$age + Republicans$age_squared + Republicans$female)
# Show coefficient of genold
summary(lm.reg_Republicans_ngirls)$coefficients

# Collect all the *genold* coefficients from the six regressions, including their standard errors and arrange them in a table as in the paper.
# Save the coefficient of genold
# Total Congress totchi
lm.reg_beta_1_total_congress <- summary(lm.reg)$coefficients[2, 1] # Regression coefficient
lm.reg_SE_beta1_total_congress_lm.reg <- summary(lm.reg)$coefficients[2, 2] # Corresponding standard error
# Democrats totchi
Democrats_beta_1_Dem_lm.reg <- summary(lm.reg_Democrats)$coefficients[2, 1] # Regression coefficient
Democrats_SE_beta_1_Dem_lm.reg <- summary(lm.reg_Democrats)$coefficients[2, 2] # Corresponding standard error
# Republicans totchi
Republicans_beta_1_Rep_lm.reg <- summary(lm.reg_Republicans)$coefficients[2, 1] # Regression coefficient
Republicans_SE_beta_1_Rep_lm.reg <- summary(lm.reg_Republicans)$coefficients[2, 2] # Corresponding standard error
# Total Congress ngirls
lm.reg_ngirls_beta_1_total_congress <- summary(lm.reg_ngirls)$coefficients[2, 1] # Regression coefficient
lm.reg_ngirls_SE_beta_1_total_congress <- summary(lm.reg_ngirls)$coefficients[2, 2] # Corresponding standard error
# Democrats ngirls
Democrats_ngirls_beta_1_Dem_lm.reg <- summary(lm.reg_Democrats_ngirls)$coefficients[2, 1] # Regression coefficient
Democrats_ngirls_SE_beta_1_Dem_lm.reg <- summary(lm.reg_Democrats_ngirls)$coefficients[2, 2] # Corresponding standard error
# Republicans ngirls
Republicans_ngirls_beta_1_Rep_lm.reg <- summary(lm.reg_Republicans_ngirls)$coefficients[2, 1] # Regression coefficient
Republicans_ngirls_SE_beta_1_Rep_lm.reg <- summary(lm.reg_Republicans_ngirls)$coefficients[2, 2] # Corresponding standard error

# Arrange them in a table
# Define a new dataset for the totchi regressions and reorganize them
Table1_App_Recreated_totchi <- tibble(lm.reg_beta_1_total_congress, lm.reg_SE_beta1_total_congress_lm.reg, Democrats_beta_1_Dem_lm.reg, Democrats_SE_beta_1_Dem_lm.reg, Republicans_beta_1_Rep_lm.reg, Republicans_SE_beta_1_Rep_lm.reg)
# Rename columns
colnames(Table1_App_Recreated_totchi)[1] <- "beta total congress"
colnames(Table1_App_Recreated_totchi)[2] <- "SE beta total congress"
colnames(Table1_App_Recreated_totchi)[3] <- "beta Democrats"
colnames(Table1_App_Recreated_totchi)[4] <- "SE beta Democrats"
colnames(Table1_App_Recreated_totchi)[5] <- "beta Republicans"
colnames(Table1_App_Recreated_totchi)[6] <- "SE beta Republicans"

# Define a new dataset for the ngirls regressions
# Define a new dataset for the genold regressions and reorganize them
Table1_App_Recreated_ngirls <- tibble(lm.reg_ngirls_beta_1_total_congress, lm.reg_ngirls_SE_beta_1_total_congress, Democrats_ngirls_beta_1_Dem_lm.reg, Democrats_ngirls_SE_beta_1_Dem_lm.reg, Republicans_ngirls_beta_1_Rep_lm.reg, Republicans_ngirls_SE_beta_1_Rep_lm.reg)
# Rename columns
colnames(Table1_App_Recreated_ngirls)[1] <- "beta total congress"
colnames(Table1_App_Recreated_ngirls)[2] <- "SE beta total congress"
colnames(Table1_App_Recreated_ngirls)[3] <- "beta Democrats"
colnames(Table1_App_Recreated_ngirls)[4] <- "SE beta Democrats"
colnames(Table1_App_Recreated_ngirls)[5] <- "beta Republicans"
colnames(Table1_App_Recreated_ngirls)[6] <- "SE beta Republicans"

# Bind Table1_App_Recreated_genold and Table1_App_Recreated_ngirls together
Table <- rbind(Table1_App_Recreated_totchi, Table1_App_Recreated_ngirls)

# Rename rows in Table
rownames(Table)[1] <- "1st child is female"
rownames(Table)[2] <- "Number of girls"

# Print the table
print(Table)

# Save as pdf
rstudioapi::documentSave("Assignment 4_41908.r")
knitr::stitch("Assignment 4_41908.r")

# test




