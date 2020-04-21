

#* Load the libraries "Rio" and "tidyverse"
#* Change the path of the working directory to your working directory.


library(rio)
library(tidyverse)

setwd("C:/Users/Marco Borchert/Desktop/NUS/02_Courses/R/01_WF")

#load additional packages

options(warn=-1)

suppressMessages(library(RCurl))
suppressMessages(library(readxl))
suppressMessages(library(lubridate))
suppressMessages(library(ggplot2))
suppressMessages(library(tidyverse))
suppressMessages(library(graphics))
suppressMessages(library(reshape2))
suppressMessages(library(fBasics))
suppressMessages(library(moments))
suppressMessages(library(MASS))
suppressMessages(library(BBmisc))
suppressMessages(library(readstata13))
suppressMessages(library(Hmisc))
suppressMessages(library(psych))
suppressMessages(library(car))
suppressMessages(library(foreign))
suppressMessages(library(sandwich))
suppressMessages(library(lmtest))
suppressMessages(library(zoo))
suppressMessages(library(haven))
suppressMessages(library(Matrix))
suppressMessages(library(lfe))
suppressMessages(library(rvest))
suppressMessages(library(tidyverse))


#* import the data sets *basic.dta* and *genold108.dta*
#* create a subset of the 108th congress from the *basic* dataset
#* join this subset with the *genold* dataset


basic <- read.dta13("basic.dta") #note: a lot of NA cols 
genold <- read.dta13("genold108.dta")

basic_108 <- subset(basic, basic$congress == 108)

new <- merge(basic_108, genold, by.x="name", by.y="name")


# Data preparation

#* check table 1 in the appendix of the paper and decide which variables are necessary for the analysis (check the footnote for control variables)
#* drop all other variables.
#* Recode *genold* such that gender is a factor variable and missing values are coded as NAs.
#* Recode *party* as a factor with 3 levels (D, R, I)
#* Recode *rgroup* and *region* as factors.
#* generate variables for age squared and service length squared
#* create an additional variable of the number of children as factor variable

#necessary variables: legislator race, gender, party, age , agesq, service length, service length sq, religion, region 

#subset the df 

new_short <- new[,c(1,7,8,19,18,4,21,17,12,15,64)]

sq<-function(x) {
x^2
}

new_short$agesq <- sq(new_short$age) 
new_short$srvlngsq <- sq(new_short$srvlng)

#by squaring we control for a non-linear relationship 

fac<-function (x) {
x <- as.factor(x)
}

#manipulate genold col

new_short$genold[new_short$genold ==""] <- NA 
new_short$genold<- fac(new_short$genold)

#manipulate party col

new_short$party [which(new_short$party == "1")] = "D"
new_short$party [which(new_short$party == "2")] = "R"
new_short$party [which(new_short$party == "3")] = "I"

new_short$party <- as.factor(new_short$party)

#manipulate rgroup and region cols

new_short$rgroup <- fac(new_short$rgroup)
new_short$region <- fac(new_short$region)

new_short$nchil <- new_short$ngirls+new_short$nboys 
new_short$nchil <- fac(new_short$nchil)

#drop bernie sanders and change virgil goode.

new_short <- new_short[!new_short$name =="SANDERS, BERNARD" & !new_short$name =="GOODE, VIRGIL H., JR.",]

#since both virgil and bernie are considered independents, I decided to leave them out of the regression. The justification for this is that sample size remains strong and given their causal difference, it did not appear reasonable to me, to assign them back to either party. 

# Replicationg Table 1 from the Appendix

#We haven't covered regressions in R yet. Use the function *lm()*. The function takes the regression model (formula) and the data as an input. The model is written as $y \sim x$, where $x$ stands for any linear combination of regressors (e.g. $y \sim x_1 + x_2 + female$). Use the help file to understand the function.

#* Run the regression $total.children = \beta_0 + \beta_1 gender.oldest + \gamma'X$ where $\gamma$ stands for a vector of coefficients and $X$ is a matrix that contains all columns that are control variables.\footnote{This is just a short notation instead of writing the full model with all control variables $totchi = \beta_0 + \beta_1 genold + \gamma_1 age + \gamma_2 age^2 + \gamma_3 Democrat + ... + \epsilon$ which quickly gets out of hand for large models.}
#* Save the main coefficient of interest ($\beta_1$)
#* Run the same regression separately for Democrats and Republicans (assign the independent to one of the parties). Save the coefficient and standard error of *genold*
#* Collect all the *genold* coefficients from the six regressions, including their standard errors and arrange them in a table as in the paper.
#* print the table


#necessary variables: legislator race, gender, party, age , agesq, service length, service length sq, religion, region

new_short$nchil <- as.numeric(as.character(new_short$nchil)) # as numeric 

#number of children full

mo_lm <- lm(nchil~genold+age+agesq+white+female+party+srvlngsq+srvlng+rgroup+region, data=new_short)

mo_sum <- summary(mo_lm)

print(mo_sum)

#numbers of daughters full

mo2_felm<-felm(ngirls~genold+age+agesq+white+female+party+srvlngsq+srvlng+rgroup+region|nchil|0|0, data=new_short)

mo2_sum<-coef(summary(mo2_felm))

print(mo2_sum)



#for democrats only 

#numbers of children demo

new_short_demo <- subset(new_short,new_short$party=="D")

mo_demo <- lm(nchil~genold+age+agesq+white+female+srvlngsq+srvlng+rgroup+region, data=new_short_demo)

mo_demo_sum <- summary(mo_demo)

print(mo_demo_sum)

#number of daughters demo 

mo2_felm_demo<-felm(ngirls~genold+age+agesq+white+female+srvlngsq+srvlng+rgroup+region|nchil|0|0, data=new_short_demo)

mo2_demo_sum<-coef(summary(mo2_felm_demo))

print(mo2_demo_sum)


#for republicans only

#number of children repu

new_short_repu <- subset(new_short,new_short$party=="R")

mo_repu <- lm(nchil~genold+age+agesq+white+female+srvlngsq+srvlng+rgroup+region, data=new_short_repu)

mo_repu_sum <- summary(mo_repu)

print(mo_repu_sum)

#number of daughters repu

mo2_repu <- lm(ngirls~genold+age+agesq+white+female+srvlngsq+srvlng+rgroup+region+nchil, data=new_short_repu)

mo2_felm_repu<-felm(ngirls~genold+age+agesq+white+female+srvlngsq+srvlng+rgroup+region|nchil|0|0, data=new_short_repu)

mo2_repu<-coef(summary(mo2_felm_repu))

print(mo2_repu)



#table w/ stargazer

test<-stargazer::stargazer(mo2_felm,mo_lm,mo2_felm_demo,mo_demo,mo2_felm_repu,mo_repu,type="text",title = "Evidence on legislator child gender mix selection, 108th congress",keep = c("genoldG"), keep.stat = c("all","n"), style="qje", digits = 2, dep.var.labels= c("#daughter(CONG)","#child(CONG)","#daughter(DEMO)","#child(DEMO)","#daughter(REPU)","#child(REPU)"), font.size = "small" )
