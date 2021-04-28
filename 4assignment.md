4assignment
================

\#Introduction to Data Analysis in R \#Assignment 4 - Teresa Helleniemi

\#Setup library(rio) library(tidyverse) library(magrittr)

\#Import the data sets library(haven) basic &lt;- read\_dta(“basic.dta”)
View(basic) library(haven) genold108 &lt;- read\_dta(“genold108.dta”)
View(genold108)

\#Create a subset of the 108th congress from the *basic* dataset basic
%&lt;&gt;% filter(grepl(“108”, basic$congress)) \#Join this subset with
the *genold* dataset bg &lt;- merge(basic, genold108)

bgo &lt;- left\_join(basic, genold108) \#Check table 1 in the appendix
of the paper and decide which variables are necessary for the analysis
\#legislator race, gender, party, age, age squared, service length,
service length squared, religion, region, fixed effects of total number
of children

\#drop all other variables. \#Recode *genold* such that gender is a
factor variable and missing values are coded as NAs. \#Recode *party* as
a factor with 3 levels (D, R, I) \#Recode *rgroup* and *region* as
factors. \#generate variables for age squared and service length squared
\#create an additional variable of the number of children as factor
variable

bgo2 &lt;- bgo\[c(3,7,9,12,15,17,18,19,21)\]
bgo2*a**g**e**s**q**r**t* &lt;  − *b**g**o*2age^2
bgo2*s**e**r**v**s**q**r**t* &lt;  − *b**g**o*2srvlng^2
bgo2*g**e**n**o**l**d* &lt;  − *g**e**n**o**l**d*108genold

genold108*g**e**n**o**l**d*\[*g**e**n**o**l**d*108genold == ""\] &lt;-
NA

genold108*g**e**n**o**l**d* &lt;  − *a**s*.*f**a**c**t**o**r*(*g**e**n**o**l**d*108genold)

bgo2*p**a**r**t**y* &lt;  − *a**s*.*f**a**c**t**o**r*(*b**g**o*2party)
levels(bgo2$party) &lt;- c("D","R","I") summary(bgo2$party)

bgo2*r**e**g**i**o**n* &lt;  − *a**s*.*f**a**c**t**o**r*(*b**g**o*2region)

bgo2*r**g**r**o**u**p* &lt;  − *a**s*.*f**a**c**t**o**r*(*b**g**o*2rgroup)

bgo2*t**o**t**c**h**i**f**a**c**t**o**r* &lt;  − *a**s*.*f**a**c**t**o**r*(*b**g**o*2totchi)

\#Select the subset variables for the regressions regtot &lt;- bgo2
%&gt;% select(“genold”, “party”, “totchi”, “region”, “rgroup”, “white”,
“age”, “srvlng”, “servsqrt”, “agesqrt”) reggirl &lt;- bgo2 %&gt;%
select(“genold”,“party”,“ngirls”,“region”,“rgroup”,“white”,“age”,“srvlng”,“servsqrt”,“agesqrt”)

\#Run the regressions & store them to the variable regtot=lm(totchi \~
., regtot) reggirl=lm(ngirls \~ ., reggirl)

\#Save the beta1 coefficient beta1 &lt;-
c(coefficients(reggirl)\[2\],coefficients(regtot)\[2\])
names(beta1)=c(“Number of daughters”,“Number of children”)

\#Run the same regression separately for Democrats and Republicans
bgo2$party %&lt;&gt;% relevel(ref=“D”) regdemtot &lt;- bgo2 %&gt;%
select(“genold”, “party”, “totchi”, “region”, “rgroup”, “white”, “age”,
“srvlng”, “servsqrt”, “agesqrt”) regdemgirl &lt;- bgo2 %&gt;%
select(“genold”,“party”,“ngirls”,“region”,“rgroup”,“white”,“age”,“srvlng”,“servsqrt”,“agesqrt”)
regdemtot=lm(totchi \~ ., regdemtot) regdemgirl=lm(ngirls \~ .,
regdemgirl)

bgo2$party %&lt;&gt;% relevel(ref=“R”,“I”) regRtot &lt;- bgo2 %&gt;%
select(“genold”, “party”, “totchi”, “region”, “rgroup”, “white”, “age”,
“srvlng”, “servsqrt”, “agesqrt”) regRgirl &lt;- bgo2 %&gt;%
select(“genold”,“party”,“ngirls”,“region”,“rgroup”,“white”,“age”,“srvlng”,“servsqrt”,“agesqrt”)
regRtot=lm(totchi \~ ., regRtot) regRgirl=lm(ngirls \~ ., regRgirl)

\#Coefficient & Std error of genold coef1 &lt;-
c(coefficients(regdemgirl)\[2\],coefficients(regdemtot)\[2\])
names(coef1)=c(“Number of daughters D”,“Number of children D”)

se &lt;-
c(summary(regtot)*c**o**e**f*\[\[2, 2\]\], *s**u**m**m**a**r**y*(*r**e**g**g**i**r**l*)coef\[\[2,2\]\],
summary(regdemgirl)*c**o**e**f*\[\[2, 2\]\], *s**u**m**m**a**r**y*(*r**e**g**d**e**m**t**o**t*)coef\[\[2,2\]\],
summary(regRgirl)*c**o**e**f*\[\[2, 2\]\], *s**u**m**m**a**r**y*(*r**e**g**R**t**o**t*)coef\[\[2,2\]\])
