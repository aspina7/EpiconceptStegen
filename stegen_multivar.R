# Stegen case study 
# Example of solution R-script
# EpiConcept
# February 2012
# Multivariable analysis
# Esther Kissling - Gilles DESVE
# R-code written by Alexander Spina September 2018



#### Reading in files #### 

load("stegen1.Rda")


#### Question 7 ####

# Create logit regression model with tira as exposure variable

model1 <- glm(ill~tira,
              data = tira.data,
              family = binomial(link = "logit"))

# Gives an overview of key elements of the model
summary(model1)

# install broom if you have not already done so
install.packages("broom")

#load broom to this session 
library(broom)

# Obtaining the key output of the regression model including ORs and CIs
model1op <- tidy(model1, exponentiate = TRUE, conf.int = TRUE)

# view your cleaned output
model1op




# We add beer to the model
model2 <- glm(ill ~ tira + beer,
              data = tira.data,
              family = binomial(link = "logit"))

# clean up your output and exponentiate
model2op <- tidy(model2, exponentiate = TRUE, conf.int = TRUE)

# view your cleaned output
model2op




# Here we use tportion a factor variable
model3 <- glm(ill~tportion, 
              data = tira.data,
              family = binomial(link = "logit"))

# clean up your output and exponentiate
model3op <- tidy(model3, exponentiate = TRUE, conf.int = TRUE)

# view your cleaned output
model3op




# Update the previous model with a new formula
model4 <- update(model2,
                 formula = ill ~ tira + beer + mousse)

#clean and exponentiate
model4op <- tidy(model4, exponentiate = TRUE, conf.int = TRUE)

#view
model4op



#### Question 8 - optional #### 


# Binomial regression with one independent variable
#specify link as log rather than logit
bin1 <- glm(ill ~ tira, 
            data = tira.data,
            family = binomial(link = "log"))

#clean output and exponentiate
bin1op <- tidy(bin1, exponentiate = TRUE, conf.int = TRUE)

#view 
bin1op 



#### Question 9 - optional ####

#use non missing dataset 
load("stegen_nomissing.Rda")



# Only one independent variable
model5 <- glm(ill~tira,
              data = tira.data,
              family = binomial(link = "logit"))

# As before, we can update the previous model and just write the new formula
model6 <- update(model5, 
                 formula = ill ~ tira + beer)

anova(model5, model6, test = "Chisq")


#### Question 10 - optional #### 

#Automated selection of best fit based on AIC

#define variables
vars <- c("sex", "tira", "age", 
          "dmousse", "wmousse", "beer", 
          "fruitsalad", "redjelly", "tportion", 
          "mportion", "salmon", "mince", 
          "tomato", "horseradish", "chickenwin", 
          "roastbeef", "pork")

#put variables in a formula for inputting to regression
form <- formula(paste0("ill ~ ",paste0(vars, collapse = "+")))


#stepwise regression to find best based on AIC
bestmodel <- step(glm(form, data = tira.data,
                      family = binomial(link = "logit")))


#clean the output of a the best fit model
final <- tidy(bestmodel, exponentiate = TRUE, conf.int = TRUE) 

#### additional ####


# need to figure out simplest way for predict()



#### Question 11 - optional #####


#load your non missing dataset 
load("stegen_nomissing.Rda")


# Check for interaction between tira and beer
tirabeer <- glm(ill ~ tira*beer, 
                data = tira.data,
                family = binomial(link = "logit"))

tirabeerop <- tidy(tirabeer, exponentiate = TRUE, conf.int = TRUE)
tirabeerop


# Stratified by tira 

#We can use names to extract the coefficient names 
# check with: names(coef(tirabeer)) 

# linfct specifies the required combination:
#In this case we want beer and tira and beer:tira=0
# The odds of illness among those who 
#drank beer and consumed tiramisu compared to those
#who consumed neither tiramisu nor beer

a <- summary(glht(tirabeer, linfct = c("beerYes + tiraYes:beerYes = 0")))

ci <- confint(a) 

# Put together (cbind) a table with the exponent of the coefficients and CI, and p-value
table_interact <- round(cbind(OR = exp(coef(a)),
                              Interval = exp(ci$confint),
                              Pvalue = a$test$pvalues),
                        digits = 3)

table_interact



#Stratified by beer 

a <- summary(glht(tirabeer, linfct = c("tiraYes + tiraYes:beerYes = 0")))

ci <- confint(a) 

# Put together (cbind) a table with the exponent of the coefficients and CI, and p-value
table_interact <- round(cbind(OR = exp(coef(a)),
                              Interval = exp(ci$confint),
                              Pvalue = a$test$pvalues),
                        digits = 3)

table_interact