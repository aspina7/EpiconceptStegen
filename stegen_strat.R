# Stegen case study 
# Example of solution R-script
# EpiConcept
# February 2012
# Stratified analysis
# Esther Kissling - Gilles DESVE
# R-code written by Alexander Spina September 2018


#### Reading in files #### 

load("stegen1.Rda")


#### Q6a: tabulate #####

# do a three way table with tira
# drop NAs specifying useNA = "no" 
# put variable names using deparse.level = 2
table(tira.data$beer, tira.data$ill, 
      tira.data$tira, useNA = "no",
      deparse.level = 2)


#### Q6c: summarise results: ####

# specify CSInter as otherwise, but also choose a by group
CSInter(tira.data, cases = "ill", exposure = "beer", by = "tira")


### Q6e: stratify other vars by tira ####

#define vars 
vars <- c("wmousse", "dmousse", "redjelly", 
          "fruitsalad", "agegroup", "sex")

# for each defined variable
# print the name on screen 
# print a table
for (var in vars) {
  print(var)
  print(CSInter(tira.data, cases = "ill", exposure = var, by = "tira"))
}



# Recode 3 portions of tportion as 2 portions
# Make a new variable called tportion2 that has the same values as tportion
tira.data$tportion2 <- tira.data$tportion
tira.data$tportion2[tira.data$tportion2 == "Three portions"] <- "Two portions"

#drop the resulting NA factor level
tira.data$tportion2 <- droplevels(tira.data$tportion2, NA)

#stratify by tportion2
CSInter(tira.data, cases = "ill", exposure = "beer", by = "tportion2")



