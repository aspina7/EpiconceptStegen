# Stegen case study 
# Example of solution R-script
# EpiConcept
# February 2012
# Crude analysis
# Esther Kissling - Gilles DESVE
# R-code written by Alexander Spina September 2018



#### Reading in files #### 

load("stegen1.Rda")

#### Q5 ####

#### Q5a: attack rates ####

#There are several ways to do this. 
#The first involves using base-R code, 
#the second is with a user-written function 
#and the third is to install a package. 


#### Version 1: base-R


# The first element will be rows and the 2nd will be columns
count <- table(tira.data$sex, tira.data$ill, deparse.level = 2)

# Here we select row % of count by including ,1 in the prop.table section
prop <- round(prop.table(count, 1) * 100, digits = 2)

# We obtain the denominator using the rowSums function
denominator <- rowSums(count)[2]

# We combine all the elements together using cbind (binding by columns)
output <- cbind(Ill = count[2, ], N = denominator, Proportions = prop[2, ])



#### Version 2: user-written function 


# Function to provide counts, denominator and proportions (equivalent of attack rate)
attack.rate <- function(exposure, outcome, data, rowcol = "cols") {
  
  #create an empty list to store results
  output <- list()
  
  #for each variable named in exposure
  for (var in exposure) {
    
    counts <- table(data[, var], data[, outcome] )
    
    if (rowcol == "cols") {
      
      
      #get column proportions
      prop <- round(prop.table(counts, 1) * 100, digits = 2)
      
      #get row totals 
      denominator <- rowSums(counts)[2]
      
      
      #pull counts together 
      intermediate <- cbind(Ill = counts[2, ], N = denominator, Proportions = prop[2, ])
      
    }
    
    if (rowcol == "rows") {
      
      #get column proportions
      prop <- round(prop.table(counts, 2) * 100, digits = 2)
      
      #get column totals
      denominator <- colSums(counts)[2]
      
      #pull counts together 
      intermediate <- cbind(Exposed = counts[ , 2], 
                            N = denominator, Proportions = prop[ , 2])
    }
    
    if (nrow(counts) > 2) {
      
      #get column proportions
      prop <- round(prop.table(counts, 1) * 100, digits = 2)
      
      #get row totals 
      denominator <- rowSums(counts)
      
      
      #pull counts together 
      intermediate <- cbind(Ill = counts[ , 2], N = denominator, Proportions = prop[ , 2])
      
    }
    
    #store your output table in the list
    output[[var]] <- intermediate
  }
  
  return(output)
}

# specify the exposure, the outcome and the dataset
attack.rate( exposure = "sex", outcome = "ill", data = tira.data)

#### Version 3: install package 

# Install the package if you have not done this yet
install.packages("EpiStats")

# Load the package to this session 
library(EpiStats)

# Use the CS function as this is a cohort study 
CS(tira.data, "ill", "sex")


#### Q5c: proportion of cases exposed #### 


# specify rows to get proportion of exposed cases
attack.rate(exposure = c("sex", "tira", "agegroup"), 
            outcome = "ill", data = tira.data, 
            rowcol = "rows")

#reversed cs gives you one count
CS(tira.data, "tira", "ill")



#### Q5d: Compute attributable risk % (or preventive fraction) among exposed ####

#same command as above for CS but in a loop

#define vars 
vars <- c("agegroup", "sex", "tira", "wmousse", "dmousse", "mousse", 
          "beer", "redjelly", "fruitsalad", "tomato", "mince", 
          "salmon", "horseradish", "chickenwin", "roastbeef", "pork")
  
for (var in vars) {
  print(var)
  print(CS(tira.data, var, "ill"))
}

#### Q5e: Search for any dose response if appropriate ####

# Recode 3 portions of tportion as 2 portions
# Make a new variable called tportion2 that has the same values as tportion
tira.data$tportion2 <- tira.data$tportion
tira.data$tportion2[tira.data$tportion2 == "Three portions"] <- "Two portions"

#drop the resulting NA factor level
tira.data$tportion2 <- droplevels(tira.data$tportion2, NA)

# Tabulate tportion2 variable against illness using attack.rate function
attack.rate(exposure = "tportion2", 
            outcome = "ill", 
            data = tira.data)

#### Q5f: Interpret the results and identify the outbreak vehicle if any ####


CSTable(tira.data, cases = "ill", exposure = c("sex", "agegroup", "tira",
                                               "beer", "mousse", "wmousse",
                                               "dmousse", "redjelly", "fruitsalad",
                                               "tomato", "mince", "salmon", "horseradish",
                                               "chickenwin", "roastbeef", "pork"))


