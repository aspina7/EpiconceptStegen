# Stegen case study 
# Example of solution R-script
# EpiConcept
# February 2012
# Recoding crude dataset
# Esther Kissling - Gilles DESVE
# R-code written by Alexander Spina September 2018



#### Reading in files #### 

tira.data <- read.csv("stegen.csv", sep = ";", stringsAsFactors = FALSE )


##### 2 different age groups: we create a new variables to compare them ####

#create a binary variable for older than 30 years of age
tira.data$agegroup <- ifelse(tira.data$age >= 30, 1, 0 )

#create labels for the agegroup variable 
tira.data$agegroup <- factor(tira.data$agegroup, 
                             levels = c(0, 1), 
                             labels = c("0-29", 
                                        "30+"))

#check the age grouping 
table(tira.data$agegroup)


#### some variables have missing coded as "9" - recoding these to ".": #### 

#for the rows where salmon is 9, overwrite with NA
tira.data$salmon[tira.data$salmon == 9] <- NA

#same for horseradish and pork
tira.data$horseradish[tira.data$horseradish == 9] <- NA
tira.data$pork[tira.data$pork == 9] <- NA



#### adding labels #### 

#for sex 

tira.data$sex <- factor(tira.data$sex, 
                        levels = c(0, 1), 
                        labels = c("female", "male"))




#define the variables you would like to recode
vars  <- c("tira", "wmousse", "dmousse", "ill", 
           "mousse", "beer", "redjelly", 
           "fruitsalad", "tomato", "mince", 
           "salmon", "horseradish", "chickenwin", 
           "roastbeef", "pork") 

#for each var defined in vars above
for (var in vars) {
  #select the column of tira.data in square brackets
  #overwrite with a factor as above
  tira.data[ , var] <- factor(tira.data[ , var], 
                              levels = c(0, 1), 
                              labels = c("No", "Yes")
  )
}





#define the variables you would like to recode
vars  <- c("tportion", "mportion") 

#for each var defined in vars above
for (var in vars) {
  #select the column of tira.data in square brackets
  #overwrite with a factor as above
  tira.data[ , var] <- factor(tira.data[ , var], 
                              levels = c(0, 1, 2, 3), 
                              labels = c("None", "One portion", 
                                         "Two portions", "Three portions")
  )
}



#### Fix dates #### 


# change missing to be NA 
tira.data$dateonset[tira.data$dateonset == ""] <- NA

# change dateonset to a date character 
tira.data$dateonset <- as.Date(tira.data$dateonset, format = "%d.%m.%Y")


#### Rearranging the order of variables #### 

tira.data <- tira.data[ , c("uniquekey", "dateonset", "ill", 
                            names(tira.data)[c(3:11, 13:ncol(tira.data))]
                            )
                        ]

#### saving the newly recoded dataset #### 

save(tira.data, file = "stegen1.Rda")

