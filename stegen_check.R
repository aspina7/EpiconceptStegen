# Stegen case study 
# Example of solution R-script
# EpiConcept
# February 2012
# Data checking
# Esther Kissling - Gilles DESVE
# R-code written by Alexander Spina September 2018

#### Setting your working directory ####

#Check your current working directory
getwd()

# set your working directory
setwd("C:/Users/Username/Desktop/EpiconceptStegen")


#### Reading in files #### 

tira.data <- read.csv("stegen.csv", sep = ";", stringsAsFactors = FALSE )


#### Defining functions #### 

# Function to make tables with counts, proportions and cumulative sum
big.table <- function(vars, data, useNA = "no") {
  # Create an empty list to hold the output of your loop
  output <- list() 
  
  # Apply big.table to each element of the object in vars. 
  #In this loop, "var" is the indexing variable; any character can be used e.g. "i"
  for (var in vars) {
    # Within the [], 
    # the item before the comma refers to rows 
    # the item after the comma refers to columns
    count <- table(data[ , var], useNA = useNA)
    prop <- round(prop.table(count)*100, digits = 2)
    cumulative <- cumsum(prop)
    total <- t(rbind(count,
                     prop,
                     cumulative))
    # assign the value of your tables (total) to the output list 
    #(note: double square brackets "[[]]" are used to subset elements of a list)
    output[[var]] <- total
  }
  
  output
  
}



#### Describe your dataset ####

# str provides an overview of the number of observations and variable types
str(tira.data)

# summary provides mean, median and max values of your variables
summary(tira.data)


## get counts table of sex 

#get counts 
#save table as "counts"
counts <- table(tira.data$sex) 

#get proportions for counts table
prop.table(counts)

#you could also multiple by 100 and round to 2 digits
round(prop.table(counts)*100, digits = 2)


# Alternative table for counts of sex 
  # specify the variable in quotations and the dataset to use
big.table(var = "sex", data = tira.data)



#### Time ####

# change missing to be NA 
tira.data$dateonset[tira.data$dateonset == ""] <- NA

# change dateonset to a date character 
tira.data$dateonset <- as.Date(tira.data$dateonset, format = "%d.%m.%Y")

# get counts of dates 
big.table("dateonset", tira.data)


# plot histogram of notification date
  # choose days and frequency
hist(tira.data$dateonset,
     breaks = "days",
     freq = TRUE, 
     xlab = "Onset date",
     ylab = "Count"
)

#save histogram of onsetdate as a png file
dev.copy(png,'onsetdate.png')
dev.off()

#### Food items #### 

big.table(c( names(tira.data)[5:21]), tira.data)


#### Person #### 

# summary of age 
summary(tira.data$age)

# summary of age by sex 
  # use the aggregate function to group by sex
  # sex must be as a list
  # specify the function you would like to use (summary)
aggregate(tira.data$age, by = list(tira.data$sex), FUN = summary)

#proportions and 95%CI for ill
prop.test( table(tira.data$ill) )


#Plot a histogram of age
#you can specify a bar for each age with "breaks"
#you can set your x axis from 0-100 using "xlim"
hist(tira.data$age, 
     xlab = "Age",
     ylab = "Count", 
     breaks = 100,
     xlim = c(0, 100)
)
