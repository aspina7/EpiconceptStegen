# Stegen case study 
# Example of solution R-script
# EpiConcept
# February 2012
# Descriptive analysis
# Esther Kissling - Gilles DESVE
# R-code written by Alexander Spina September 2018


#### Reading in files #### 

load("stegen1.Rda")


#### Person #### 

# specify the variable in quotations and the dataset to use
big.table(var = "sex", data = tira.data)


# summary of age 
summary(tira.data$age)

# summary of age by sex 
# use the aggregate function to group by sex
# sex must be as a list
# specify the function you would like to use (summary)
aggregate(tira.data$age, by = list(tira.data$sex), FUN = summary)


#Plot a histogram of age
#you can specify a bar for each age with "breaks"
#you can set your x axis from 0-100 using "xlim"
hist(tira.data$age, 
     xlab = "Age",
     ylab = "Count", 
     breaks = 100,
     xlim = c(0, 100)
)


# save histogram of age as a png file
dev.copy(png,'age.png')
dev.off()


#### 2 different age groups: ####


big.table("agegroup", tira.data)
big.table("ill", tira.data) 
big.table("dateonset", tiradata[tira.data$ill == 1]) 

#### epidemic curve #### 

# Load the epiconcept epicurve function
library(EpiCurve) 

# subset data so only have ill and non missing

tester <- tira.data[which(tira.data$ill == "Yes" &
                            !is.na(tira.data$dateonset)), ]

#use epicurve function to plot daily 
EpiCurve(x = tester, 
         date = "dateonset", 
         period = "day", 
         xlabel = sprintf("From %s to %s", 
                          min(tester$dateonset, na.rm = T), 
                          max(tester$dateonset, na.rm = T))
         )

