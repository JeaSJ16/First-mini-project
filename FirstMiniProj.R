#Jea Juanillo
#BS in Statistics
#CMSC197
#First Mini Project

#Set the working directory
setwd("D:/Users/Personal Computer/Desktop/CMSC-197/specdata")
#unzip the file
unzip("rprog_data_specdata.zip", exdir = "specdata")
#Set the working directory
setwd("D:/Users/Personal Computer/Desktop/CMSC-197/specdata/specdata")


#Number 1
pollutantmean <- function(directory, pollutant, id = 1:332) {
#The 'directory' is a character vector of length 1 indicating the location of the CSV files.
#The 'pollutant' is a character vector of length 1 indicating the name of  the pollutant for which we will calcultate the mean; either "sulfate" or "nitrate".
#The 'id' is an integer vector indicating the monitor ID numbers to be used.
  
  ## Return the mean of the pollutant across all monitors list
 
  means <- c()#Create an empty vector to store the data 
  
  for(monitor in id){
    path <- paste(getwd(), "/", directory, "/", sprintf("%03d", monitor), ".csv", sep = "")
    monitor_data <- read.csv(path)
    interested_data <- monitor_data[pollutant]
    means <- c(means, interested_data[!is.na(interested_data)])
  }
  
  mean(means)#solve for the mean of the pollutant across all monitors list
}

#Example
pollutantmean("specdata", "sulfate", 1:10)
#[1] 4.064128

#Number 2
  complete <- function(directory, id = 1:332){
#The 'director' is a character vector of length 1 indicating the location of the CSV files.
#The 'id' is an integer vector indicating the monitor ID numbers to be used.
    
    results <- data.frame(id=numeric(0), nobs=numeric(0))
    for(monitor in id){
      path <- paste(getwd(), "/", directory, "/", sprintf("%03d", monitor), ".csv", sep = "")
      monitor_data <- read.csv(path)
      interested_data <- monitor_data[(!is.na(monitor_data$sulfate)), ]
      interested_data <- interested_data[(!is.na(interested_data$nitrate)), ]
      nobs <- nrow(interested_data)
      results <- rbind(results, data.frame(id=monitor, nobs=nobs))
    }
    results
  }
  
#Example
complete("specdata",1)
#id nobs
#1  1  117

#Number 3
corr <- function(directory, threshold = 0){
#The 'directory' is a character vector of length 1 indicating the location of the CSV files.
#The 'threshold' is a numeric vector of length 1 indicating the number of completely observed observations (on all variables) requi?red to compute the correlation between nitrate and sulfate; the default. is 0
      
      cor_results <- numeric(0)
      
      complete_cases <- complete(directory)
      complete_cases <- complete_cases[complete_cases$nobs>=threshold, ]
      
      if(nrow(complete_cases)>0){
        for(monitor in complete_cases$id){
          path <- paste(getwd(), "/", directory, "/", sprintf("%03d", monitor), ".csv", sep = "")
          monitor_data <- read.csv(path)
          interested_data <- monitor_data[(!is.na(monitor_data$sulfate)), ]
          interested_data <- interested_data[(!is.na(interested_data$nitrate)), ]
          sulfate_data <- interested_data["sulfate"]
          nitrate_data <- interested_data["nitrate"]
          cor_results <- c(cor_results, cor(sulfate_data, nitrate_data))
        }
      }
      cor_results
}

#Example
cr<- corr("specdata", 150)
head(cr); summary(cr)
#[1] -0.01895754 -0.14051254 -0.04389737 -0.06815956 -0.12350667 -0.07588814
#Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#-0.21057 -0.05147  0.09333  0.12401  0.26836  0.76313

#Number 4
#Set the working directory
setwd("D:/Users/Personal Computer/Desktop/CMSC-197/specdata")
#unzip the file
unzip("rprog_data_ProgHospData.zip", exdir = "HospData")
#Set the working directory
setwd("D:/Users/Personal Computer/Desktop/CMSC-197/specdata/HospData")

outcome <- read.csv("HospData/outcome-of-care-measures.csv", TRUE, colClasses = "character") 
head(outcome) 

ncol(outcome)   #shows the number of columns
nrow(outcome)   #shows the number of rows
names(outcome)  #shows the names

#Plotting the histogram
outcome[, 11] <- as.numeric(outcome[, 11])
## You may get a warning about NAs being introduced; that is okay
hist(outcome[, 11])
