# Jea S. Juanillo

#CMSC 197 - MINI PROJECT

#Number 1.
#Note: I downloaded the file from the LMS
unzip("rprog_data_specdata.zip", exdir = "specdata")

pollutantMean <- function(directory, pollutant, id)	{             #creates a list of file
  specfiles <- list.files(directory, full.names=TRUE)	
  new_vec <- vector(mode = "list", length = length(specfiles))    #create an empty data frame
  for (i in seq_along(specfiles)) {
    new_vec[[i]] <- read.csv(specfiles[[i]])
  }
  output <- do.call(rbind, new_vec)                               #binds the new data frame
  
  if(pollutant=="sulfate"){                                       #if else separate Sulfate and nitrate
    dat_subset <- output[which(output[, "ID"] == id),]              #subsets the rows that match the 'id' argument for sulfate	           
    mean(dat_subset[, "sulfate"], na.rm=TRUE)	                      #identifies mean sulfate while stripping out NAs
  }else{
    dat_subset <- output[which(output[, "ID"] == id),]	          #subsets the rows that match the 'id' argument for nitrate
    mean(dat_subset[, "nitrate"], na.rm=TRUE)                     #identifies mean sulfate while stripping out NAs	
  }
}
#Example output: 
pollutantMean("specdata/specdata", "nitrate", 23)
#[1] 1.280833

#Number 2.

#Number 3.

#Number 4.
#Note: I downloaded the file from the LMS
unzip("rprog_data_ProgHospData.zip", exdir = "HospData")

outcome <- read.csv("HospData/outcome-of-care-measures.csv", TRUE, colClasses = "character") 
head(outcome) 

ncol(outcome)   #shows the number of columns
nrow(outcome)   #shows the number of rows
names(outcome)  #shows the names

#Plotting the histogram
outcome[, 11] <- as.numeric(outcome[, 11])
## You may get a warning about NAs being introduced; that is okay
hist(outcome[, 11])
