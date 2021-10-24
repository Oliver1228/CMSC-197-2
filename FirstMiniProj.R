#Item No. 1
pollutantmean <- function(directory, pollutant, id = 1:332){
  
  #store all means across all csv files
  mean_all <- c()
  
  #reading all csv files and store in the variable all_files
  all_files <- list.files(path=directory, pattern = ".csv")
  
  #loop from 1 to n based on user input
  for(i in id) {
    
    current_file <- read.csv(all_files[i])
    
    #store all values with specified pollutant in the mean_all variable
    mean_all <- c(mean_all, current_file[[pollutant]])
    
  }
  #getting the mean of all values excluding the NA values
  mean(mean_all,na.rm=T)
  
}

## In running this function, the command used in the console is
## If first time running, type source('FirstMiniProj.R')
## then pollutantmean("/home/oliver1228/Desktop/specdata/", 'sulfate', 1:10)

#====================================================================

#Item No. 2
complete <- function(directory, id = 1:332){
  
  # store the Number of observations
  nobs <- c()
  
  #reading all csv files and store in the variable all_files
  all_files <- list.files(path=directory, pattern = ".csv")
  
  for(i in id) {
    current_file <- read.csv(all_files[i])
    
    #sum of the complete cases in the current file
    cases_list <- sum(complete.cases(current_file))
    
    #concatenate in the nobs variables all complete observations
    nobs <- c(nobs,cases_list)
    
  }
  #display in a data frame format
  data.frame(id,nobs)
  
}
## In running this function, the command used in the console is
## If first time running, type source('FirstMiniProj.R')
## then complete("/home/oliver1228/Desktop/specdata/", 1)

#====================================================================

#Item No. 3
corr <- function(directory, threshold = 0){
  
  #store the correlations
  vect_corr <- c()
  
  all_files <- list.files(path=directory, pattern = ".csv")
  files <- complete(directory)
  
  #check all files and store those with correlation greater than the threshold in num
  num <- files[files['vect_corr'] > threshold, ]$id
  
  for(i in num) {
    current_file <- read.csv(all_files[i])
    
    #check the cases of the current file
    dataf <- current_file[complete.cases(current_file),]
    
    #store the value of the correlation between sulfate and nitrare to vect_corr
    vect_corr <- c(vect_corr, cor(dataf$sulfate,dataf$nitrate))
  }
  #output all correlations
  return(vect_corr)
}

## In running this function, the command used in the console is
## If first time running, type source('FirstMiniProj.R')
## then cr <- corr("/home/oliver1228/Desktop/specdata/", 150)
## lastly, head(cr); summary(cr)

#====================================================================

#Item No. 4
outcome <- read.csv('outcome-of-care-measures.csv', colClasses = "character")
outcome[,11] <- as.numeric(outcome[,11])

#output the histogram with the label on top as well as the design required
hist(outcome[,11],main="Hospital 30-Day Death(Mortality) Rates from Heart Attack", xlab = 'Deaths', col = 'skyblue')

#In running this one, just click the Run button on the upper right of the editor

