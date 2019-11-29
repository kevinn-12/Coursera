##Q1
outcome <- read.csv("rprog_data_ProgAssignment3-data/outcome-of-care-measures.csv", colClasses = "character")
head(outcome)
str(outcome)
outcome[,11] <- as.numeric(outcome[,11])
hist(outcome[,11])

##Q2
library(tidyverse)

best <- function(state,outcome){
 data <- read.csv("rprog_data_ProgAssignment3-data/outcome-of-care-measures.csv", colClasses = "character")
 names(data)[names(data)=="Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"] <- "heart failure"
 names(data)[names(data)=="Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"] <- "heart attack"
 names(data)[names(data)=="Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"] <- "pneumonia"
 data$`heart attack` <- suppressWarnings(as.numeric(data$`heart attack`))
 data$`heart failure` <- suppressWarnings(as.numeric(data$`heart failure`))
 data$pneumonia <- suppressWarnings(as.numeric(data$pneumonia))
 int <- c("heart failure", "heart attack", "pneumonia")
 
 if((outcome %in% int)==F){
   stop("Invalid Outcome")
 }
 if((state %in% data$State)==F){
   stop("Invalid State")
 } 
 data <- subset(data,data$State==state)
 outcome <- noquote(outcome)
 data <- select(data,Hospital.Name,outcome)
 data <- data[order(data[,2],data[,1]),] #orders data on lowest heart attack value and then alphabetically on 
                                                              #hospital names (handles ties)
 result <- data[1,1]
 return(result)
}

best("TX", "heart attack") #function check

##Q3

rankhospital <- function(state, outcome, num = "best") {
 data <- read.csv("rprog_data_ProgAssignment3-data/outcome-of-care-measures.csv",colClasses = "character")
 names(data)[names(data)=="Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"] <- "heart failure"
 names(data)[names(data)=="Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"] <- "heart attack"
 names(data)[names(data)=="Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"] <- "pneumonia"
 data$`heart attack` <- suppressWarnings(as.numeric(data$`heart attack`))
 data$`heart failure` <- suppressWarnings(as.numeric(data$`heart failure`))
 data$pneumonia <- suppressWarnings(as.numeric(data$pneumonia))
 int <- c("heart failure", "heart attack", "pneumonia")
 
 if((outcome %in% int)==F){
   stop("Invalid Outcome")
 }
 if((state %in% data$State)==F){
   stop("Invalid State")
 } 
 
 data <- subset(data,data$State==state)
 outcome <- noquote(outcome)
 data <- select(data,Hospital.Name,outcome)
 data <- data[complete.cases(data),]
 if(num == "best") {
   num = 1
 }
 else if(num == "worst") {
   num = nrow(data)
 }
 else if(is.numeric(x=num)) {
   if(num<1 || num > nrow(data)) {
     return(NA)
   }
 }
 else {
   stop('invalid num')
 }
 data <- data[order(data[,2],data[,1]),]
 result <- data[num,1]
 return(result)
}

rankhospital("TX", "heart failure", 4)

##Q4
rankall <- function(outcome, num = "best") {
  data <- read.csv("rprog_data_ProgAssignment3-data/outcome-of-care-measures.csv", colClasses = "character")
  
  if(outcome=="heart failure"){
    names(data)[names(data)=="Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"] <- "outcome"
  } else if(outcome=="heart attack"){
    names(data)[names(data)=="Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"] <- "outcome"
  } else if(outcome=="pneumonia"){
    names(data)[names(data)=="Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"] <- "outcome"
  } else{
    stop("Invalid Outcome")
  }
  
  data$outcome <- suppressWarnings(as.numeric(data$outcome))
  data <- select(data,Hospital.Name,State,outcome)
  data <- data %>%
    group_by(State) %>%
    mutate(my_ranks = order(order(outcome, Hospital.Name, decreasing = F))) 
  
  unique.states <- sort(unique(data$State))
  unique.states <- as.data.frame(unique.states)
  names(unique.states)[names(unique.states)=="unique.states"] <- "State"
  
  if(num=="best"){
    data <- data %>%
      group_by(State) %>%
      na.omit() %>%
      filter(my_ranks == min(my_ranks)) 
  } else if(num=="worst"){
    data <- data %>%
      group_by(State) %>%
      na.omit() %>%
      filter(my_ranks == max(my_ranks)) 
  } else{
    data <- data %>%
      group_by(State) %>%
      filter(my_ranks == num)
  }
  data <- merge(data,unique.states,by="State", all.y = T)
  return(data)
}

