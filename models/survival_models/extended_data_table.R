# ------------------- Build the extended survival data table -------------------  

# Data format: 
  # multiple rows or observations
  # each of which applies to an interval of observation (start, stop]
  # each individual is observed along j(i) follow-up intervals

# ----- paths -----
dir <- "./models/"
setwd(dir)
data_path <- "C:/Users/pemma/OneDrive - Université de Tours/Mécen/M2/S2/04 - 3R/portfolio_churn_value/data"

# ----- packages -----
library(tidyverse)
library(survival)

# ----- data -----
load(file = paste(data_path, "train_test_data.RData", sep = "/"))

# ---- function to make the table -----

augment_data_table <- function(surv_data, Time, Event){
  
  prog_bar <- txtProgressBar(min = 0, max = 1, style = 3)
  result <- NULL
  N <- floor(max(surv_data[, Time]))
  
  for (t in seq(0, N-1, 1)){
    surv_data_t <- surv_data[surv_data[, Time]>t, ]
    surv_data_t$t <- t
    surv_data_t$ER_t <- surv_data_t[, Time] - t
    surv_data_t$ER_t[surv_data_t$ER_t>1] <- 1
    surv_data_t$D <- (surv_data_t[, Time]<=t+1)*(surv_data_t[, Event])
    result <- rbind(result, surv_data_t)
    
    setTxtProgressBar(prog_bar, t/N)
  }
  
  rm(surv_data, surv_data_t, t, N) 
  close(prog_bar) 
  
  result %>%
    arrange(CustomerID)
  
}

# ----- apply function -----

data_train <- data_train %>%
  select(-c(
    Churn_Label, 
    Churn_Reason, 
    Churn_Score
  ))

extended_data_train <- augment_data_table(
  surv_data = data_train, 
  Time = "Tenure_Months", 
  Event = "Churn_Value"
)

