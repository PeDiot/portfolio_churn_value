# ------------------- Build the pseudo-observation data table -------------------  

# Data format: 
  # multiple rows or observations
  # each of which applies to an interval of observation (start, stop]
  # each individual is observed along j(i) follow-up intervals

# ----- paths -----
dir <- "C:/Users/pemma/Dropbox/M2 EE 2021 2022/COURS/3R/TEAM 2 AB PORTEFEUILLE/04 - MODELISATION"
setwd(dir)
data_path <- "C:/Users/pemma/Dropbox/M2 EE 2021 2022/COURS/3R/TEAM 2 AB PORTEFEUILLE/03 - DATA ANALYSIS/datasets"
figs_path <- paste(dir, "figs", sep = "/")

# ----- packages -----
library(tidyverse)
library(survival)

# ----- data -----
load(file = paste(data_path, "train_test_data.RData", sep = "/"))

# ---- function to make the table -----

make_pseudo_obs_table <- function(surv_data, Time, Event){
  
  prog_bar <- txtProgressBar(min = 0, max = 1, style=3)
  result <- NULL
  N <- floor(max(surv_data[, Time]))
  
  for (t in seq(0, N, 1)){
    surv_data_t <- surv_data[surv_data[, Time]>t,]
    surv_data_t$t <- t
    surv_data_t$ER_t <- surv_data_t[, Time]-t
    surv_data_t$ER_t[surv_data_t$ER_t>1] <- 1
    surv_data_t$D <- (surv_data_t[, Time]<=t+1)*(surv_data_t[, Event])
    result <- rbind(result, surv_data_t)
    
    setTxtProgressBar(prog_bar, t/N)
  }
  
  rm(surv_data, surv_data_t, t,N) #tidy up
  close(prog_bar) #tidy up
  result <- result %>%
    arrange(CustomerID)
  
  return(result)
  
}

# ----- apply function -----

data_train <- data_train %>%
  select(-Churn_Label)

make_pseudo_obs_table(
  surv_data = data_train, 
  Time = "Tenure_Months", 
  Event = "Churn_Value"
)

