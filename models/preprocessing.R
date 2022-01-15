# --------------------- Data Preprocessing --------------------- 

# NOT RUN {

  dir <- "C:/Users/pemma/Dropbox/M2 EE 2021 2022/COURS/3R/TEAM 2 AB PORTEFEUILLE/04 - MODELISATION"
  setwd(dir)
  
  # ----- packages -----
  library(tidyverse)
  library(readxl)
  library(rsample)
  
  # ----- dataset -----
  
  # load
  data_path <- "C:/Users/pemma/Dropbox/M2 EE 2021 2022/COURS/3R/TEAM 2 AB PORTEFEUILLE/03 - DATA ANALYSIS/datasets"
  data <- read_excel(path = paste(data_path, "Telco_Customer_Churn.xlsx", sep = "/")) %>%
    as.data.frame()
  
  # info
  View(data)
  str(data)
  summary(data, na.rm = T)
  
  # ----- preprocessing -----
  
  # variables selection
  data <- data %>%
    select(-c(
      "Count",
      "Country",     # Country is removed since the only value is 'United States'
      "State",       # State is removed since the only value is 'California'
      "Lat Long"
    ))
  
  # character to numeric
  data$`Total Charges` <- as.numeric(data$`Total Charges`)
  
  # character to factor
  data <- data %>%
    mutate_if(is.character, as.factor) %>%
    mutate( `Zip Code` = as.factor(`Zip Code`) )
  
  # NAs
  na_obs <- which( !( complete.cases(data) ) )
  data[na_obs, ] %>% View()
  
  # change "NA" to "No churn" for "Churn Reason"
  data <- data %>%
    mutate( `Churn Reason` = factor(
      `Churn Reason`, 
      levels = levels(addNA(`Churn Reason`)), 
      labels = c(levels(`Churn Reason`), "No Churn"), 
      exclude = NULL
    ) ) 
  
  # remove obs with NAs in "Total Charges" 
  # Tenure Months = 0 for these obs
  cleaned_data <- data %>%
    filter(!is.na(`Total Charges`))
  
  # remove "(automatic)" from categories of "Payment Method"
  levels(cleaned_data$`Payment Method`)[1:2] <- c("Bank transfer", "Credit card")
  
  # format column names 
  names(cleaned_data) <- gsub(" ", "_", names(cleaned_data))
  
  # save cleaned data
  save(
    cleaned_data,
    file = paste(data_path, "Telco_Cleaned.RData", sep = "/")
  )
  
  
  # ----- train test split -----
  
  data_split <- initial_split(cleaned_data, prop = .7)
  data_train <- training(data_split)
  data_test  <- testing(data_split)
  
  save(
    data_train, data_test, 
    file = paste(data_path, "train_test_data.RData", sep = "/")
  )

# }
