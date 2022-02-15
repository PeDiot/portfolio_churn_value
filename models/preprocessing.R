# --------------------- Data preprocessing --------------------- 

# NOT RUN {

  dir <- "./models/"
  setwd(dir)
  
  # ----- packages -----
  library(tidyverse)
  library(readxl)
  library(rsample)
  
  # ----- dataset -----
  
  # load
  data_path <- "C:/Users/pemma/OneDrive - Université de Tours/Mécen/M2/S2/04 - 3R/portfolio_churn_value/data/"
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
  
  # NAs
  na_obs <- which( !( complete.cases(data) ) )
  data[na_obs, ] %>% View()
  
  # change "NA" to "No churn" for "Churn Reason"
  data <- data %>%
    mutate( `Churn Reason` = as.factor(`Churn Reason` ) ) %>%
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
  
  # format column names 
  names(cleaned_data) <- gsub(" ", "_", names(cleaned_data))
  
  # change 'No internet service' to 'No'
  cleaned_data <- cleaned_data %>%
    mutate(across(
      Online_Security:Streaming_Movies, 
      ~ifelse(.x == "No internet service", "No", .x)
    ))
  
  # change 'No phone service' to 'No'
  cleaned_data <- cleaned_data %>%
    mutate(Multiple_Lines = ifelse(Multiple_Lines == "No phone service", "No", "Yes"))
  
  # character to factor
  cleaned_data <- cleaned_data %>%
    mutate_if(is.character, as.factor) %>%
    mutate(Zip_Code = as.factor(Zip_Code))
  
  # remove "(automatic)" from categories of "Payment Method"
  levels(cleaned_data$Payment_Method)[1:2] <- c("Bank transfer", "Credit card")
  
  # change customer IDs
  regexp <- "[[:digit:]]+"
  cleaned_data <- cleaned_data %>%
    mutate(CustomerID = str_extract(CustomerID, regexp))
  
  # save cleaned data
  save(
    cleaned_data,
    file = paste(data_path, "Telco_Cleaned.RData", sep = "/")
  )
  
  
  # ----- train test split -----
  
  data_split <- initial_split(cleaned_data, prop = .7)
  data_train <- training(data_split)
  rownames(data_train) <- 1:nrow(data_train) 
  data_test  <- testing(data_split)
  rownames(data_test) <- 1:nrow(data_test) 
  
  save(
    data_train, data_test, 
    file = paste(data_path, "train_test_data.RData", sep = "/")
  )

# }
