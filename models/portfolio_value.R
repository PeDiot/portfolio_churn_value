# --------- Overall value of the firm's portfolio ----------

# ---- Method 

# 1. With all customers

  # For each period (T=72): 
    # Retrieve the estimated survival probability 
    # Calculate the updated monthly price using the discount factor 
  # Sum over the N customers who are in the portfolio 

# 2. Using clustering results
  
  # For each K clusters: 
    # Retrieve the most representative individual 
    # Compute her estimated survival 
    # Calculate her updated monthly price using the discount factor 
  # Sum over the K clusters 

# Note: give confidence interval for the expected portfolio value 

# Change in portfolio value after a change in some variables 
  # ex: what portfolio value with 70% month-to-month contract customers?

# ----- Setup -----

dir <- "./models/"
setwd(dir)

data_path <- "C:/Users/pemma/OneDrive - Université de Tours/Mécen/M2/S2/04 - 3R/portfolio_churn_value/data/"
backup <- "C:/Users/pemma/OneDrive - Université de Tours/Mécen/M2/S2/04 - 3R/portfolio_churn_value/backup/"

load(file = paste(data_path, "telco_cleaned.Rdata", sep = ""))
load(file = paste(backup, "survival/cox_final.RData", sep = ""))

library(tidyverse)
library(survival)
library(ggplot2)
library(parallel)
library(doParallel)
library(latex2exp)

theme_set(theme_minimal())

# ----- Requirements -----

custIDs <- cleaned_data %>%
  pull(CustomerID)                            # customer unique IDs

a <- .08                                      # discount factor 

num_months <- 1:72

surv_fit <- survfit(final_cox, 
                    newdata = cleaned_data)   # survfit object to estimate survival

surv <- surv_fit$surv %>%
  as.data.frame()                             # estimated survival probabilities
colnames(surv) <- custIDs

surv_lower <- surv_fit$lower %>%
  as.data.frame()                             # estimated lower bounds
colnames(surv_lower) <- custIDs

surv_upper <- surv_fit$upper %>%
  as.data.frame()                             # estimated upper bounds
colnames(surv_upper) <- custIDs

# ----- Functions -----

estimate_survival <- function(custID){
  
  # Return number of months, conditional survival and confidence interval for a given client.
  
  s <- surv %>%
    dplyr::select(all_of(custID)) %>%
    dplyr::pull(custID)
  l <- surv_lower %>%
    dplyr::select(all_of(custID)) %>%
    dplyr::pull(custID) 
  u <- surv_upper %>%
    dplyr::select(all_of(custID)) %>%
    dplyr::pull(custID)  
  
  data.frame(num_months = num_months, 
             surv_lower = ifelse(is.na(l), 0, l), 
             surv = s, 
             surv_upper = ifelse(is.na(u), 0, u))
}

compute_cust_total_value <- function(surv_dat, price){
  
  # Return customer value over all periods and IC bounds.
  
  surv_dat %>%
    dplyr::mutate( v_lower = price * surv_lower / (1 + a)^num_months, 
            v = price * surv / (1 + a)^num_months,
            v_upper = price * surv_upper / (1 + a)^num_months ) %>%
    dplyr::select(starts_with("v")) %>%
    colSums()
  
}

process <- function(custID){
  
  # Estimate customer survival to then calculate total value and IC bounds.
  
  surv_dat <- estimate_survival(custID) 
  price <- cleaned_data[cleaned_data$CustomerID == custID, ] %>%
    dplyr::pull(Monthly_Charges) 
  compute_cust_total_value(surv_dat, price)
  
}

# ----- Apply process to all customers using parallelization -----


# NOT RUN {
  num_cores <- detectCores() - 1
  cl <- makeCluster(num_cores)
  
  registerDoParallel(cl)  
  clusterExport(cl = cl,
                varlist = list("estimate_survival",
                               "compute_cust_total_value", 
                               "process", 
                               "cleaned_data",
                               "surv",
                               "surv_lower", 
                               "surv_upper",
                               "custIDs", 
                               "a",
                               "num_months",  
                               "%>%"), 
                envir = environment())
  
  system.time(
    results <- c(parLapply(cl,
                           X = custIDs,
                           fun = process))
  )
  
  stopCluster(cl)
  
  custValues <- do.call(rbind, results) %>%
    as.data.frame() %>%
    mutate(CustomerID = custIDs)
  
  save(custValues, file = paste(backup, "custValues.RData", sep = "/"))
  
# } 

# ----- Results -----
  
load(file = paste(backup, "custValues.RData", sep = "/"))

custValues %>%
  View()

portfolio_value <- custValues %>%
  select(starts_with("v")) %>%
  colSums() %>%
  format(big.mark = ",", scientific = F)  # overall value of the firm's portfolio 


subtitle <- TeX(paste("Portfolio value =", 
                  portfolio_value[2],
                  "(IC$_{95\\%} = $", 
                  portfolio_value[1], 
                  ";", 
                  portfolio_value[3], 
                  ")"))

custValues %>%
  ggplot(aes(x = v)) +
  geom_density(size = 1, 
               color = "blue") +
  scale_x_continuous(labels = scales::comma) +
  labs(x = "V", 
       y = "Density", 
       title = "Distribution of customer raw value", 
       subtitle = subtitle) 

