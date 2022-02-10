# --------- Overall value of the firm's portfolio ----------

# ---- Method 

# 1. With all customers

  # For each period (T=72): 
    # Retrieve the estimated survival probability 
    # Calculate the updated CLV using the discount factor 
  # Sum over the N customers who are in the portfolio 

# 2. Using clustering results
  
  # For each K clusters: 
    # Retrieve the most representative individual 
    # Compute her estimated survival 
    # Calculate her updated CLV using the discount factor 
  # Sum over the K clusters 

# Note: give confidence interval for the expected portfolio value 

# ----- Setup 

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

# ----- Requirements 

surv_fit <- survfit(cox_final, 
                    newdata = cleaned_data)   # survfit object to estimate survival

custIDs <- cleaned_data %>%
  pull(CustomerID) %>%
  as.character()                              # customer unique IDs

a <- .08                                      # discount factor 


# ----- Functions

estimate_survival <- function(custID){
  
  # Return number of months, conditional survival and confidence interval for a given client.
  
  surv <- surv_fit$surv %>%
    as.data.frame() %>%
    dplyr::select(all_of(custID))  
  num_months <- as.numeric(rownames(surv))
  surv_lower <- surv_fit$lower %>%
    as.data.frame() %>%
    dplyr::pull(custID) 
  surv_upper <- surv_fit$upper %>%
    as.data.frame() %>%
    dplyr::pull(custID) 
  surv <- surv %>%
    dplyr::pull(custID) 
  
  data.frame(
    num_months = num_months, 
    surv_lower = ifelse(is.na(surv_lower), 0, surv_lower), 
    surv = surv, 
    surv_upper = ifelse(is.na(surv_upper), 0, surv_upper)
  )
}

compute_cust_total_value <- function(surv_dat, clv){
  
  # Return customer value over all periods and IC bounds.
  
  surv_dat %>%
    dplyr::mutate( v_lower = clv * surv_lower / (1 + a)^num_months, 
            v = clv * surv / (1 + a)^num_months,
            v_upper = clv * surv_upper / (1 + a)^num_months ) %>%
    dplyr::select(starts_with("v")) %>%
    colSums()
  
}

process <- function(custID){
  
  # Estimate customer survival to then calculate total value and IC bounds.
  
  surv_dat <- estimate_survival(custID) 
  clv <- cleaned_data[as.numeric(custID), ]%>%
    dplyr::pull(CLTV) 
  compute_cust_total_value(surv_dat, clv)
  
}

# ----- Apply process to all customers using parallelization 


# NOT RUN {
  num_cores <- detectCores() - 1
  cl <- makeCluster(num_cores)
  
  registerDoParallel(cl)  
  clusterExport(cl = cl,
                varlist = list("estimate_survival",
                               "compute_cust_total_value", 
                               "process", 
                               "cleaned_data",
                               "surv_fit", 
                               "custIDs", 
                               "a",
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
    rownames_to_column(var = "CustomerID")
  
  save(custValues, file = paste(backup, "custValues.RData", sep = "/"))
  
# } 
  
load(file = paste(backup, "custValues.RData", sep = "/"))

custValues %>%
  mutate(CLTV = cleaned_data$CLTV, 
         diff = (v - cleaned_data$CLTV) / cleaned_data$CLTV) %>%
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
  labs(x = "", 
       y = "Density", 
       title = "Distribution of customer overall value", 
       subtitle = subtitle) 

