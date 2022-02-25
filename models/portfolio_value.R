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

setwd("./models/")

dir <- "C:/Users/pemma/OneDrive - Université de Tours/Mécen/M2/S2/04 - 3R/portfolio_churn_value/"

data_path <- paste0(dir, "data/")
backup <- paste0(dir, "backup/")

load(file = paste(data_path, "telco_cleaned.Rdata", sep = ""))
load(file = paste(backup, "survival/cox_final.RData", sep = ""))

library(tidyverse)
library(survival)
library(ggplot2)
library(ggsci)
library(survminer)
library(parallel)
library(doParallel)
library(latex2exp)

theme_set(theme_minimal())

source(paste0(dir, "colors.R"))

# ----- Requirements -----

custIDs <- cleaned_data %>%
  pull(CustomerID) %>%
  as.character()                              # customer unique IDs

a <- .08                                      # discount factor

num_months <- 1:72

surv_fit <- survfit(final_cox, 
                    newdata = cleaned_data)   # survfit object to estimate survival

surv <- surv_fit$surv %>%
  as.data.frame()                             # estimated survival probabilities
colnames(surv) <- custIDs

surv_lower <- surv_fit$lower %>%
  as.data.frame() %>% 
  mutate(across(everything(), 
                .fns = ~replace_na(.,0)))     # estimated lower bounds
colnames(surv_lower) <- custIDs

surv_upper <- surv_fit$upper %>%
  as.data.frame() %>% 
  mutate(across(everything(), 
                .fns = ~replace_na(.,0)))      # estimated upper bounds
colnames(surv_upper) <- custIDs


# ----- Predictions relatively to the raw hazard / survival -----

risk_preds <- predict(final_cox, 
                    newdata = cleaned_data, 
                    type = "risk", 
                    se.fit = T) %>%
  as.data.frame() %>%
  mutate(fit_lower = fit - 1.96*se.fit, 
       fit_upper = fit + 1.96*se.fit) %>%
  mutate(CustomerID = rownames(.), 
         num_months = cleaned_data$Tenure_Months)

surv_preds <- predict(final_cox, 
                      newdata = cleaned_data, 
                      se.fit = T, 
                      type = "survival") %>%
  as.data.frame() %>%
  mutate(fit_lower = fit - 1.96*se.fit, 
         fit_upper = fit + 1.96*se.fit) %>%
  mutate(CustomerID = rownames(.), 
         num_months = cleaned_data$Tenure_Months) # one

# ----- Functions -----

get_monthly_discount_factor <- function(a){
  
  res <- (1+a)**(1/12) - 1
  return(res)
  
}

estimate_survival <- function(custID){
  "Return number of months, conditional survival and confidence interval for a given client."
  
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
  "Return customer value over all periods and IC bounds."
  
  a <- get_monthly_discount_factor(a)
  
  surv_dat %>%
    dplyr::mutate( v_lower = price * surv_lower / (1 + a)^num_months, 
            v = price * surv / (1 + a)^num_months,
            v_upper = price * surv_upper / (1 + a)^num_months ) %>%
    dplyr::select(starts_with("v")) %>%
    colSums()
  
}

process <- function(custID){
  "Estimate customer survival to then calculate total value and IC bounds."
  
  surv_dat <- estimate_survival(custID) 
  price <- cleaned_data[cleaned_data$CustomerID == custID, ] %>%
    dplyr::pull(Monthly_Charges) 
  compute_cust_total_value(surv_dat, price)
  
}

# ----- Apply process using parallelization -----


# NOT RUN {
  num_cores <- detectCores() - 1
  cl <- makeCluster(num_cores)
  
  registerDoParallel(cl)  
  clusterExport(cl = cl,
                varlist = list("estimate_survival",
                               "compute_cust_total_value", 
                               "get_monthly_discount_factor", 
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
  
  file_path <- paste0(backup, "custValues_", a*100, "pct", ".RData")
  save(custValues, file = file_path)
  
# } 


# ----- Results -----
  
file_path <- paste0(backup, "custValues_", a*100, "pct", ".RData")
load(file = file_path)

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

# accross all customers ---

ggpubr::ggarrange(
  custValues %>%
    ggplot(aes(x = v)) +
    geom_density(size = 1, 
                 color = portfol_col) +
    scale_x_continuous(labels = scales::comma) +
    labs(x = "V", 
         y = "Density", 
         title = "Distribution of customer lifetime raw value", 
         subtitle = subtitle), 
  cleaned_data %>%
    ggplot(aes(x = Monthly_Charges)) +
      geom_density(size = 1, 
                color = monthly_charges_col) +
    labs(x = "Monthly Charges", 
         y = "Density", 
         title = "Distribution of monthly charges", 
         subtitle = "Customer raw value"), 
  risk_preds %>%
    group_by(num_months) %>%
    summarise(fit = mean(fit), 
              fit_lower = mean(fit_lower), 
              fit_upper = mean(fit_upper)) %>%
    ggplot() +
      geom_step(aes(x = num_months, 
                    y = fit), 
                size = 1, 
                color = risk_col) +
    geom_ribbon(aes(x = num_months, 
                    ymin = fit_lower, 
                    ymax = fit_upper), 
                fill = "grey", 
                alpha = .7) +
    labs(x = "Number of months", 
         y = "Hazard", 
         title = "Customer churn given number of months", 
         subtitle = "with 95% confidence interval"), 
  surv_preds %>%
    group_by(num_months) %>%
    summarise(fit = mean(fit), 
              fit_lower = mean(fit_lower), 
              fit_upper = mean(fit_upper)) %>%
    ggplot() +
    geom_step(aes(x = num_months, 
                  y = fit), 
              size = 1, 
              color = surv_col) +
    geom_ribbon(aes(x = num_months, 
                    ymin = fit_lower, 
                    ymax = fit_upper), 
                fill = "grey", 
                alpha = .4) +
    labs(x = "Number of months", 
         y = "Survival", 
         title = "Customer survival given number of months", 
         subtitle = "with 95% confidence interval"),
  ncol = 2, nrow = 2
)


# accross clusters ---

load(file = paste0(backup, "clustering/res.hcpc.RData"))

ggarrange(
  custValues %>%
    mutate(cluster = res.hcpc$data.clust$clust) %>%
    ggplot(aes(x = v, 
               y = ..density.., 
                 fill = cluster)) +
    geom_histogram(size = 1, 
                   alpha = .6, 
                   bins = 30) +
    scale_x_continuous(labels = scales::comma) +
    scale_fill_jco() +
    labs(x = "V", 
         y = "Density", 
         title = "Distribution of customer lifetime raw value per cluster"), 
  cleaned_data %>%
    mutate(cluster = res.hcpc$data.clust$clust) %>%
    ggplot(aes(x = Monthly_Charges, 
               y = ..density.., 
               fill = cluster))  +
    geom_histogram(size = 1, 
                   bins = 30, 
                   alpha = .6) +
    scale_fill_jco() +
    labs(x = "Monthly Charges", 
         y = "Density", 
         title = "Distribution of monthly charges per cluster", 
         subtitle = "Customer raw value"), 
  risk_preds %>%
    mutate(cluster = res.hcpc$data.clust$clust) %>%
    group_by(cluster, num_months) %>%
    summarise(fit = mean(fit), 
              fit_lower = mean(fit_lower), 
              fit_upper = mean(fit_upper)) %>%
    ggplot() +
    geom_step(aes(x = num_months, 
                  y = fit, 
                  color = cluster), 
              size = .8) +
    scale_color_jco() +
    labs(x = "Number of months", 
         y = "Hazard", 
         title = "Customer churn per cluster given number of months"), 
  surv_preds %>%
    mutate(cluster = res.hcpc$data.clust$clust) %>%
    group_by(cluster, num_months) %>%
    summarise(fit = mean(fit), 
              fit_lower = mean(fit_lower), 
              fit_upper = mean(fit_upper)) %>%
    ggplot() +
    geom_step(aes(x = num_months, 
                  y = fit, 
                  color = cluster), 
              size = .8) +
    geom_ribbon(aes(x = num_months, 
                    ymin = fit_lower, 
                    ymax = fit_upper, 
                    fill = cluster), 
                alpha = .4) +
    scale_color_jco() +
    scale_fill_jco() +
    labs(x = "Number of months", 
         y = "Survival", 
         title = "Customer survival per cluster given number of months", 
         subtitle = "with 95% confidence interval"),
  ncol = 2, nrow = 2, 
  common.legend = T, 
  legend = "bottom"
)
