# Method ------------------------------------------------------------------


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


# Setup -------------------------------------------------------------------

setwd("./models/")

dir <- "C:/Users/pemma/OneDrive - Université de Tours/Mécen/M2/S2/04 - 3R/portfolio_churn_value/"

data_path <- paste0(dir, "data/")
backup <- paste0(dir, "backup/")

load(file = paste(data_path, "telco_cleaned.Rdata", sep = ""))
load(file = paste(backup, "survival/cox_final.RData", sep = ""))
load(file = paste0(backup, "clustering/res.hcpc.RData")) 

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


custIDs <- cleaned_data %>%
  pull(CustomerID) %>%
  as.character()                              # customer unique IDs

a <- .08                                      # discount factor

num_months <- 1:72

surv_fit <- survfit(final_cox,
                    newdata = cleaned_data)   # survfit object to estimate survival

# 

# Functions ---------------------------------------------------------------

compute_survival <- function(surv_fit){
  "Return estimated survival function with confidence interval."
  
  surv <- surv_fit$surv %>%
    as.data.frame()                             
  colnames(surv) <- custIDs
  
  surv_lower <- surv_fit$lower %>%
    as.data.frame() %>% 
    mutate(across(everything(), 
                  .fns = ~replace_na(.,0)))     
  colnames(surv_lower) <- custIDs
  
  surv_upper <- surv_fit$upper %>%
    as.data.frame() %>% 
    mutate(across(everything(), 
                  .fns = ~replace_na(.,0)))      
  colnames(surv_upper) <- custIDs
  
  return(list("surv_lower" = surv_lower, 
              "surv" = surv, 
              "surv_upper" = surv_upper))
  
}

res <- compute_survival(surv_fit)
surv_lower <- res$surv_lower ; surv <- res$surv ; surv_upper <- res$surv_upper

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

process <- function(custID, dat){
  "Estimate customer survival to then calculate total value and IC bounds."
  
  surv_dat <- estimate_survival(custID) 
  price <- dat[dat$CustomerID == custID, ] %>%
    dplyr::pull(Monthly_Charges) 
  compute_cust_total_value(surv_dat, price)
  
}


# Apply process using parallelization -------------------------------------

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
                           fun = process, 
                           dat = cleaned_data))
  )
  
  stopCluster(cl)
  
  custValues <- do.call(rbind, results) %>%
    as.data.frame() %>%
    mutate(CustomerID = custIDs)
  
  file_path <- paste0(backup, "custValues/custValues_", a*100, "pct", ".RData")
  save(custValues, file = file_path)
  
# } 


# 

# First results -----------------------------------------------------------

file_path <- paste0(backup, "custvalues/", "custValues_8pct", ".RData")
load(file = file_path)

custValues %>%
  View()

summary(custValues$v) 

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

## Across all customers -----------------------------------------------------------

ggarrange(
  custValues %>%
    ggplot(aes(x = v)) +
    geom_density(size = 1.5, 
                 color = portfol_col) +
    scale_x_continuous(labels = scales::comma) +
    labs(x = "", 
         y = "Density", 
         title = "Density plot", 
         subtitle = subtitle) +
    theme(axis.title = element_text(size = 14), 
          axis.text = element_text(size = 14), 
          title = element_text(size = 14)), 
  custValues %>%
    mutate(num_months = cleaned_data$Tenure_Months) %>%
    group_by(num_months) %>%
    summarise(v_lower = mean(v_lower), 
              v = mean(v), 
              v_upper = mean(v_upper)) %>%
    ggplot() +
    geom_line(aes(x = num_months, 
                  y = v), 
              color = portfol_col, 
              size = 1.5) +
    geom_ribbon(aes(x = num_months, 
                    ymin = v_lower, 
                    ymax = v_upper),
                fill = "grey", 
                alpha = .5) +
    labs(x = "Number of months", 
         y = "", 
         title = "Evolution through time") +
    theme(axis.title = element_text(size = 14), 
          axis.text = element_text(size = 14), 
          title = element_text(size = 14)), 
  ncol = 2
)


# Accross clusters -----------------------------------------------------------

custValues_clust <- custValues %>%
  mutate(cluster = res.hcpc$data.clust$clust)

save(custValues_clust, 
     file = paste0(backup, "custvalues/", "custValues_clust", ".RData"))

custValues_clust %>%
  group_by(cluster) %>%
  summarise_at(vars(starts_with("v")), 
               sum)

ggarrange(
  custValues_clust %>%
    ggplot(aes(x = v, 
               y = ..density.., 
               fill = cluster)) +
    geom_histogram(size = 1, 
                   alpha = .5, 
                   bins = 30) +
    scale_x_continuous(labels = scales::comma) +
    scale_fill_jco() +
    labs(x = "CLRV", 
         y = "Density", 
         title = "Density plot per cluster") +
    theme(legend.position = "bottom", 
          legend.title = element_blank(), 
          title = element_text(size = 14), 
          axis.title = element_text(size = 14), 
          axis.text = element_text(size = 14),
          legend.text = element_text(size = 14)), 
  
  custValues_clust %>%
    mutate(num_months = cleaned_data$Tenure_Months) %>%
    group_by(cluster, num_months) %>%
    summarise(v_lower = mean(v_lower), 
              v = mean(v), 
              v_upper = mean(v_upper)) %>%
    ggplot() +
    geom_line(aes(x = num_months, 
                  y = v, 
                  color = cluster),
              size = 1.5) +
    scale_color_jco() +
    labs(x = "Number of months", 
         y = "", 
         title = "Evolution through time per cluster") +
    theme(legend.position = "bottom", 
          legend.title = element_blank(), 
          title = element_text(size = 14), 
          axis.title = element_text(size = 14), 
          axis.text = element_text(size = 14),
          legend.text = element_text(size = 14)),
  ncol = 2, 
  common.legend = T, 
  legend = "bottom"
)


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
       title = "Distribution of monthly charges per cluster") 



# Simulations -------------------------------------------------------------

## Based on discount factor -------------------------------------------------------------

load(file = paste0(backup, "custvalues/", "custValues_8pct", ".RData"))
custValues8 <- custValues

load(file = paste0(backup, "custvalues/", "custValues_4pct", ".RData"))
custValues4 <- custValues

load(file = paste0(backup, "custvalues/", "custValues_2pct", ".RData"))
custValues2 <- custValues

load(file = paste0(backup, "custvalues/", "custValues_2pct", ".RData"))
custValues2 <- custValues

load(file = paste0(backup, "custvalues/", "custValues_1pct", ".RData"))
custValues1 <- custValues

custValues <- rbind(custValues1 %>%
                      mutate(discount = "1%"), 
                    custValues2 %>%
                      mutate(discount = "2%"), 
                    custValues4 %>%
                      mutate(discount = "4%"), 
                    custValues8 %>%
                      mutate(discount = "8%")) %>% 
  mutate(discount = as.factor(discount)) 

portVal_discount_rate <- custValues %>%
  group_by(discount) %>%
  summarise_at(vars(v_lower, v, v_upper), 
               sum) %>%
  mutate_at(vars(starts_with("v")), 
            function(var){ format(var, big.mark = ",") }) ; portVal_discount_rate 

save(portVal_discount_rate, 
     file = paste0(backup, "portVal_discount_rate.RData"))

custValues %>% 
  split(.$discount) %>%
  purrr::map(summary)

custValues %>%
  ggplot(aes(x = v, 
             color = discount)) +
  geom_density(size = 1.3) +
  scale_x_continuous(labels = scales::comma) +
  scale_color_brewer(palette = "Set2") +
  labs(x = "CLRV", 
       y = "Density", 
       title = "") +
  theme(axis.title = element_text(size = 14), 
        axis.text = element_text(size = 14), 
        title = element_text(size = 14), 
        legend.position = "bottom", 
        legend.title = element_blank(), 
        legend.text = element_text(size = 14))

## Based on type of contract -------------------------------------------------------------

# NOT RUN {
          N <- nrow(cleaned_data)
          
          month_to_month_90 <- cleaned_data %>%
            mutate(Contract = if_else(
              condition = CustomerID %in% sample(x = custIDs %>% as.numeric(),
                                                 size = .7*N), 
              true = "Month-to-month", 
              false = as.character(Contract)
            ))
          
          surv_fit <- compute_survival(surv_fit = survfit(final_cox,
                                                          newdata = month_to_month_90))
          surv_lower <- res$surv_lower ; surv <- res$surv ; surv_upper <- res$surv_upper
          
          num_cores <- detectCores() - 1
          cl <- makeCluster(num_cores)
          
          registerDoParallel(cl)  
          clusterExport(cl = cl,
                        varlist = list("estimate_survival",
                                       "compute_cust_total_value", 
                                       "get_monthly_discount_factor", 
                                       "process", 
                                       "month_to_month_90",
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
                                   fun = process, 
                                   dat = month_to_month_90))
          )
          
          stopCluster(cl)
          
          custValues_simul2 <- do.call(rbind, results) %>%
            as.data.frame() %>%
            mutate(CustomerID = custIDs)
          
          file_path <- paste0(backup, "custValues/month_to_month_90.RData")
          save(custValues_simul2, file = file_path)

#         }
