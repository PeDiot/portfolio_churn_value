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

# ----- Requirements -----

custIDs <- cleaned_data %>%
  pull(CustomerID) %>%
  as.character()                              # customer unique IDs

a <- .01                                      # discount factor

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
  
  file_path <- paste0(backup, "custValues/custValues_", a*100, "pct", ".RData")
  save(custValues, file = file_path)
  
# } 


# ----- Results -----
  
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

# accross all customers ---

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


# accross clusters ---

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

# ----- Simulations -----

# based on discount factor ---

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

totVal <- custValues %>%
  group_by(discount) %>%
  summarise_at(vars(v_lower, v, v_upper), 
               sum) ; totVal

custValues %>% 
  split(.$discount) %>%
  purrr::map(summary)

custValues %>%
  ggplot(aes(x = v, 
             color = discount)) +
  geom_density(size = 1) +
  scale_x_continuous(labels = scales::comma) +
  scale_color_brewer(palette = "Set2") +
  labs(x = "", 
       y = "Density", 
       title = "Density plot per discount factor") +
  theme(axis.title = element_text(size = 14), 
        axis.text = element_text(size = 14), 
        title = element_text(size = 14), 
        legend.position = "bottom", 
        legend.title = element_blank())

custValues %>%
  ggplot(aes(x = v, 
             y = ..density.., 
             fill = discount)) +
  geom_histogram(size = 1, 
                 color = "white", 
                 alpha = .5, 
                 bins = 30) +
  geom_density(aes(x = v, 
                   color = discount), 
               inherit.aes = F) +
  scale_x_continuous(labels = scales::comma) +
  scale_fill_brewer(palette = "Set2") +
  scale_color_brewer(palette = "Set2") +
  facet_wrap(~discount, 
             nrow = 2, 
             ncol = 2) +
  labs(x = "", 
       y = "Count", 
       title = "Histogram plot per discount factor") +
  theme(axis.title = element_text(size = 14), 
        axis.text = element_text(size = 14), 
        title = element_text(size = 14), 
        strip.text = element_text(size = 14),
        legend.position = "none")


