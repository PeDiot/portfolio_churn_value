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

theme_new <- function() {
  theme_minimal() %+replace%
    theme(axis.title = element_text(size = 14), 
          axis.text = element_text(size = 14), 
          title = element_text(size = 14), 
          legend.position = "bottom", 
          legend.title = element_blank(), 
          legend.text = element_text(size = 14))
}
theme_set(theme_new())

source(paste0(dir, "colors.R"))


custIDs <- cleaned_data %>%
  pull(CustomerID) %>%
  as.character()                              # customer unique IDs

a <- .08                                      # discount factor

num_months <- 1:72

surv_fit <- survfit(final_cox,
                    newdata = cleaned_data)   # survfit object to estimate survival


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
    dplyr::select(starts_with("v")) # %>% colSums()
  
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
          res <- compute_survival(surv_fit)
          surv_lower <- res$surv_lower ; surv <- res$surv ; surv_upper <- res$surv_upper
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
            mutate( CustomerID = lapply(custIDs,
                                        function(id){ rep(id, max(num_months)) }) %>%
                      unlist() %>%
                      as.factor() )
          
          file_path <- paste0(backup, "custValues/custValues_", a*100, "pct", ".RData") ; file_path
          save(custValues, file = file_path)
  
#         } 


# 

# First results -----------------------------------------------------------

file_path <- paste0(backup, "custvalues/", "custValues_8pct", ".RData")
load(file = file_path)

dim(custValues)

summary(custValues$v) 

custValuesTot <- custValues %>%
  group_by(CustomerID) %>%
  summarise_at(vars(starts_with("v")), 
               sum) 

portfolio_value <- custValuesTot %>%
  select(starts_with("v")) %>%
  colSums() %>%
  format(big.mark = ",", scientific = F)  ; portfolio_value

subtitle <- TeX(paste("Portfolio value =", 
                  portfolio_value[2],
                  "(IC$_{95\\%} = $", 
                  portfolio_value[1], 
                  ";", 
                  portfolio_value[3], 
                  ")"))

## Across all customers -----------------------------------------------------------

custValuesTot %>%
  ggplot(aes(x = v)) +
  geom_density(size = 1.5, 
               color = portfol_col) +
  geom_vline(aes(xintercept = custValuesTot %>%
                   pull(v) %>%
                   mean(),
                 color = "Average CLRV"), 
             linetype = "dotted", 
             size = 1) +
  scale_x_continuous(labels = scales::comma) +
  scale_color_manual(values = c("black")) +
  labs(x = "CLRV", 
       y = "Density", 
       title = "") +
  theme_new()

df_plot <- custValues %>%
  group_by(CustomerID) %>%
  mutate(num_months = num_months) %>%
  group_by(num_months) %>%
  summarise_at(vars(starts_with("v")), 
               mean) %>%
  mutate(v_cum = cumsum(v)) 

coef <- 50

df_plot %>%
  ggplot() +
  geom_line(aes(x = num_months, 
                y = v), 
            size = 1,linetype = "dotted",
            color = "black") +
  geom_line(aes(x = num_months, 
                y = v_cum / coef), 
            size = 1.5, 
            color = portfol_col) +
  scale_y_continuous(name = "Monthly Value", 
                     sec.axis = sec_axis(~.*coef, 
                                         name = "Cumulative Monthly Value")) + 
  labs(x = "Number of months", 
       y = "", 
       title = "") +
  theme_new() +
  theme(axis.title.y.right = element_text(color = portfol_col), 
        axis.text.y.right = element_text(color = portfol_col))

df_plot %>%
  ggplot() +
  geom_segment(aes(x = num_months, xend = num_months,
                   y = 0, yend = v), 
               size = 1,
               color = "grey80") +
  geom_line(aes(x = num_months, 
                y = v_cum / coef), 
            size = 1.5, 
            color = portfol_col) +
  scale_y_continuous(name = "Monthly Contribution", 
                     sec.axis = sec_axis(~.*coef, 
                                         name = "Cumulative Value")) + 
  labs(x = "Number of months", 
       y = "", 
       title = "") +
  theme_new() +
  theme(axis.title.y.right = element_text(color = portfol_col), 
        axis.text.y.right = element_text(color = portfol_col),
        axis.title.y.left = element_text(color = "grey50"),
        axis.text.y.left = element_text(color = "grey50"))


## Accross clusters -----------------------------------------------------------

clusters <- res.hcpc$data.clust$clust
custValues_clust <- custValues %>%
  mutate(cluster = lapply(clusters, 
                          function(x){ rep(x, max(num_months)) }) %>%
           unlist())

save(custValues_clust, 
     file = paste0(backup, "custvalues/custValues_clust.RData")) 

custValues_clust %>%
  group_by(cluster) %>%
  summarise_at(vars(starts_with("v")), 
               sum)

custValues_clust %>%
  group_by(cluster, CustomerID) %>%
  summarise_at(vars(starts_with("v")), 
               sum) %>%
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
       title = "") +
  theme_new()


custValues_clust %>%
  group_by(cluster, CustomerID) %>%
  mutate(num_months = num_months) %>%
  group_by(cluster, num_months) %>%
  summarise_at(vars(starts_with("v")), 
               mean) %>%
  group_by(cluster) %>%
  mutate(v_cum = cumsum(v)) %>% 
  ggplot() +
    geom_line(aes(x = num_months, 
                  y = v_cum, 
                  color = cluster), 
              size = 1.5) +
    scale_color_jco() +
    labs(x = "Number of months", 
         y = "Cumulative CLRV", 
         title = "") +
    theme_new()



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
  group_by(discount, CustomerID) %>%
  summarise_at(vars(starts_with("v")), 
               sum) %>%
  ggplot(aes(x = v, 
             color = discount)) +
  geom_density(size = 1.3) +
  scale_x_continuous(labels = scales::comma) +
  scale_color_brewer(palette = "Set2") +
  labs(x = "CLRV", 
       y = "Density", 
       title = "") +
  theme_new()

custValues %>%
  group_by(discount, CustomerID) %>%
  mutate(num_months = num_months) %>%
  group_by(discount, num_months) %>%
  summarise_at(vars(starts_with("v")), 
               mean) %>%
  group_by(discount) %>%
  mutate(v_cum = cumsum(v)) %>% 
  ggplot() +
  geom_line(aes(x = num_months, 
                y = v_cum, 
                color = discount), 
            size = 1.5) +
  scale_color_brewer(palette = "Set2") +
  labs(x = "Number of months", 
       y = "Cumulative CLRV", 
       title = "") +
  theme_new()

## Based on type of contract -------------------------------------------------------------

# NOT RUN {

          N <- nrow(cleaned_data) 
          p_ref <- sum(cleaned_data %>% 
                        pull(Contract) == "Month-to-month") / N ; p_ref
          p <- .8
          cust_subset <- sample(x = cleaned_data %>%
                                  filter(Contract != "Month-to-month") %>%
                                  pull(CustomerID),
                                size = (p - p_ref) * N)
          # one_year_cust <- sample(x = cust_subset, 
          #                         size = .5 * length(cust_subset))
          
          dat <- cleaned_data %>%
            mutate(Contract = if_else(
              condition = CustomerID %in% cust_subset,
              true = "Month-to-month", 
              false = Contract %>%
                as.character()
            )  %>% as.factor()) 
          
          questionr::freq(dat$Contract)
          
          surv_fit <- compute_survival(surv_fit = survfit(final_cox,
                                                          newdata = dat))
          surv_lower <- surv_fit$surv_lower ; surv <- surv_fit$surv ; surv_upper <- surv_fit$surv_upper
          
          num_cores <- detectCores() - 1
          cl <- makeCluster(num_cores)
          
          registerDoParallel(cl)  
          clusterExport(cl = cl,
                        varlist = list("estimate_survival",
                                       "compute_cust_total_value", 
                                       "get_monthly_discount_factor", 
                                       "process", 
                                       "dat",
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
                                   dat = dat))
          )
          
          stopCluster(cl)
          
          month_to_month_80 <- do.call(rbind, results) %>%
            as.data.frame() %>% 
            mutate( CustomerID = lapply(custIDs,
                                        function(id){ rep(id, max(num_months)) }) %>%
                      unlist() %>%
                      as.factor() ) ; nrow(month_to_month_80) / max(num_months)
          
          file_path <- paste0(backup, "custValues/month_to_month_80.RData") ; file_path
          save(month_to_month_80, file = file_path)

#         }
          

load(file = paste0(backup, "custvalues/", "custValues_8pct", ".RData"))
custValuesref <- custValues

x <- 80
load(file = paste0(backup, 
                   "custValues/month_to_month_", 
                   x, 
                   ".RData"))

custValues <- rbind(custValuesref %>%
                      mutate(month_to_month_pct = "ref"), 
                    month_to_month_10 %>%
                      mutate(month_to_month_pct = "10"),
                    month_to_month_30 %>%
                      mutate(month_to_month_pct = "30"),
                    month_to_month_70 %>%
                      mutate(month_to_month_pct = "70"),
                    month_to_month_90 %>%
                      mutate(month_to_month_pct = "90"))

custVal_month_to_month <- custValues %>%
  mutate(month_to_month_pct = month_to_month_pct %>%
           as.factor() %>%
           recode_factor(`10` = "10%", 
                         `30` = "30%", 
                         `70` = "70%", 
                         `90` = "90%", 
                         `ref` = "55.1% (reference)")) %>%
  group_by(month_to_month_pct) %>%
  summarise_at(vars(starts_with("v")), 
               sum) %>%
  rename(`% 'month-to-month' contracts` = month_to_month_pct,
         `V lower` = v_lower, 
         V = v, 
         `V upper` = v_upper) %>%
  mutate_at(vars(starts_with("v")), 
            format, 
            big.mark = ",")

save(custVal_month_to_month, 
     file = paste0(backup, "custValues/custVal_month_to_month.RData"))

custValues %>%
  filter(month_to_month_pct != "ref") %>%
  mutate(month_to_month_pct = month_to_month_pct %>%
           as.factor() %>%
           recode_factor(`10` = "10%", 
                         `30` = "30%", 
                         `70` = "70%", 
                         `90` = "90%")) %>%
  group_by(month_to_month_pct, CustomerID) %>%
  summarise_at(vars(starts_with("v")), 
               sum) %>%
  ggplot(aes(x = v, 
             y = ..density.., 
             fill = month_to_month_pct)) +
  geom_histogram(bins = 30, 
                 alpha = .5, 
                 color = "white") +
  geom_density(aes(x = v, 
                   color = month_to_month_pct), 
               inherit.aes = F, 
               size = 1) +
  labs(x = "CLRV", 
       y  = "Density") +
  scale_fill_brewer(palette = "Set2") +
  scale_color_brewer(palette = "Set2") +
  facet_wrap(~month_to_month_pct, 
             ncol = 2, 
             nrow = 2) +
  theme(strip.text = element_text(size = 14), 
        legend.position = "none")

custValues <- rbind(month_to_month_10 %>%
                      mutate(month_to_month_pct = "10"),
                    month_to_month_20 %>%
                      mutate(month_to_month_pct = "20"),
                    month_to_month_30 %>%
                      mutate(month_to_month_pct = "30"),
                    month_to_month_40 %>%
                      mutate(month_to_month_pct = "40"),
                    month_to_month_50 %>%
                      mutate(month_to_month_pct = "50"),
                    month_to_month_60 %>%
                      mutate(month_to_month_pct = "60"),
                    month_to_month_70 %>%
                      mutate(month_to_month_pct = "70"),
                    month_to_month_80 %>%
                      mutate(month_to_month_pct = "80"),
                    month_to_month_90 %>%
                      mutate(month_to_month_pct = "90")) ; nrow(custValues) / 9 / 72

custValues %>%
  group_by(month_to_month_pct) %>%
  summarise_at(vars(starts_with("v")), sum) %>%
  mutate(month_to_month_pct = month_to_month_pct %>%
           as.numeric()) %>%
  ggplot(aes(x = month_to_month_pct, 
             y = v)) +
  geom_point(size = 3, 
             color = portfol_col) +
  geom_line(size = 1.5, 
            color = portfol_col) +
  labs(x = "% 'month-to-month' contracts", 
       y = "Customer Raw Equity") +
  theme_new()
  
## Based on monthly charges -------------------------------------------------------------

# NOT RUN {
          
          chargesVar <- -1
  
          dat <- cleaned_data %>%
            mutate(Monthly_Charges = Monthly_Charges * (1 + chargesVar))
          
          ( mean(dat$Monthly_Charges) / mean(cleaned_data$Monthly_Charges) ) - 1
          
          surv_fit <- compute_survival(surv_fit = survfit(final_cox,
                                                          newdata = dat))
          surv_lower <- surv_fit$surv_lower ; surv <- surv_fit$surv ; surv_upper <- surv_fit$surv_upper
          
          num_cores <- detectCores() - 1
          cl <- makeCluster(num_cores)
          
          registerDoParallel(cl)  
          clusterExport(cl = cl,
                        varlist = list("estimate_survival",
                                       "compute_cust_total_value", 
                                       "get_monthly_discount_factor", 
                                       "process", 
                                       "dat",
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
                                   dat = dat))
          )
          
          stopCluster(cl)
          
          monthly_charges_neg100pct <- do.call(rbind, results) %>%
            as.data.frame() %>% 
            mutate( CustomerID = lapply(custIDs,
                                        function(id){ rep(id, max(num_months)) }) %>%
                      unlist() %>%
                      as.factor() ) ; nrow(monthly_charges_neg100pct) / max(num_months)
          
          file_path <- paste0(backup, "custValues/monthly_charges_neg100pct.RData") ; file_path
          save(monthly_charges_neg100pct, file = file_path)

#         }

### simulate multiple price changes -------------------------------------------------------------
          
dir_ <- paste0(backup, "custValues/")
files <- list.files(path = dir_,
                    pattern = "monthly_charges")
for (file in files){ load(file = paste0(dir, file)) }  

custValues <- rbind(custValuesref %>%
                      mutate(chargesVar_pct = "ref"), 
                    monthly_charges_10pct %>%
                      mutate(chargesVar_pct = "10"),
                    monthly_charges_30pct %>%
                      mutate(chargesVar_pct = "30"),
                    monthly_charges_50pct %>%
                      mutate(chargesVar_pct = "50"),
                    monthly_charges_70pct %>%
                      mutate(chargesVar_pct = "70"),
                    monthly_charges_100pct %>%
                      mutate(chargesVar_pct = "100"),
                    monthly_charges_neg10pct %>%
                      mutate(chargesVar_pct = "-10"),
                    monthly_charges_neg30pct %>%
                      mutate(chargesVar_pct = "-30"),
                    monthly_charges_neg50pct %>%
                      mutate(chargesVar_pct = "-50"),
                    monthly_charges_neg70pct %>%
                      mutate(chargesVar_pct = "-70"))

nrow(custValues) / length(files) / 72

totVal_monthly_charges <- custValues %>%
  select(c(chargesVar_pct, 
           v)) %>%
  group_by(chargesVar_pct) %>%
  summarise_at(vars(starts_with("v")), 
               sum) 

totValref <- totVal_monthly_charges %>%
  filter(chargesVar_pct == "ref") %>%
  pull(v)

totVal_monthly_charges <- totVal_monthly_charges %>%
  mutate( diff_pct = paste(round(100 * (v - totValref) / totValref, 1), 
                           "%") ) %>%
  mutate(v = format(v, big.mark = ",")) %>%
  dplyr::rename(`% Charges Variation` = chargesVar_pct, 
         V = v, 
         `% Value Difference` = diff_pct) ; totVal_monthly_charges

save(totVal_monthly_charges, 
     file = paste0(backup, "totVal_monthly_charges.RData"))  

### add more services to cluster 2 customers -------------------------------------------------------------

# NOT RUN {

          clusters <- res.hcpc$data.clust$clust

          dat <- cleaned_data %>%
            mutate(cluster = clusters)
          
          dat[(
            dat$cluster == 2 & 
              dat$Internet_Service != "No"
            ), ] <- dat[(
              dat$cluster == 2 & 
                dat$Internet_Service != "No"
            ), ] %>%
            mutate_at(vars(Online_Security:Streaming_Movies), 
                      function(var){ "Yes" }) 
          
          surv_fit <- compute_survival(surv_fit = survfit(final_cox,
                                                          newdata = dat))
          surv_lower <- surv_fit$surv_lower ; surv <- surv_fit$surv ; surv_upper <- surv_fit$surv_upper
          
          num_cores <- detectCores() - 1
          cl <- makeCluster(num_cores)
          
          registerDoParallel(cl)  
          clusterExport(cl = cl,
                        varlist = list("estimate_survival",
                                       "compute_cust_total_value", 
                                       "get_monthly_discount_factor", 
                                       "process", 
                                       "dat",
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
                                   dat = dat))
          )
          
          stopCluster(cl)
          
          clust2_with_options <- do.call(rbind, results) %>%
            as.data.frame() %>% 
            mutate( CustomerID = lapply(custIDs,
                                        function(id){ rep(id, max(num_months)) }) %>%
                      unlist() %>%
                      as.factor() ) ; nrow(clust2_with_options) / max(num_months)
          
          file_path <- paste0(backup, "custValues/clust2_with_options.RData") ; file_path
          save(clust2_with_options, file = file_path)

#         }

load(file = paste0(backup, "custValues/clust2_with_options.RData"))
load(file = paste0(backup, "custvalues/custValues_clust.RData"))



tab_clust2_portVal <- rbind(
  custValues_clust %>%
    group_by(cluster) %>%
    summarise_at(vars(starts_with("v")), 
                 sum) %>%
    filter(cluster == 2), 
  clust2_with_options %>%
    mutate(cluster = lapply(clusters, 
                            function(x){ rep(x, max(num_months)) }) %>%
             unlist()) %>%
    group_by(cluster) %>%
    summarise_at(vars(starts_with("v")), 
                 sum) %>%
    filter(cluster == 2) 
) %>%
  select(v) %>%
  as.data.frame()

v_ref <- tab_clust2_portVal[1, "v"]
tab_clust2_portVal <- tab_clust2_portVal %>%
  mutate(pct_diff = 100 * (v - v_ref) / v_ref) %>%
  rename(V = v, 
         `% Variation` = pct_diff) %>%
  mutate(V = format(V, big.mark = ","))

rownames(tab_clust2_portVal) <- c("Reference", 
                                  "With additional options") ; tab_clust2_portVal   

save(tab_clust2_portVal,
     file = paste0(backup, "custValues/tab_clust2_portVal.RData"))

rbind(
  custValues_clust %>%
    mutate(cluster = lapply(clusters, 
                            function(x){ rep(x, max(num_months)) }) %>%
             unlist()) %>%
    filter(cluster == 2) %>%
    group_by(CustomerID) %>%
    summarise_at(vars(starts_with("v")), 
              sum) %>%
    mutate(with_more_options = "No"), 
  clust2_with_options %>%
    mutate(cluster = lapply(clusters, 
                            function(x){ rep(x, max(num_months)) }) %>%
             unlist()) %>%
    filter(cluster == 2) %>%
    group_by(CustomerID) %>%
    summarise_at(vars(starts_with("v")), 
              sum) %>%
    mutate(with_more_options = "Yes")
) %>%
  as.data.frame() %>%
  mutate(with_more_options = as.factor(with_more_options)) %>%
  ggplot(aes(x = v, 
             color = with_more_options)) +
  geom_density(size = 1.5) +
  scale_color_brewer(palette = "Set2") +
  labs(x = "CLRV", 
       y = "Density") +
  guides(color = guide_legend(title = "Additional options")) +
  theme(legend.title = element_text(size = 14))
  