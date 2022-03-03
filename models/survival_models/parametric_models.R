# Doc -----
  # https://www.ms.uky.edu/~mai/Rsurv.pdf
  # https://www.rdocumentation.org/packages/survival/versions/3.2-7/topics/predict.survreg
  # https://www.r-bloggers.com/2019/06/parametric-survival-modeling/


# Exponential model
# Weibull model

# SET UP ----------

## paths -----
dir <- "./models/survival_models/"
setwd(dir)
data_path <- "C:/Users/pemma/OneDrive - Université de Tours/Mécen/M2/S2/04 - 3R/portfolio_churn_value/data/"
backup_path <- "C:/Users/pemma/OneDrive - Université de Tours/Mécen/M2/S2/04 - 3R/portfolio_churn_value/backup/survival/"

## packages -----
library(tidyverse)
library(data.table)
library(ggplot2)
library(tidyr)

library(survival)
library(flexsurv)     # for parametric survival models 
library(survminer)  
library(survcomp)     # for concordance index
library(muhaz)        # for kernel density estimator
library(lmtest)       # for LR test 

theme_set(theme_minimal())

## Cox model -----
load(paste0(backup_path, "cox_final.RData"))

## data -----
load(file = paste0(data_path, "telco_cleaned.RData"))
load(file = paste0(data_path, "train_test_data.RData"))

## maximum number of months -----

max_num_months <- cleaned_data %>%
  pull(Tenure_Months) %>%
  max()

## functions -----

predict_hazard <- function(mod, new_dat){
  "Return predicted hazard function for each individual for a given parametric model."
  
  preds <- predict(mod, 
                   newdata = new_dat, 
                   se.fit = T, 
                   conf.int = T, 
                   type = "hazard", 
                   times = 1:max_num_months)
  
  custIDs <- lapply(new_dat$CustomerID, 
                   function(id){ rep(id, max_num_months) }) %>%
    unlist()
  
  preds %>%
    unnest(cols = .pred) %>%
    mutate(CustomerID = custIDs)
  
}

predict_survival <- function(mod, new_dat){
  "Return predicted survival function for each individual for a given parametric model."
  
  preds <- predict(mod, 
                   newdata = new_dat, 
                   se.fit = T, 
                   conf.int = T, 
                   type = "survival", 
                   times = 1:max_num_months)
  
  custIDs <- lapply(new_dat$CustomerID, 
                   function(id){ rep(id, max_num_months) }) %>%
    unlist()
  
  preds %>%
    unnest(cols = .pred) %>%
    mutate(CustomerID = custIDs)
  
}

# INTERCEPT ONLY MODELS ----------

## kernel density estimation -----

survdata <- data.table(data_train)
head(survdata)

kernel_haz_est <- muhaz(
  times=survdata$Tenure_Months,
  delta=survdata$Churn_Value, 
  min.time = 0, 
  max.time = 72
)
kernel_haz <- data.table(time = kernel_haz_est$est.grid,
                         est = kernel_haz_est$haz.est,
                         method = "Kernel density")


## parametric estimation -----

dists <- c("exp", "weibull", "gompertz", "gamma", 
           "lognormal", "llogis", "gengamma")
dists_long <- c("Exponential", "Weibull (AFT)",
                "Gompertz", "Gamma", "Lognormal", "Log-logistic",
                "Generalized gamma")
parametric_haz <- vector(mode = "list", length = length(dists))
for (i in 1:length(dists)){
  fit <- flexsurvreg(
    formula = Surv(Tenure_Months, Churn_Value) ~ 1, 
    data = survdata, 
    dist = dists[i]
  ) 
  parametric_haz[[i]] <- summary(fit, type = "hazard", 
                                 ci = FALSE, tidy = TRUE)
  parametric_haz[[i]]$method <- dists_long[i]
}

parametric_haz <- rbindlist(parametric_haz)

haz <- rbind(kernel_haz, parametric_haz)
haz[, method := factor(method,
                       levels = c("Kernel density",
                                  dists_long))]
n_dists <- length(dists) 

## Data viz -----
ggplot(
  data = haz,
  aes(x = time, 
      y = est, 
      col = method, 
      linetype = method)
  ) +
  geom_line() +
  scale_colour_manual(name = "", 
                      values = c("black", rainbow(n_dists))) +
  scale_linetype_manual(name = "",
                        values = c(1, rep_len(2:6, n_dists))) +
  labs(x = "Number of months", 
       y = "Hazard", 
       title = "Hazard function for different parametric models", 
       subtitle = "Without covariates") +
  theme(legend.position = "bottom")

# it looks like the chosen parametric forms do not fit the data
# as the natural risk (kernel density) seems to be convex,
# which is not the case for the chosen models

# MODELS WITH COVARIATES ----------

formula <- final_cox$formula

## Exponential -----

### Model ----- 

exp <- flexsurvreg(formula = formula, 
                   data = data_train, 
                   dist = "exp") 

exp$coefficients

summary(exp, 
        type = "hazard") 
summary(exp,
        type = "survival") 

### Predictions -----

system.time(
  exp.haz.predstr <- predict_hazard(mod = exp, 
                                    new_dat = data_train)
) 
  
system.time(
  exp.haz.predste <- predict_hazard(mod = exp, 
                                    new_dat = data_test)
)

system.time(
  exp.surv.predstr <- predict_survival(mod = exp, 
                                       new_dat = data_train)
) 

system.time(
  exp.surv.predste <- predict_survival(mod = exp, 
                                       new_dat = data_test)
)
 

save(exp.haz.predstr, 
     exp.haz.predste, 
     exp.surv.predstr, 
     exp.surv.predste, 
     file = paste0(backup_path, "exp_predictions.RData"))

## Weibull -----

### Model -----

wei <- flexsurvreg(formula = formula, 
                   data = data_train, 
                   dist = "weibull") 

wei$coefficients

summary(wei, 
        type = "hazard") 
summary(wei,
        type = "survival") 


### Predictions -----

system.time(
  wei.haz.predstr <- predict_hazard(mod = wei, 
                                    new_dat = data_train)
) 

system.time(
  wei.haz.predste <- predict_hazard(mod = wei, 
                                    new_dat = data_test)
)

system.time(
  wei.surv.predstr <- predict_survival(mod = wei, 
                                       new_dat = data_train)
) 

system.time(
  wei.surv.predste <- predict_survival(mod = wei, 
                                       new_dat = data_test)
)

save(wei.haz.predstr, 
     wei.haz.predste, 
     wei.surv.predstr, 
     wei.surv.predste, 
     file = paste0(backup_path, "wei_predictions.RData"))

# MODEL COMPARISON 




