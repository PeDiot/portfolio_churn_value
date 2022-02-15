# --------------- Weibull model with maxLik ---------------

# ----- Setup -----

dir <- "./models/survival_models/"
setwd(dir)
data_path <- "C:/Users/pemma/OneDrive - Université de Tours/Mécen/M2/S2/04 - 3R/portfolio_churn_value/data/"
backup_path <- "C:/Users/pemma/OneDrive - Université de Tours/Mécen/M2/S2/04 - 3R/portfolio_churn_value/backup/survival/"

library(tidyverse)
library(ggplot2)
library(survival)
library(muhaz)      # for kernel density estimator
library(maxLik)


theme_set(theme_minimal())

load(file = paste(data_path, "train_test_data.RData", sep = ""))

# ----- MLE -----

# fixed parameters
N <- nrow(data_train)

X <- data.frame(
  Bias = rep(1, N), 
  data_train %>%
    dplyr::select(Monthly_Charges)
) %>%
  as.matrix()
p <- ncol(X)

t <- data_train$Tenure_Months
delta <- data_train$Churn_Value

# survival function 
surv_wei <- function(t, gamma, alpha = 1){
  1 - pweibull(
    q = t, 
    shape = gamma, 
    scale = alpha)
}

# model gamma
gamma_reg <- function(X, beta){
  fit <- exp(X%*%beta)
  return(fit)
}

# log-likelihood
log_lik <- function(params){
  n_params <- length(params)
  beta <- params[-n_params] %>%
    matrix(nrow = p, 
           ncol = 1)
  alpha <- params[n_params] %>%
    matrix(nrow = 1, 
           ncol = 1)
  
  gamma <- gamma_reg(X, beta)
  no_cens <- sum( delta * dweibull(x = t, 
                                   shape = gamma, 
                                   scale = alpha) )
  cens <- sum( (1 - delta) * surv_wei(t = t, 
                                      shape = gamma, 
                                      scale = alpha) )
  
  return (no_cens + cens)
}

# initialization
params_init <- c("beta" = c(.1, .1), 
                 "alpha" = .1)

log_lik(params_init)

