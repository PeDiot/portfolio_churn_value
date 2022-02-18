# --------------- Weibull model with maxLik ---------------

# ----- Setup -----

dir <- "./models/survival_models/"
setwd(dir)
data_path <- "C:/Users/pemma/OneDrive - Université de Tours/Mécen/M2/S2/04 - 3R/portfolio_churn_value/data/"
backup_path <- "C:/Users/pemma/OneDrive - Université de Tours/Mécen/M2/S2/04 - 3R/portfolio_churn_value/backup/survival/"

library(tidyverse)
library(ggplot2)
library(survival)
library(flexsurv)     # parametric models 
library(maxLik)
library(fastDummies)  # one-hot encoding

theme_set(theme_minimal())

load(file = paste(data_path, "train_test_data.RData", sep = ""))

# ----- Functions -----

# build feature vector ---
build_feature_vector <- function(data, vars){
  
  N <- nrow(data)
  
  if (length(vars) == 1){
    predictors <- data_train %>%
      dplyr::select(all_of(vars))
    X_tr <- data.frame(Intercept = rep(1, N), 
                       predictors) %>%
      as.matrix()
  }
  else {
    predictors <- data_train %>%
      dplyr::select(all_of(vars)) %>%
      dummy_cols(remove_first_dummy = T,
                 remove_selected_columns = T)
    
    X_tr <- data.frame(Intercept = rep(1, N), 
                       predictors) %>%
      as.matrix()
  }
  
  return(X_tr)
  
}

# regression on gamma parameter ---
gamma_reg <- function(X, beta){
  fit <- exp(X%*%beta)
  return(fit)
}

# initialize parameters ---
init_alpha <- function(params){
  params["alpha"]
}
init_beta <- function(params){
  params[
    setdiff(names(params),
            "alpha")
    ] %>%
    as.matrix(nrow = p, ncol = 1)
}

# log-likelihood ---
log_lik <- function(params){
  
  alpha <- init_alpha(params)
  beta <- init_beta(params) 
  
  gamma <- gamma_reg(X_tr, beta)
  no_cens <- sum( delta_tr * (X_tr%*%beta + log(alpha) + (alpha - 1)*log(time_tr) - gamma*time_tr**alpha) )
  cens <- sum( (1 - delta_tr) * gamma*time_tr**alpha )
  
  return (no_cens - cens)
}

# ----- Inputs -----

vars <- "Monthly_Charges"

X_tr <- build_feature_vector(data = data_train, 
                             vars = vars)
p <- ncol(X_tr)

time_tr <- data_train$Tenure_Months
delta_tr <- data_train$Churn_Value

# ------ MLE ------

params_init <- c("beta" = c(.1, .1), 
                 "alpha" = .1)
co <- maxControl(tol = 1e-4, 
                 printLevel = 3)

method <- "NR"
res.maxLik <- maxLik(logLik = log_lik, 
                     start = params_init, 
                     control = co, 
                     method = method)
summary(res.maxLik)

# ----- Analyze results -----

# Estimate regression coeficents using flexsurvreg ---

