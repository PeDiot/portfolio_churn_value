# ---------- Generalized Weibull estimation with maxLik ----------

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

# ----- Maximum Likelihood Estimation -----

# fixed parameters
X <- data_train %>%
  select(Monthly_Charges) %>%
  as.matrix()

p <- ncol(X)

t <- data_train$Tenure_Months
delta <- data_train$Churn_Value

# survival function 
surv_exp <- function(t, theta){
  1 - pexp(q = t, rate = theta)
}

# theta regression 
theta_reg <- function(X, beta){
  fit <- exp(-X%*%beta)
  return(fit)
}

# log-likelihood
log_lik <- function(params){
  beta <- params["beta"] %>%
    matrix(nrow = p, ncol = 1)
 
  theta <- theta_reg(X, beta)
  no_cens <- sum( delta * dexp(x = t, rate = theta) )
  cens <- sum( (1 - delta) * surv_exp(t = t, theta = theta) )
  
  return (no_cens + cens)
}

# initialization

params_init <- c("beta" = .1)

co <- maxControl(tol = 1e-4, 
                 printLevel = 3)
res.maxLik <- maxLik(logLik = log_lik, 
                     start = params_init, 
                     control = co)
summary(res.maxLik)

