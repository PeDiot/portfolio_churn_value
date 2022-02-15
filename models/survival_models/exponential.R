# ---------- Exponential regression with maxLik ----------

# ----- Setup -----

dir <- "./models/survival_models/"
setwd(dir)
data_path <- "C:/Users/pemma/OneDrive - Université de Tours/Mécen/M2/S2/04 - 3R/portfolio_churn_value/data/"
backup_path <- "C:/Users/pemma/OneDrive - Université de Tours/Mécen/M2/S2/04 - 3R/portfolio_churn_value/backup/survival/"

library(tidyverse)
library(data.table) 
library(ggplot2)
library(survival)
library(maxLik)       # MLE
library(mltools)      # one-hot encoding

theme_set(theme_minimal())

load(file = paste(data_path, "train_test_data.RData", sep = ""))

# ----- Functions -----

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
  beta <- params %>%
    matrix(nrow = p, ncol = 1)
  
  theta <- theta_reg(X, beta)
  no_cens <- sum( delta * log(dexp(x = t, rate = theta)) )
  cens <- sum( (1 - delta) * log(surv_exp(t = t, theta = theta)) )
  
  return (no_cens + cens)
}

init_params <- function(X, vals){
  names(vals) <- c("Intercept", 
                   colnames(X)[-1])
  return(vals)
}

# ----- Inputs -----

N <- nrow(data_train)

vars <- c("Monthly_Charges", "Contract")

predictors <- data_train %>%
  dplyr::select(all_of(vars)) %>%
  as.data.table() %>% one_hot()

X <- data.frame(Bias = rep(1, N), 
                predictors) %>%
  as.matrix()
p <- ncol(X)

t <- data_train$Tenure_Months
delta <- data_train$Churn_Value

params <- init_params(X = X,
                      vals = c(.1, .1))

# ----- Maximum Likelihood Estimation by hand -----

co <- maxControl(tol = 1e-4, 
                 printLevel = 3)
res.maxLik <- maxLik(logLik = log_lik, 
                     start = params, 
                     control = co)
summary(res.maxLik)

coefs <- res.maxLik$estimate

# ----- Maximum Likelihood Estimation with survival -----

exp.reg <- survreg(
  formula = Surv(Tenure_Months, Churn_Value) ~ Monthly_Charges, 
  data = data_train, 
  dist = "exponential"
)

summary(exp.reg)
