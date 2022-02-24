# --------------- Weibull Model Estimation ---------------

# ----- Setup -----

source("colors.R")
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

# format parameters ---
format_alpha <- function(params){
  params["alpha"]
}
format_beta <- function(params){
  params[
    setdiff(names(params),
            "alpha")
  ] %>%
    as.matrix(nrow = p, ncol = 1)
}

# log-likelihood ---
log_lik <- function(params){
  
  alpha <- format_alpha(params)
  beta <- format_beta(params) 
  
  gamma <- gamma_reg(X_tr, beta)
  no_cens <- sum( delta_tr * (X_tr%*%beta + log(alpha) + (alpha - 1)*log(time_tr) - gamma*time_tr**alpha) )
  cens <- sum( (1 - delta_tr) * gamma*time_tr**alpha )
  
  return (no_cens - cens)
}

# hazard function for Weibull model ---
haz_wei <- function(gamma, alpha, t){
  h <- gamma*alpha*t**(alpha-1)
  return(h)
}

# survivor function for Weibull model ---
surv_wei <- function(gamma, alpha, t){
  s <- exp(-gamma*t**alpha)
  return(s)
}

# ----- Inputs -----

vars <- "Monthly_Charges"

X_tr <- build_feature_vector(data = data_train, 
                             vars = vars)
p <- ncol(X_tr)

time_tr <- data_train$Tenure_Months
delta_tr <- data_train$Churn_Value

beta_init <- c(.01, .01)
names(beta_init) <- colnames(X_tr)
params_init <- c(beta_init, 
                 "alpha" = .01)

# ------ MLE using maxLik ------

co <- maxControl(tol = 1e-4, 
                 printLevel = 3)

method <- "NR"
res.maxLik <- maxLik(logLik = log_lik, 
                     start = params_init, 
                     control = co, 
                     method = method)
summary(res.maxLik)

params.opt <- res.maxLik$estimate
gamma_opt <- gamma_reg(X = X_tr, 
                       beta = format_beta(params.opt))
alpha.opt <- format_alpha(params.opt)

# ----- MLE using optimx -----

# NOT RUN {

# res.optimx <- optimx(par = params_init,
# fn = log_lik,
# hessian = TRUE,
# control = list(trace = 0, all.methods = TRUE))

# }


# ----- Weibull model using flexsurvreg -----

wei.survreg <- flexsurvreg(
  formula = Surv(time = Tenure_Months, event = Churn_Value) ~ Monthly_Charges, 
  data = data_train,
  dist = "weibullph")

survreg_coefs <- wei.survreg$coefficients

paste("Scale parameter estimated with maxLik:", 
      log(params.opt["alpha"]) %>%
        round(3))

paste("Scale parameter estimated with flexsurvreg:", 
      survreg_coefs["shape"] %>%
        round(3))


# ----- Risk and Survival prediction -----

system.time(
  haz.survreg <- predict(wei.flexsurvreg, 
                         type = "hazard", 
                         times = 1:72, 
                         se.fit = T)
)


system.time(
  haz.maxLik <- haz_wei(gamma = gamma_opt, 
                        alpha = alpha.opt,
                        t = time_tr)
)

risk_plot <- cbind(data_train,
                   haz.maxLik,
                   haz.survreg) %>%
  group_by(Tenure_Months) %>%
  summarise(haz.maxLik = mean(haz.maxLik),
            haz.survreg = haz.survreg) %>%
  ggplot(aes(x = Tenure_Months, 
             y = churn_risk)) +
  geom_line(size = .7, 
            color = risk_col) +
  labs(x = "Number of months", 
       y = "Churn risk", 
       title = "Weibull model") ; risk_plot