# ---------- Exponential regression with maxLik ----------

# ----- Setup -----

parent_dir <- "C:/Users/pemma/OneDrive - Université de Tours/Mécen/M2/S2/04 - 3R/portfolio_churn_value/"
dir <- "./models/survival_models/"
setwd(dir)
data_path <- paste0(parent_dir, "data/") 
backup_path <- paste0(parent_dir, "/backup/survival/") 

source(paste0(parent_dir, "colors.R"))

library(tidyverse)
library(data.table) 
library(ggplot2)
library(survival)
library(maxLik)           # MLE
library(fastDummies)      # one-hot encoding
library(latex2exp)

theme_set(theme_minimal())

load(file = paste(data_path, "train_test_data.RData", sep = ""))

# ----- Inputs -----

N <- nrow(data_train)

vars <- c("Monthly_Charges", "Contract")

predictors <- data_train %>%
  dplyr::select(all_of(vars)) %>%
  dummy_cols(remove_first_dummy = T,
             remove_selected_columns = T)

X_tr <- data.frame(Intercept = rep(1, N), 
                   predictors) %>%
  as.matrix()
p <- ncol(X_tr)

t <- data_train$Tenure_Months
delta <- data_train$Churn_Value


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
  
  theta <- theta_reg(X_tr, beta)
  no_cens <- sum( delta * log(dexp(x = t, rate = theta)) )
  cens <- sum( (1 - delta) * log(surv_exp(t = t, theta = theta)) )
  
  return (no_cens + cens)
}

init_params <- function(X, vals){
  names(vals) <- c("Intercept", 
                   colnames(X)[-1])
  return(vals)
}

estimate_risk <- function(X, coefs){
  theta <- theta_reg(X, beta = coefs)
  return(theta)
}

estimate_survival <- function(X, t, coefs){
  theta <- theta_reg(X, beta = coefs)
  s <- exp(x = - theta * t)
}

# ----- Maximum Likelihood Estimation by hand -----

params <- init_params(X = X_tr,
                      vals = c(.1, .1, .1, .1))

co <- maxControl(tol = 1e-4, 
                 printLevel = 3)
res.maxLik <- maxLik(logLik = log_lik, 
                     start = params, 
                     control = co)
summary(res.maxLik)

coefs <- res.maxLik$estimate ; coefs

# ----- Estimated risk and survival functions -----

risk_tr <- estimate_risk(X_tr, coefs)
surv_tr <- estimate_survival(X_tr, t, coefs)

d <- data.frame(t = t, 
                risk = risk_tr, 
                surv = surv_tr) %>%
  group_by(t) %>%
  summarise(risk = mean(risk, na.rm = T), 
            surv = mean(surv, na.rm = T))

ggpubr::ggarrange(
  d %>%
    ggplot(aes(x = t, 
               y = surv)) +
    geom_line(size = .7,
              color = surv_col) +
    labs(x = "Number of months", 
         y = "S(t)", 
         title = "Estimated survival function", 
         subtitle = "Exponential model"), 
  d %>%
    ggplot(aes(x = t, 
               y = risk)) +
    geom_line(size = .7,
              color = risk_col) +
    labs(x = "Number of months", 
         y = TeX("$\\lambda(t)$"), 
         title = "Estimated risk function", 
         subtitle = "Exponential model"), 
  ncol = 2
)

res.aov <- aov(formula = risk ~ t, 
               data = d)
summary(res.aov)

    
# ----- Maximum Likelihood Estimation with survival -----

exp.reg <- survreg(
  formula = Surv(Tenure_Months, Churn_Value) ~ Monthly_Charges + Contract, 
  data = data_train, 
  dist = "exponential"
)

summary(exp.reg)

predict(exp.reg, 
        type = "response")    # estimated number of months 

probs <- (1:98)/100
surv_tr <- predict(
  exp.reg,
  type = "quantile",
  p = probs, 
  se.fit = T
) 

d <- data.frame(
  surv = 1-probs, 
  fit = surv_tr$fit[1, ], 
  se = 2*surv_tr$se.fit[1, ]
) 
d %>%
  ggplot(aes(x = fit, y = surv)) +
  geom_line(size = .8, color = "#2E9FDF") +
  geom_ribbon(aes(xmin = fit-se,
                  xmax = fit+se),
              alpha = .2) +
  labs(title = "Estimated survival function",
       subtitle = "Exponential model",
       x = "Number of months", 
       y = "Survival probability")
