# ---------- Generalized Weibull estimation with maxLik ----------

# ----- Setup -----

dir <- "./models/survival_models/"
setwd(dir)
data_path <- "C:/Users/pemma/OneDrive - Université de Tours/Mécen/M2/S2/04 - 3R/portfolio_churn_value/data/"
backup_path <- "C:/Users/pemma/OneDrive - Université de Tours/Mécen/M2/S2/04 - 3R/portfolio_churn_value/backup/survival/"

library(tidyverse)
library(ggplot2)
library(survival)
library(rmutil)     # for generalized Weibull distribution 
library(muhaz)      # for kernel density estimator
library(maxLik)


theme_set(theme_minimal())

load(file = paste(data_path, "train_test_data.RData", sep = ""))


# ----- Unconditionnal churn risk -----

t <- data_train %>%
  pull(Tenure_Months)
status <- data_train %>%
  pull(Churn_Value)

# kernel density estimator not working
kernel_haz_est <- muhaz(
  times =  t,
  delta = status, 
  min.time = 0, 
  max.time = 72
)
kernel_haz <- data.frame(time = kernel_haz_est$est.grid,
                         est = kernel_haz_est$haz.est,
                         method = "Kernel density")
kernel_haz %>%
  ggplot(aes(x = time, 
             y = est)) +
  geom_line(size = .8, 
            color = "blue", 
            alpha = .3) +
  labs(x = "Tenure months", 
       y = "Risk", 
       title = "Unconditional churn risk", 
       subtitle = "Kernel density estimation")

surv <- with(
  data_train,
  Surv(time = Tenure_Months, 
       event = Churn_Value,
       type = "right")
)


# ----- Generalized Weibull distribution -----

mu <- .2
alpha <- .4
gamma <- .1

S <- 1 - pgweibull(q = t, 
                   m = mu, 
                   s = alpha, 
                   f = gamma)

gwei_risk <- function(t, S, mu, alpha, gamma){
  r <- gamma*alpha*t**(alpha-1)*S**(-mu)
  return(r)
}

r <- gwei_risk(t = t, 
               S = S, 
               mu = mu, 
               alpha = alpha, 
               gamma = gamma) 

data.frame(t = t,
           S = S, 
           r = r) %>%
  rename(Risk = r, 
         Survival = S) %>%
  pivot_longer(cols = Survival:Risk) %>%
  ggplot(aes(x = t, 
             y = value, 
             color = name)) +
  geom_line(size = .8) +
  scale_color_brewer(palette = "Set2") +
  facet_grid(~name) +
  theme(legend.position = "none")

# ----- Maximum Likelihood Estimation -----

# L(theta) = Pi f(t_i| x_i, theta)^delta_i * S(t_i| x_i, theta)^(1-delta_i)
# with delta_i = 0 if censoring



