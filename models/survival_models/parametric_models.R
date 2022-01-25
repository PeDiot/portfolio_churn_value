# ------------------- Parametric models for survival data ------------------- 

# Doc: 
  # https://www.ms.uky.edu/~mai/Rsurv.pdf
  # https://www.rdocumentation.org/packages/survival/versions/3.2-7/topics/predict.survreg
  # https://www.r-bloggers.com/2019/06/parametric-survival-modeling/


# Exponential model
# Weibull model

# ---------- SET UP ----------

# ----- paths -----
dir <- "./models/survival_models/"
setwd(dir)
data_path <- "C:/Users/pemma/OneDrive - Université de Tours/Mécen/M2/S2/04 - 3R/portfolio_churn_value/data/"
backup_path <- "C:/Users/pemma/OneDrive - Université de Tours/Mécen/M2/S2/04 - 3R/portfolio_churn_value/models/backup/survival/"

# ----- packages -----
library(tidyverse)
library(data.table)
library(ggplot2)

library(survival)
library(flexsurv)   # for parametric survival models 
library(survminer)  
library(survcomp)   # for concordance index
library(muhaz)      # for kernel density estimator
library(lmtest)     # for LR test 

theme_set(theme_minimal())

# ----- data -----
load(file = paste(data_path, "train_test_data.RData", sep = ""))

# ---------- INTERCEPT ONLY MODELS ----------

survdata <- data.table(data_train)
head(survdata)

# kernel density estimator not working
kernel_haz_est <- muhaz(
  times=survdata$Tenure_Months,
  delta=survdata$Churn_Value, 
  min.time = 0, 
  max.time = 72
)
kernel_haz <- data.table(time = kernel_haz_est$est.grid,
                         est = kernel_haz_est$haz.est,
                         method = "Kernel density")



# parametric estimation
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
ggplot(
  data = haz,
  aes(x = time, 
      y = est, 
      col = method, 
      linetype = method)
  ) +
  geom_line() +
  xlab("Days") + ylab("Hazard") + 
  scale_colour_manual(name = "", 
                      values = c("black", rainbow(n_dists))) +
  scale_linetype_manual(name = "",
                        values = c(1, rep_len(2:6, n_dists)))

# it looks like the chosen parametric forms do not fit the data
# as the natural risk (kernel density) seems to be convex,
# which is not the case for the chosen models

# ---------- MODELS WITH COVARIATES ----------

# ----- Exponential -----

# --- Without explanatory variable

# survival package 

exp.reg1 <- survreg(
  formula = Surv(Tenure_Months, Churn_Value) ~ 1, 
  data = data_train, 
  dist = "exponential"
)

summary(exp.reg1)

exp.reg1$loglik

predict(exp.reg1,
        newdata = data_train,
        type = "response") 

probs <- (1:98)/100

pred_exp.reg1 <- predict(
  exp.reg1, 
  newdata = data_train,
  type = "quantile",
  p=probs, 
  se.fit = T
) 

d <- data.frame(
  surv = 1-probs, 
  fit = pred_exp.reg1$fit[1, ], 
  se = 2*pred_exp.reg1$se.fit[1, ]
) 
d %>%
  ggplot(aes(x = fit, y = surv)) +
  geom_line(size = .8, color = "#2E9FDF") +
  geom_ribbon(
    aes(
      xmin = fit-se,
      xmax = fit+se
    ), 
    alpha = .2
  ) +
  labs(
    title = "Estimated survival with exponential model without explanatory variables", 
    x = "Number of months", 
    y = "Survival probability"
  )


# NOT RUN {
c_index <- concordance.index(
  x = exp.reg1_pred, 
  surv.time = data_train$Tenure_Months, 
  surv.event = data_train$Churn_Value
)
c_index$c.index
# }

# flexsurv package 

flexsurv1 <- flexsurvreg(
  formula = Surv(Tenure_Months, Churn_Value) ~ 1, 
  data = data_train, 
  dist = "exponential"
) 

summary(flexsurv1)

flexsurv1$coefficients
flexsurv1$res

# --- With Total_Charges

exp.reg2 <- survreg(
  formula = Surv(Tenure_Months, Churn_Value) ~ Total_Charges, 
  data = data_train, 
  dist = "exponential"
)

summary(exp.reg2)

exp.reg2$loglik

lrtest(exp.reg1, exp.reg2)  # model 2 performs better than model 1


predict(exp.reg2,
        newdata = data_train,
        type = "response") 


pred_exp.reg2 <- predict(
  exp.reg2, 
  newdata = data_train,
  type = "quantile",
  p=probs, 
  se.fit = T
) 

d <- data.frame(
  surv = 1-probs, 
  fit = pred_exp.reg2$fit[1, ], 
  se = 2*pred_exp.reg2$se.fit[1, ]
) 
d %>%
  ggplot(aes(x = fit, y = surv)) +
  geom_line(size = .8, color = "#2E9FDF") +
  geom_ribbon(
    aes(
      xmin = fit-se,
      xmax = fit+se
    ), 
    alpha = .2
  ) +
  labs(
    title = "Estimated survival with exponential model with 'Total_Charges' as explanatory variable", 
    x = "Number of months", 
    y = "Survival probability"
  )

# flexsurv package 

flexsurv2 <- flexsurvreg(
  formula = Surv(Tenure_Months, Churn_Value) ~ Total_Charges, 
  data = data_train, 
  dist = "exponential"
) 

summary(flexsurv2)

ggsurvplot(
  flexsurv2, 
  conf.int = F, 
  xlab = "Number of months", 
  ylab = "Survival probability", 
  break.time.by = 1, 
  risk.table = F
)

flexsurv2_pred <- predict(
  flexsurv2, 
  newdata = data_train, 
  type = "survival"
)
tidyr::unnest(flexsurv2_pred, .pred)

# --- With several covariates

data_train_multivar <- data_train %>%
  dplyr::select(-c(
    CustomerID, City, Gender, Zip_Code, Multiple_Lines, 
    Churn_Label, Churn_Score, CLTV, Churn_Reason
  ))

exp.reg_multivar <- survreg(
  formula = Surv(Tenure_Months, Churn_Value) ~ ., 
  data = data_train_multivar, 
  dist = "exponential"
)

summary(exp.reg_multivar)





