# ------------------- Cox Proportional Hazards Model ------------------- 

# C-index: the probability that, for a pair of randomly chosen comparable samples, 
# the sample with the higher risk prediction will experience an event (churn) before the other sample

# ---------- SET UP ----------

# ----- paths -----
dir <- "./models/survival_models/"
setwd(dir)
data_path <- "C:/Users/pemma/OneDrive - Université de Tours/Mécen/M2/S2/04 - 3R/portfolio_churn_value/data/"
backup_path <- "C:/Users/pemma/OneDrive - Université de Tours/Mécen/M2/S2/04 - 3R/portfolio_churn_value/backup/survival/"

# ----- packages -----
library(tidyverse)
library(data.table)
library(survival)
library(survminer)    # for ggsurvplot
library(survcomp)
library(muhaz)        # for kernel density estimator
library(ggplot2)
library(plotly)

theme_set(theme_minimal())

# ----- data -----
load(file = paste(data_path, "train_test_data.RData", sep = ""))

# ---------- TRAINING ---------- 

# --- kernel density estimation ---

survdata <- data.table(data_train)
kernel_haz_est <- muhaz(
  times=survdata$Tenure_Months,
  delta=survdata$Churn_Value, 
  min.time = 0, 
  max.time = 72
)
kernel_haz <- data.table(time = kernel_haz_est$est.grid,
                         est = kernel_haz_est$haz.est,
                         method = "Kernel density")
haz_plot <- kernel_haz %>%
  ggplot(aes(x = time, 
             y = est)) +
  geom_line(size = .7) +
  labs(x = "Number of months", 
       y = "Churn risk", 
       title = "Observed churn risk", 
       subtitle = "Kernel density")

# NOT RUN {

# --- univariate models ---

# with Monthly_Charges
cox_monthly_charges <- coxph(
  formula = Surv(Tenure_Months, Churn_Value) ~ Monthly_Charges, 
  data = data_train
)
summary(cox_monthly_charges)
cox_monthly_charges_risk <- predict(cox_monthly_charges, type = "risk")

cox_monthly_charges$concordance

C <- cox_monthly_charges$concordance   # C-index ~ 50.2% (non-informative model : random predictions)
C["concordance"]

# with Total_Charges
cox_total_charges <- coxph(
  formula = Surv(Tenure_Months, Churn_Value) ~ Total_Charges, 
  data = data_train
)
summary(cox_total_charges)
C <- cox_total_charges$concordance    # C-index ~ 87.1%
C["concordance"]

cox_total_charges_pred <- predict(
  cox_total_charges, 
  newdata = data_test, 
  type = "risk"
)
C_test <- concordance.index(
  x = cox_total_charges_pred,
  surv.time = data_test$Tenure_Months,
  surv.event = data_test$Churn_Value
)
C_test$c.index

# }


# --- multivariate models ---

data_train_multivar <- data_train %>%
  dplyr::select(-c(
    CustomerID, City, Gender, Zip_Code, Multiple_Lines, 
    Churn_Label, Churn_Score, CLTV, Churn_Reason
  ))

# remove Multiple_Lines due to collinearity with Phone_Service


# NOT RUN {

# with every variables

system.time(
  cox_multivar <- coxph(
    formula = Surv(Tenure_Months, Churn_Value) ~ ., 
    data = data_train_multivar
  )
)

save(cox_multivar, file = paste(backup_path, "cox_multivar.Rdata", sep = ""))

# }

load(paste(backup_path, "cox_multivar.Rdata", sep = ""))

summary(cox_multivar)
stargazer::stargazer(
  cox_multivar,
  type = "text"
)
C <- cox_multivar$concordance          # C-index ~ 93.16%
C["concordance"]

cox_haz_plt <- ggforest(cox_multivar, main = "", fontsize = 1)
cox_haz_plt     

# remove variables with no significant influence on the churn hazard 

# NOT RUN {

cox_signif_var <- update(
  cox_multivar,
  . ~ . - Latitude - Longitude - Senior_Citizen - Partner - 
    Device_Protection - Streaming_TV - Streaming_Movies - Paperless_Billing - Monthly_Charges
) 

save(cox_signif_var, file = paste(backup_path, "cox_signif_var.Rdata", sep = ""))

# }

load(paste(backup_path, "cox_signif_var.Rdata", sep = ""))

summary(cox_signif_var)
C <- cox_signif_var$concordance          # C-index ~ 93.15%
C["concordance"]

ggforest(cox_signif_var, main = "", fontsize = 1)


# ---------- PREDICTION ---------- 

risk_pred_tr <- predict(
  cox_signif_var, 
  type = "risk", 
  se.fit = T
)

risk_pred_te <- predict(
  cox_signif_var, 
  newdata = data_test, 
  type = "risk", 
  se.fit = T
)

# predicted churn risk

p1 <- data.frame(num_months = data_train$Tenure_Months, 
           risk = risk_pred_tr$fit, 
           se = risk_pred_tr$se.fit) %>%
  mutate(lower_bound = risk - 1.96*se, 
         upper_bound = risk + 1.96*se) %>%
  group_by(num_months) %>%
  summarise(across(-se,
                   mean,
                   na.rm = T)) %>%
  ggplot(aes(x = num_months, 
             y = risk)) +
  geom_line(size = .7, 
            color = "#2E9FDF") +
  geom_ribbon(aes(ymin = lower_bound, 
                  ymax = upper_bound), 
              fill = "grey70", 
              alpha = .3) +
  labs(x = "Number of months", 
       y = "Churn risk", 
       title = "Cox model estimation",
       subtitle = "Training set")

ggplotly(p1)

p2 <- data.frame(num_months = data_test$Tenure_Months, 
                 risk = risk_pred_te$fit, 
                 se = risk_pred_te$se.fit) %>%
  mutate(lower_bound = risk - 1.96*se, 
         upper_bound = risk + 1.96*se) %>%
  group_by(num_months) %>%
  summarise(across(-se,
                   mean,
                   na.rm = T)) %>%
  ggplot(aes(x = num_months, 
             y = risk)) +
  geom_line(size = .7, 
            color = "#BE7970") +
  geom_ribbon(aes(ymin = lower_bound, 
                  ymax = upper_bound), 
              fill = "grey70", 
              alpha = .3) +
  labs(x = "Number of months", 
       y = "Churn risk",
       title = "Cox model estimation",
       subtitle = "Testing set")

ggarrange(haz_plot,  
          p1, p2, 
          ncol = 3)

# data set for clustering

data_clust_tr <- cbind(data_train, 
                       Churn_Risk = risk_pred_tr)
data_clust_te <- cbind(data_test, 
                       Churn_Risk = risk_pred_te)

save(
  data_clust_tr, data_clust_te,
  file = paste(data_path, "train_test_data_clust.RData", sep = "")
)

# ---------- SAVE COX MODEL ---------- 

load(file = paste(data_path, "telco_cleaned.RData", sep = "/"))

formula <- cox_signif_var$formula
final_cox <- coxph(
  formula = formula, 
  data = cleaned_data
)

summary(final_cox)

save(final_cox, 
     file = paste(backup_path, "cox_final.RData", sep = "/")) 
