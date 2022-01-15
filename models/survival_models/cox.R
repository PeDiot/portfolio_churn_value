# ------------------- Cox Proportional Hazards Model ------------------- 

# C-index: the probability that, for a pair of randomly chosen comparable samples, 
# the sample with the higher risk prediction will experience an event (churn) before the other sample

# ---------- SET UP ----------

# ----- paths -----
dir <- "./models/survival_models/"
setwd(dir)
data_path <- "C:/Users/pemma/OneDrive - Université de Tours/Mécen/M2/S2/04 - 3R/portfolio_churn_value/data/"
backup_path <- "C:/Users/pemma/OneDrive - Université de Tours/Mécen/M2/S2/04 - 3R/portfolio_churn_value/models/backup/survival/"

# ----- packages -----
library(tidyverse)
library(survival)
library(survminer)    # for ggsurvplot
library(RegParallel)  # for parallel processing
library(ggpubr)
library(stringr)      # for text mining

theme_set(theme_minimal())

# ----- data -----
load(file = paste(data_path, "train_test_data.RData", sep = ""))

# ---------- MODELS ---------- 

# --- univariate models ---

# with "Monthly Charges"
cox_monthly_charges <- coxph(
  formula = Surv(Tenure_Months, Churn_Value) ~ Monthly_Charges, 
  data = data_train
)
summary(cox_monthly_charges)
cox_monthly_charges_risk <- predict(cox_monthly_charges, type = "risk")

cox_monthly_charges$concordance

C <- cox_monthly_charges$concordance   # C-index ~ 50.2% (non-informative model : random predictions)
C["concordance"]

# with "Total Charges"
cox_total_charges <- coxph(
  formula = Surv(Tenure_Months, Churn_Value) ~ Total_Charges, 
  data = data_train
)
summary(cox_total_charges)
C <- cox_total_charges$concordance    # C-index ~ 87.1%
C["concordance"]

# --- multivariate models ---

# with every variables
data_train_multivar <- data_train %>%
  select(-c(
    CustomerID, City, Gender, Zip_Code,  
    Phone_Service, Multiple_Lines, 
    Churn_Label, Churn_Score, CLTV, Churn_Reason
  ))

system.time(
  cox_multivar <- coxph(
    formula = Surv(Tenure_Months, Churn_Value) ~ ., 
    data = data_train_multivar
  )
)

save(cox_multivar, file = paste(backup_path, "cox_multivar.Rdata", sep = ""))

summary(cox_multivar)
stargazer::stargazer(
  cox_multivar,
  type = "text", 
  apply.coef = exp
)
C <- cox_multivar$concordance          # C-index ~ 92.7%
C["concordance"]

cox_haz_plt <- ggforest(cox_multivar, main = "", fontsize = 1)
cox_haz_plt   # lots of variables with no significant impact on the risk of churn 

# }

# with significant variables

cox_signif_var <- update(
  cox_multivar,
  . ~ . - Latitude - Longitude - Senior_Citizen - Partner - 
    Device_Protection - Streaming_TV - Streaming_Movies - Paperless_Billing
) 

summary(cox_signif_var)
C <- cox_signif_var$concordance          # C-index ~ 93.1%
C["concordance"]

# model without variables with "No internet service as levels"

cox_signif_var_2 <- update(
  cox_signif_var, 
  . ~ . - Online_Security - Online_Backup - Tech_Support
)

summary(cox_signif_var_2)

cox_haz_pred <- predict(cox_signif_var_2, type = "risk")

ggsurvplot(
  fit = survfit(cox_signif_var_2),
  data = data_train_multivar, 
  color = "#2E9FDF",
  ggtheme = theme_minimal()
) 
