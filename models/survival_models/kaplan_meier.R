# ------------------- Kaplan-Meier Estimation -------------------  

# ---------- SOURCES ---------

# https://medium.com/plytrix-analytics/3-ways-to-predict-your-customer-is-about-to-churn-eae4252b90b6

# ---------- SET UP ----------

# ----- paths -----
dir <- "./models/survival_models/"
setwd(dir)
data_path <- "C:/Users/pemma/OneDrive - Université de Tours/Mécen/M2/S2/04 - 3R/portfolio_churn_value/data/"

# ----- packages -----
library(tidyverse)
library(survival)
library(survminer)    # for ggsurvplot
library(ggpubr)
library(stringr)      # for text mining

theme_set(theme_minimal())

# ----- data -----
load(file = paste(data_path, "train_test_data.RData", sep = ""))

# ---------- MODELS ---------- 

# ----- Simple Kaplan-Meier ------

# standard survival object
km <- with(
  data_train,
  Surv(Tenure_Months, Churn_Value)
)

surv_data <- data.frame(
  Churn = data_train$Churn_Value, 
  Time = km
)
View(surv_data)

km_summary <- surv_data %>%
  group_by(as.factor(Churn)) %>%
  summarise(mean(Time)) %>%
  rename(
    Churn = `as.factor(Churn)`, 
    Avg_Time = `mean(Time)`
  ) %>%
  as.data.frame()
km_summary
# churners have an average duration of ~9 months 
# censored observations have an average duration of ~19 months

# km estimation
km_fit <- survfit(Surv(Tenure_Months, Churn_Value) ~ 1, data = data_train)
summary(km_fit)

# plot cumulative events 
event <- ggsurvplot(
  km_fit, 
  data = data_train, 
  fun = "event", 
  conf.int = T, 
  risk.table = T
)
event
# plot cumulative hazard function
cum_haz <- ggsurvplot(
  km_fit, 
  data = data_train, 
  fun = "cumhaz", 
  conf.int = T, 
  risk.table = T
)
cum_haz
# plot survival function
surv <- ggsurvplot(
  km_fit, 
  data = data_train, 
  fun = "pct", 
  conf.int = T, 
  risk.table = T
)
surv

# ----- Kaplan-Meier with treatment variables -----

# treatment <- "Gender"
# paste("Surv(Tenure_Months, Churn_Value) ~ ", treatment, sep = "")

plot_km <- function(treatment, risk_table = T){
  km <- survfit(
    formula = formula( paste("Surv(Tenure_Months, Churn_Value) ~ ", treatment, sep = "") ), 
    data = data_train
  )
  surv_plot <- ggsurvplot(
    km, 
    data = data_train, 
    fun = "pct", 
    conf.int = T, 
    risk.table = risk_table
  )
  return (surv_plot)
}

# --- with demographic variables ---

demographics <- c("Gender", "Senior_Citizen", "Partner", "Dependents")
# "City" is not taken into account since it has 1129 categories
# data_train %>% pull(City) %>% nlevels()

plot_list <- lapply(
  demographics, 
  plot_km, 
  risk_table = F
)
demographics_plot <- arrange_ggsurvplots(
  x = plot_list, 
  ncol = 2, nrow = 2,
  print = F
)

# No impact of "Gender" in terms of survival probability 

ggsave(
  filename = paste(figs_path, "demographics_plot.pdf", sep = "/"), 
  plot = demographics_plot, 
  height = 12, width = 15
)

# --- with transaction variables ---

transaction <- c("Phone_Service",
                 "Multiple_Lines",
                 "Internet_Service",
                 "Online_Security",
                 "Online_Backup",
                 "Device_Protection", 
                 "Tech_Support",
                 "Streaming_TV",
                 "Streaming_Movies",
                 "Paperless_Billing",
                 "Payment_Method")

plot_list <- lapply(
  transaction,
  plot_km, 
  risk_table = F
)

transaction_plot <- arrange_ggsurvplots(
  x = plot_list, 
  ncol = 3, nrow = 4,
  print = F
)

# No impact of "Phone Service" and "Multiple Lines" in terms of survival probability 

ggsave(
  filename = paste(figs_path, "transaction_plot.pdf", sep = "/"), 
  plot = transaction_plot, 
  height = 20, width = 18
)
