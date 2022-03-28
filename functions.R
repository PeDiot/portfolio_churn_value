# ---------- Bookdown functions ----------

# --- Tables' layout ---

if (knitr::is_latex_output()) {
  mykable <- function(tab, transp = FALSE, digits =2, title=NULL, font_size = NULL, scale_tab = F,...){
    if( transp ){
      if(scale_tab == F){
        tab %>% t() %>% kable(caption=title, digits = digits, booktabs=TRUE, format = "latex",...) %>%
          kable_styling(full_width = F, position = "center", 
                        latex_options = c("striped", "condensed", "HOLD_position"),
                        font_size =  font_size)
      } else {
        tab %>% t() %>% kable(caption=title, digits = digits, booktabs=TRUE, format = "latex",...) %>%
          kable_styling(full_width = F, position = "center", 
                        latex_options = c("striped", "condensed", "HOLD_position","scale_down"),
                        font_size =  font_size)
      }
      
    } else {
      if(scale_tab == F){
        tab %>% kable(caption=title, digits = digits, booktabs=TRUE,...) %>%
          kable_styling(full_width = F, position = "center", 
                        latex_options = c("striped", "condensed", "HOLD_position"),
                        font_size =  font_size)
      } else {
        tab %>% kable(caption=title, digits = digits, booktabs=TRUE,...) %>%
          kable_styling(full_width = F, position = "center", 
                        latex_options = c("striped", "condensed", "HOLD_position","scale_down"),
                        font_size =  font_size)
      }
    }
  }
} else {
  mykable <- function(tab, transp = FALSE, digits = 2, title=NULL, font_size = NULL, ...){
    if(transp){
      tab %>% t() %>% kable(caption=title, digits = digits,...) %>%
        kable_styling(full_width = F, position = "center",
                      bootstrap_options = c("striped", "condensed"))  
    } else {
      tab %>% kable(caption=title, digits = digits, ...) %>%
        kable_styling(full_width = F, position = "center",
                      bootstrap_options = c("striped", "condensed"))
    }
  }
}

# --- Kaplan-Meier curves ---

plot_km <- function(
  treatment,
  surv_data, 
  risk_table = T, 
  font_size = 12
){
  km <- survfit(
    formula = formula( paste("Surv(Tenure_Months, Churn_Value) ~ ", treatment, sep = "") ), 
    data = surv_data
  )
  surv_plot <- ggsurvplot(
    km, 
    data = surv_data, 
    fun = "pct", 
    conf.int = T, 
    risk.table = risk_table, 
    ggtheme = theme_minimal(base_size = font_size), 
    palette = "jco", 
    legend.labs = surv_data %>% 
      pull(treatment) %>%
      levels(), 
    legend.title = treatment
  )
  return (surv_plot)
}

# --- Hist/density plot depending on treatement var ---

hist_dens_plot <- function(
  data, 
  treatment, 
  target = "CLTV",
  base_size = 10
) {
  
  data %>%
    ggplot(aes_string(x = target)) +
    geom_histogram(
      aes_string(
        fill = treatment, 
        y = "..density.."
      ), 
      bins = 30, 
      alpha = .3, 
      position = "identity"
    ) +
    geom_density(
      aes_string(color = treatment), 
      size = .8
    ) +
    scale_color_jco() +
    scale_fill_jco() +
    scale_x_continuous(labels = scales::comma) +
    ylab("Density") +
    theme_minimal(base_size = base_size) +
    theme(legend.position = "top") 
  
}


dens_plot <- function(
  data, 
  treatment, 
  target = "CLTV",
  base_size = 10
) { 
  
  data %>%
    ggplot() +
    geom_density(
      aes_string(x = target,
                 color = treatment), 
      size = 1.3
    ) +
    scale_color_jco() +
    scale_fill_jco() +
    ylab("Density") +
    theme_minimal(base_size = base_size) +
    theme(legend.position = "bottom") 
  
}

hist_plot <- function(
  data, 
  treatment, 
  target = "CLTV",
  base_size = 10
) {
  
  data %>%
    ggplot(aes_string(x = target)) +
    geom_histogram(
      aes_string(fill = treatment), 
      color = "white", 
      bins = 30, 
      alpha = .5,
    ) +
    scale_color_jco() +
    scale_fill_jco() +
    scale_x_continuous(labels = scales::comma) +
    ylab("Density") +
    theme_minimal(base_size = base_size) +
    theme(legend.position = "top") +
    facet_wrap(as.formula(paste("~", treatment)))
  
}# --- Chi2 test table ---

chi_2_test_tab <- function(data, treatment){
  
  t <- chisq.test(
    x = cleaned_data %>% pull(Churn_Label),
    y = cleaned_data %>% pull(treatment))
  tc <- qchisq(p = .95, df = t$parameter)
  tab <- data.frame(
    t$statistic, 
    t$parameter, 
    tc, 
    format(t$p.value, scientific = TRUE, digits = 2)
  )
  colnames(tab) <- c("Statistic", "Df", "Critical Value", "p-value")
  rownames(tab) <- treatment
  return(tab)
  
}

# --- ANOVA test table ---

aov_test_tab <- function(data, target = "CLTV", treatment){
  
  formula <- formula( paste(target, "~", treatment) )
  anova <- aov(formula = formula, data = cleaned_data)
  sum_anova <- unlist(summary(anova))
  tab <- data.frame(
    sum_anova["F value1"], 
    sum_anova["Df1"],
    sum_anova["Df2"],
    format(sum_anova["Pr(>F)1"], scientific = TRUE, digits = 2)
  )
  colnames(tab) <- c(
    "F statistic",
    "Df1", 
    "Df2", 
    "p-value"
  )
  rownames(tab) <- treatment
  return(tab) 
  
}
