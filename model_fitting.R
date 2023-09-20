# MIXED MODEL FITTING

library(XML)
library(methods)
library(magrittr)
library(dplyr)
library(tidyverse)
library(here)
detach("package:here", unload = TRUE)
library(here)
library(tm)
library(wordcloud)
library(RColorBrewer)
library(gm)
library(FRACTION)
library(gsubfn)
library(np)
library(tidytext)
library(stringi)
library(ggplot2)
library(mclust)
library(readxl)
library(lme4)
library(MASS)
library(lmerTest)
library(wordcloud2)
library(RColorBrewer)
library(patchwork)

metric_table = read_csv("metric_table.csv", show_col_types = F)
metric_table = metric_table[!is.na(metric_table$year), ]

normal_p = function(mean_x, se_x) {
  # Returns approximate p-value of t-test assuming normal distribution
  # (also known as studentized p-value)
  t_statistic = mean_x/se_x
  sapply(t_statistic, function(x) 2*min(pnorm(x, lower.tail = F), pnorm(x)))
}

significance = function(p_values) {
  case_when(
    p_values > 0.1 ~ "",
    p_values > 0.05 ~ ".", 
    p_values > 0.01 ~ "*", 
    p_values > 0.001 ~ "**", 
    TRUE ~ "***"
  )
}

bootstrap_lmer = function(lmer_model, out_file_path) {
  
  # This is a custom function made to perform parametric bootstrap on 
  # a model given as argument. It outputs a written csv file with the effects,
  # according to out_file_path.
  
  print(Sys.time)
  tictoc::tic()
  
  fixed_effects = fixef(lmer_model)
  names(fixed_effects) = NULL
  
  boot_t = bootMer(lmer_model, FUN = fixef, nsim = 1000, use.u = T, 
                   type = "parametric", parallel = "multicore")$t
  
  colnames(boot_t) = str_remove_all(colnames(boot_t), "stem")
  std_error = apply(boot_t, 2, sd)
  lower_95_CI = apply(boot_t, 2, \(x) quantile(x, 0.025))
  upper_95_CI = apply(boot_t, 2, \(x) quantile(x, 0.975))
  
  df = data.frame("Fixed_effect" = colnames(boot_t), 
                  "Coefficient" = fixed_effects, 
                  "Std_error" = std_error, 
                  "Lower_95_CI" = lower_95_CI, 
                  "Upper_95_CI" = upper_95_CI) %>% arrange(Fixed_effect)
  
  df$p_value = normal_p(df$Coefficient, df$Std_error)
  df$Signif = significance(df$p_value)
  
  write_csv(df, out_file_path)
  
  tictoc::toc()
}

# LOCAL SLOPE 

# Centered, 1 neighbor
mod_ls1 = lmer(local_slope_1 ~ stem + stem:year + (1 | didone_id), 
               data = metric_table)

bootstrap_lmer(mod_ls1, here::here("bootstrap_tables", "bootstrap_table_ls1.csv"))

# Centered, 2 neighbors
mod_ls2 = lmer(local_slope_2 ~ stem + stem:year + (1 | didone_id), 
               data = metric_table)

bootstrap_lmer(mod_ls2, here::here("bootstrap_tables", "bootstrap_table_ls2.csv"))

# Left, 1 neighbor
mod_ls1u = lmer(local_slope_bef_1 ~ stem + stem:year + (1 | didone_id), 
                data = metric_table[!is.na(metric_table$local_slope_bef_1), ])

bootstrap_lmer(mod_ls1u, here::here("bootstrap_tables", "bootstrap_table_ls1u.csv"))

# Left, 2 neighbors
mod_ls2u = lmer(local_slope_bef_2 ~ stem + stem:year + (1 | didone_id), 
                data = metric_table[!is.na(metric_table$local_slope_bef_2), ])

bootstrap_lmer(mod_ls2u, here::here("bootstrap_tables", "bootstrap_table_ls2u.csv"))


# LOCAL QUADRATIC

# Centered, 1 neighbor
mod_lq1 = lmer(local_quadratic_1 ~ stem + stem:year + (1 | didone_id), 
               data = metric_table[!is.na(metric_table$local_quadratic_1), ])

bootstrap_lmer(mod_lq1, here::here("bootstrap_tables", "bootstrap_table_lq1.csv"))

# Centered, 2 neighbors
mod_lq2 = lmer(local_quadratic_2 ~ stem + stem:year + (1 | didone_id), 
               data = metric_table[!is.na(metric_table$local_quadratic_2), ])

bootstrap_lmer(mod_lq2, here::here("bootstrap_tables", "bootstrap_table_lq2.csv"))

# Left, 1 neighbor
mod_lq1u = lmer(local_quadratic_bef_1 ~ stem + stem:year + (1 | didone_id), 
                data = metric_table[!is.na(metric_table$local_quadratic_bef_1), ])

bootstrap_lmer(mod_lq1u, here::here("bootstrap_tables", "bootstrap_table_lq1u.csv"))

# Left, 2 neighbors
mod_lq2u = lmer(local_quadratic_bef_2 ~ stem + stem:year + (1 | didone_id), 
                data = metric_table[!is.na(metric_table$local_quadratic_bef_2), ])

bootstrap_lmer(mod_lq2u, here::here("bootstrap_tables", "bootstrap_table_lq2u.csv"))


# MONOTONIC RHO

# Centered, 1 neighbor
mod_mr1 = lmer(monotonic_rho_1 ~ stem + stem:year + (1 | didone_id), 
               data = metric_table[!is.na(metric_table$monotonic_rho_1), ])

bootstrap_lmer(mod_mr1, here::here("bootstrap_tables", "bootstrap_table_mr1.csv"))

# Centered, 2 neighbors
mod_mr2 = lmer(monotonic_rho_2 ~ stem + stem:year + (1 | didone_id), 
               data = metric_table[!is.na(metric_table$monotonic_rho_2), ])

bootstrap_lmer(mod_mr2, here::here("bootstrap_tables", "bootstrap_table_mr2.csv"))

# Left, 1 neighbor
mod_mr1u = lmer(monotonic_rho_bef_1 ~ stem + stem:year + (1 | didone_id), 
                data = metric_table[!is.na(metric_table$monotonic_rho_bef_1), ])

bootstrap_lmer(mod_mr1u, here::here("bootstrap_tables", "bootstrap_table_mr1u.csv"))

# Left, 2 neighbors
mod_mr2u = lmer(monotonic_rho_bef_2 ~ stem + stem:year + (1 | didone_id), 
                data = metric_table[!is.na(metric_table$monotonic_rho_bef_2), ])

bootstrap_lmer(mod_mr2u, here::here("bootstrap_tables", "bootstrap_table_mr2u.csv"))


# INTERVALLIC MEAN

# Centered, 1 neighbor
mod_im1 = lmer(interv_mean_1 ~ stem + stem:year + (1 | didone_id), 
               data = metric_table[!is.na(metric_table$interv_mean_1), ])

bootstrap_lmer(mod_im1, here::here("bootstrap_tables", "bootstrap_table_im1.csv"))

# Centered, 2 neighbors
mod_im2 = lmer(interv_mean_2 ~ stem + stem:year + (1 | didone_id), 
               data = metric_table)

bootstrap_lmer(mod_im2, here::here("bootstrap_tables", "bootstrap_table_im2.csv"))

# Left, 1 neighbor
mod_im1u = lmer(interv_mean_bef_1 ~ stem + stem:year + (1 | didone_id), 
                data = metric_table[!is.na(metric_table$interv_mean_bef_1), ])

bootstrap_lmer(mod_im1u, here::here("bootstrap_tables", "bootstrap_table_im1u.csv"))

# Left, 2 neighbors
mod_im2u = lmer(interv_mean_bef_2 ~ stem + stem:year + (1 | didone_id), 
                data = metric_table)

bootstrap_lmer(mod_im2u, here::here("bootstrap_tables", "bootstrap_table_im2u.csv"))



# Export models
save(mod_ls1, mod_ls1u, mod_ls2, mod_ls2u, 
     mod_lq1, mod_lq1u, mod_lq2, mod_lq2u, 
     mod_mr1, mod_mr1u, mod_mr2, mod_mr2u, 
     mod_im1, mod_im1u, mod_im2, mod_im2u, file = "fitted_models.RData")

# To save the tables:
fixef_to_df = function(lmer_model, out_file_path) {
  # Exports csv w/ rounded coefficients, 4 decimal digits
  fixed_eff = fixef(lmer_model)
  write_csv(data.frame(Effect = str_remove_all(names(fixed_eff), "stem"), 
                       Coefficient = round(fixed_eff, 4)), out_file_path)
}

fixef_to_df(mod_ls1, "coef_ls1.csv")
fixef_to_df(mod_ls1u, "coef_ls1u.csv")
fixef_to_df(mod_ls2, "coef_ls2.csv")
fixef_to_df(mod_ls2u, "coef_ls2u.csv")
fixef_to_df(mod_lq1, "coef_lq1.csv")
fixef_to_df(mod_lq1u, "coef_lq1u.csv")
fixef_to_df(mod_lq2, "coef_lq2.csv")
fixef_to_df(mod_lq2u, "coef_lq2u.csv")
fixef_to_df(mod_mr1, "coef_mr1.csv")
fixef_to_df(mod_mr1u, "coef_mr1u.csv")
fixef_to_df(mod_mr2, "coef_mr2.csv")
fixef_to_df(mod_mr2u, "coef_mr2u.csv")
fixef_to_df(mod_im1, "coef_im1.csv")
fixef_to_df(mod_im1u, "coef_im1u.csv")
fixef_to_df(mod_im2, "coef_im2.csv")
fixef_to_df(mod_im2u, "coef_im2u.csv")



# Tables to test significance vs. 0 (intervallic mean doesn't make 
# much sense here since it isn't distributed around zero).
fixef_central = function(lmer_model) {
  # This function is similar to lme4::fixef(), but it centers 
  # coefficients so that they account for the absolute effect (no
  # intercept). It also computes the overall effect each stem has,
  # on average, for every decade
  fixed_eff = fixef(lmer_model)
  # In our case the first stem is "addi", which is the intercept
  names(fixed_eff)[1] = "addi"
  out = numeric(0)
  
  # Remove stem from each name
  names(fixed_eff) = str_remove_all(names(fixed_eff), "stem")
  
  # First we compute the effects for "addi"
  fixed_coef = fixed_eff["addi"]
  out["addi_1730"] = fixed_coef + 1730*fixed_eff["addi:year"]
  out["addi_1740"] = fixed_coef + 1740*fixed_eff["addi:year"]
  out["addi_1750"] = fixed_coef + 1750*fixed_eff["addi:year"]
  out["addi_1760"] = fixed_coef + 1760*fixed_eff["addi:year"]
  out["addi_1770"] = fixed_coef + 1770*fixed_eff["addi:year"]
  out["addi_1780"] = fixed_coef + 1780*fixed_eff["addi:year"]
  out["addi_1790"] = fixed_coef + 1790*fixed_eff["addi:year"]
  out["addi_1800"] = fixed_coef + 1800*fixed_eff["addi:year"]
  
  for (name in names(fixed_eff)) {
    if (grepl(":", name))
      next
    if (name != "addi") {
      fixed_eff[name] = fixed_eff[name] + fixed_eff["addi"]
      fixed_coef = fixed_eff[name]
      out[paste0(name, "_1730")] = fixed_coef + 1730*fixed_eff[paste0(name, ":year")]
      out[paste0(name, "_1740")] = fixed_coef + 1740*fixed_eff[paste0(name, ":year")]
      out[paste0(name, "_1750")] = fixed_coef + 1750*fixed_eff[paste0(name, ":year")]
      out[paste0(name, "_1760")] = fixed_coef + 1760*fixed_eff[paste0(name, ":year")]
      out[paste0(name, "_1770")] = fixed_coef + 1770*fixed_eff[paste0(name, ":year")]
      out[paste0(name, "_1780")] = fixed_coef + 1780*fixed_eff[paste0(name, ":year")]
      out[paste0(name, "_1790")] = fixed_coef + 1790*fixed_eff[paste0(name, ":year")]
      out[paste0(name, "_1800")] = fixed_coef + 1800*fixed_eff[paste0(name, ":year")]
    }
  }
  
  return(out)
}

bootstrap_lmer_center = function(lmer_model, out_file_path) {
  
  # This is a custom function made to perform parametric bootstrap on 
  # a model given as argument. It outputs a written csv file with the effects,
  # according to out_file_path. This function eliminates the intercept
  # and accounts for the change in metric over time.
  
  print(Sys.time())
  
  tictoc::tic()
  
  # Calculate fixed effects
  fixed_effects = fixef(lmer_model)
  # Compute effect per year on every stem
  names(fixed_effects)[1] = "addi"
  names(fixed_effects) = str_remove_all(names(fixed_effects), "stem")
  coeff = numeric(0)
  coeff["addi_1730"] = fixed_effects["addi"] + 1730*fixed_effects["addi:year"]
  coeff["addi_1740"] = fixed_effects["addi"] + 1740*fixed_effects["addi:year"]
  coeff["addi_1750"] = fixed_effects["addi"] + 1750*fixed_effects["addi:year"]
  coeff["addi_1760"] = fixed_effects["addi"] + 1760*fixed_effects["addi:year"]
  coeff["addi_1770"] = fixed_effects["addi"] + 1770*fixed_effects["addi:year"]
  coeff["addi_1780"] = fixed_effects["addi"] + 1780*fixed_effects["addi:year"]
  coeff["addi_1790"] = fixed_effects["addi"] + 1790*fixed_effects["addi:year"]
  coeff["addi_1800"] = fixed_effects["addi"] + 1800*fixed_effects["addi:year"]
  
  for (name in names(fixed_effects)) {
    if (grepl(":", name))
      next
    if (name != "addi") {
      fixed_effects[name] = fixed_effects[name] + fixed_effects["addi"]
      fixed_coef = fixed_effects[name]
      coeff[paste0(name, "_1730")] = fixed_coef + 1730*fixed_effects[paste0(name, ":year")]
      coeff[paste0(name, "_1740")] = fixed_coef + 1740*fixed_effects[paste0(name, ":year")]
      coeff[paste0(name, "_1750")] = fixed_coef + 1750*fixed_effects[paste0(name, ":year")]
      coeff[paste0(name, "_1760")] = fixed_coef + 1760*fixed_effects[paste0(name, ":year")]
      coeff[paste0(name, "_1770")] = fixed_coef + 1770*fixed_effects[paste0(name, ":year")]
      coeff[paste0(name, "_1780")] = fixed_coef + 1780*fixed_effects[paste0(name, ":year")]
      coeff[paste0(name, "_1790")] = fixed_coef + 1790*fixed_effects[paste0(name, ":year")]
      coeff[paste0(name, "_1800")] = fixed_coef + 1800*fixed_effects[paste0(name, ":year")]
    }
  }
  
  # Bootstrap
  
  boot_t = bootMer(lmer_model, FUN = fixef_central, nsim = 1000, use.u = T, 
                   type = "parametric", parallel = "multicore")$t
  
  # Bootstrap statistics
  std_error = apply(boot_t, 2, sd)
  lower_95_CI = apply(boot_t, 2, \(x) quantile(x, 0.025, na.rm = TRUE))
  upper_95_CI = apply(boot_t, 2, \(x) quantile(x, 0.975, na.rm = TRUE))
  
  df = data.frame("Fixed_effect" = colnames(boot_t), 
                  "Coefficient" = coeff, 
                  "Std_error" = std_error, 
                  "Lower_95_CI" = lower_95_CI, 
                  "Upper_95_CI" = upper_95_CI) %>% arrange(Fixed_effect)
  
  df$p_value = normal_p(df$Coefficient, df$Std_error)
  df$Signif = significance(df$p_value)
  
  write_csv(df, out_file_path)
  
  tictoc::toc()
}


# LOCAL SLOPE 

# Centered, 1 neighbor
mod_ls1 = lmer(local_slope_1 ~ stem + stem:year + (1 | didone_id), 
               data = metric_table)

bootstrap_lmer_center(mod_ls1, here::here("bootstrap_tables", "bootstrap_table_ls1_center.csv"))

# Centered, 2 neighbors
mod_ls2 = lmer(local_slope_2 ~ stem + stem:year + (1 | didone_id), 
               data = metric_table)

bootstrap_lmer_center(mod_ls2, here::here("bootstrap_tables", "bootstrap_table_ls2_center.csv"))

# Left, 1 neighbor
mod_ls1u = lmer(local_slope_bef_1 ~ stem + stem:year + (1 | didone_id), 
                data = metric_table[!is.na(metric_table$local_slope_bef_1), ])

bootstrap_lmer_center(mod_ls1u, here::here("bootstrap_tables", "bootstrap_table_ls1u_center.csv"))

# Left, 2 neighbors
mod_ls2u = lmer(local_slope_bef_2 ~ stem + stem:year + (1 | didone_id), 
                data = metric_table[!is.na(metric_table$local_slope_bef_2), ])

bootstrap_lmer_center(mod_ls2u, here::here("bootstrap_tables", "bootstrap_table_ls2u_center.csv"))


# LOCAL QUADRATIC

# Centered, 1 neighbor
mod_lq1 = lmer(local_quadratic_1 ~ stem + stem:year + (1 | didone_id), 
               data = metric_table[!is.na(metric_table$local_quadratic_1), ])

bootstrap_lmer_center(mod_lq1, here::here("bootstrap_tables", "bootstrap_table_lq1_center.csv"))

# Centered, 2 neighbors
mod_lq2 = lmer(local_quadratic_2 ~ stem + stem:year + (1 | didone_id), 
               data = metric_table[!is.na(metric_table$local_quadratic_2), ])

bootstrap_lmer_center(mod_lq2, here::here("bootstrap_tables", "bootstrap_table_lq2_center.csv"))

# Left, 1 neighbor
mod_lq1u = lmer(local_quadratic_bef_1 ~ stem + stem:year + (1 | didone_id), 
                data = metric_table[!is.na(metric_table$local_quadratic_bef_1), ])

bootstrap_lmer_center(mod_lq1u, here::here("bootstrap_tables", "bootstrap_table_lq1u_center.csv"))

# Left, 2 neighbors
mod_lq2u = lmer(local_quadratic_bef_2 ~ stem + stem:year + (1 | didone_id), 
                data = metric_table[!is.na(metric_table$local_quadratic_bef_2), ])

bootstrap_lmer_center(mod_lq2u, here::here("bootstrap_tables", "bootstrap_table_lq2u_center.csv"))


# MONOTONIC RHO

# Centered, 1 neighbor
mod_mr1 = lmer(monotonic_rho_1 ~ stem + stem:year + (1 | didone_id), 
               data = metric_table[!is.na(metric_table$monotonic_rho_1), ])

bootstrap_lmer_center(mod_mr1, here::here("bootstrap_tables", "bootstrap_table_mr1_center.csv"))

# Centered, 2 neighbors
mod_mr2 = lmer(monotonic_rho_2 ~ stem + stem:year + (1 | didone_id), 
               data = metric_table[!is.na(metric_table$monotonic_rho_2), ])

bootstrap_lmer_center(mod_mr2, here::here("bootstrap_tables", "bootstrap_table_mr2_center.csv"))

# Left, 1 neighbor
mod_mr1u = lmer(monotonic_rho_bef_1 ~ stem + stem:year + (1 | didone_id), 
                data = metric_table[!is.na(metric_table$monotonic_rho_bef_1), ])

bootstrap_lmer_center(mod_mr1u, here::here("bootstrap_tables", "bootstrap_table_mr1u_center.csv"))

# Left, 2 neighbors
mod_mr2u = lmer(monotonic_rho_bef_2 ~ stem + stem:year + (1 | didone_id), 
                data = metric_table[!is.na(metric_table$monotonic_rho_bef_2), ])

bootstrap_lmer_center(mod_mr2u, here::here("bootstrap_tables", "bootstrap_table_mr2u_center.csv"))



# BOOTSTRAP DIFF IN DEVIANCES
bootstrap_diff_dev = function(lmer_model) {
  # First we compute the deviance of the full model, note the use of REML = FALSE
  # as models have not been fitted with ML
  dev_full = deviance(lmer_model, REML = FALSE)
  
  # Now we update the formula
  form = formula(lmer_model)
  form = update(form, ~ . - stem:year)
  
  # This removes the interaction term. Now we refit the model
  refitted = update(lmer_model, form)
  
  # Calculate the deviance of the simpler model
  dev_simple = deviance(refitted, REML = FALSE)
  
  # Output the difference in deviances
  return(dev_simple - dev_full)
}

# Now we build a custom bootstrapping function using bootMer
bootstrap_lmer_deviance = function(lmer_model, out_file_path) {
  print(Sys.time())
  tictoc::tic()
  boot_t = bootMer(lmer_model, FUN = bootstrap_diff_dev, nsim = 1000, use.u = T, 
                   type = "parametric", parallel = "multicore")$t
  
  write_csv(data.frame(boot_t), out_file_path)
  tictoc::toc()
}

bootstrap_lmer_deviance(mod_ls1, here::here("bootstrap_tables", "bootstrap_table_ls1_deviance.csv"))
bootstrap_lmer_deviance(mod_ls2, here::here("bootstrap_tables", "bootstrap_table_ls2_deviance.csv"))

bootstrap_lmer_deviance(mod_lq1, here::here("bootstrap_tables", "bootstrap_table_lq1_deviance.csv"))
bootstrap_lmer_deviance(mod_lq2, here::here("bootstrap_tables", "bootstrap_table_lq2_deviance.csv"))

bootstrap_lmer_deviance(mod_mr1, here::here("bootstrap_tables", "bootstrap_table_mr1_deviance.csv"))
bootstrap_lmer_deviance(mod_mr2, here::here("bootstrap_tables", "bootstrap_table_mr2_deviance.csv"))

bootstrap_lmer_deviance(mod_im1, here::here("bootstrap_tables", "bootstrap_table_im1_deviance.csv"))
bootstrap_lmer_deviance(mod_im2, here::here("bootstrap_tables", "bootstrap_table_im2_deviance.csv"))



# DIAGNOSTIC PLOTS (fitted vs residuals)
# Bootstrapping not needed for this, so to save
# time if only the plots are to be seen, one 
# can only do fitting and skip bootstrap altogether.

# Local slope, 1 neighbor, centered
loess_df = data.frame(fitted = fitted.values(mod_ls1), 
                      resid = residuals(mod_ls1))
loess_df$loess = predict(loess(resid ~ fitted, data = loess_df))
plot1 = ggplot(data = loess_df, aes(fitted, resid)) + 
  geom_point(color = "slateblue") + 
  theme_bw() + 
  geom_line(aes(y = loess), color = "lightsalmon", linewidth = 1.1) + 
  xlab("Fitted values") + 
  ylab("Residuals") + 
  ggtitle("Residuals vs. fitted", 
          subtitle = "Local slope. Centered neighborhood, size 1")

# Local slope, 2 neighbors, left
loess_df = data.frame(fitted = fitted.values(mod_ls2u), 
                      resid = residuals(mod_ls2u))
loess_df$loess = predict(loess(resid ~ fitted, data = loess_df))
plot2 = ggplot(data = loess_df, aes(fitted, resid)) + 
  geom_point(color = "slateblue") + 
  theme_bw() + 
  geom_line(aes(y = loess), color = "lightsalmon", linewidth = 1.1) + 
  xlab("Fitted values") + 
  ylab("Residuals") + 
  ggtitle("Residuals vs. fitted", 
          subtitle = "Local slope. Left neighborhood, size 2")

ggarrange(plot1, plot2, ncol = 1, nrow = 2)

# Local curvature, 1 neighbor, centered
loess_df = data.frame(fitted = fitted.values(mod_lq1), 
                      resid = residuals(mod_lq1))
loess_df$loess = predict(loess(resid ~ fitted, data = loess_df))
ggplot(data = loess_df, aes(fitted, resid)) + 
  geom_point(color = "slateblue") + 
  theme_bw() + 
  geom_line(aes(y = loess), color = "lightsalmon", linewidth = 1.1) + 
  xlab("Fitted values") + 
  ylab("Residuals") + 
  ggtitle("Residuals vs. fitted", 
          subtitle = "Local curvature. Centered neighborhood, size 1")

# Monotonic rho, 2 neighbors, left
loess_df = data.frame(fitted = fitted.values(mod_mr2u), 
                      resid = residuals(mod_mr2u))
loess_df$loess = predict(loess(resid ~ fitted, data = loess_df))
ggplot(data = loess_df, aes(fitted, resid)) + 
  geom_point(color = "slateblue") + 
  theme_bw() + 
  geom_line(aes(y = loess), color = "lightsalmon", linewidth = 1.1) + 
  xlab("Fitted values") + 
  ylab("Residuals") + 
  ggtitle("Residuals vs. fitted", 
          subtitle = "Monotonic rho. Left neighborhood, size 2")

# Intervallic mean, 1 neighbor, centered
loess_df = data.frame(fitted = fitted.values(mod_im1), 
                      resid = residuals(mod_im1))
loess_df$loess = predict(loess(resid ~ fitted, data = loess_df))
ggplot(data = loess_df, aes(fitted, resid)) + 
  geom_point(color = "slateblue") + 
  theme_bw() + 
  geom_line(aes(y = loess), color = "lightsalmon", linewidth = 1.1) + 
  xlab("Fitted values") + 
  ylab("Residuals") + 
  ggtitle("Residuals vs. fitted", 
          subtitle = "Intervallic mean. Centered neighborhood, size 1")


# Difference in deviances (bootstrap LRT)
# The idea is that instead of using an F distribution for the difference in 
# deviances, we use the bootstrapped distribution

bootstrap_lrt = function(lmer_model, bootstrapped_diff_dev) {
  # bootstrapped_diff_dev is a vector of bootstrapped differences in deviances
  form = formula(lmer_model)
  form = update(form, ~ . - stem:year)
  
  diff_in_deviances = deviance(update(lmer_model, form), REML = F) - 
    deviance(lmer_model, REML = F)
  
  # Now the p-value is the probability that the diff. in deviances are larger
  # than their actual value. Usually this probability is Pr(>F), but instead of
  # an F distribution we use the bootstrapped distribution
  
  return(sum(
    (diff_in_deviances > bootstrapped_diff_dev) / length(bootstrapped_diff_dev)))
}

bootstrap_lrt(mod_ls1, 
              read_csv(here::here("bootstrap_tables", "bootstrap_table_ls1_deviance.csv"), 
                       show_col_types = FALSE)$boot_t)

bootstrap_lrt(mod_ls2, 
              read_csv(here::here("bootstrap_tables", "bootstrap_table_ls2_deviance.csv"), 
                       show_col_types = FALSE)$boot_t)

bootstrap_lrt(mod_lq1, 
              read_csv(here::here("bootstrap_tables", "bootstrap_table_lq1_deviance.csv"), 
                       show_col_types = FALSE)$boot_t)
