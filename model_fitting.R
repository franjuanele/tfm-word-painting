library(ggplot2)
library(tidyverse)

metric_table = read_csv(here::here("metric_table.csv"))

dens = \(x) dnorm(x, mean = mean(na.omit(metric_table$local_slope_1)), 
                  sd = sd(na.omit(metric_table$local_slope_1)))

ls1 = ggplot(data = metric_table, aes(x = local_slope_1)) + 
  geom_histogram(color = "black", fill = "seagreen2", 
                 binwidth = 0.20, aes(y = after_stat(density))) + 
  theme_bw() + 
  ggtitle("Local slope", "Centered, 1 neighbor") + 
  xlab("Local slope") + 
  ylab("Density") + 
  geom_function(fun = dens, colour = "slateblue4", linewidth = 1.3, 
                linetype = "dashed")


dens = \(x) dnorm(x, mean = mean(na.omit(metric_table$local_slope_bef_1)), 
                  sd = sd(na.omit(metric_table$local_slope_bef_1)))

ls1b = ggplot(data = metric_table, aes(x = local_slope_bef_1)) + 
  geom_histogram(color = "black", fill = "seagreen2", 
                 binwidth = 0.35, aes(y = after_stat(density))) + 
  theme_bw() + 
  ggtitle("Local slope", "Uncentered, 1 neighbor") + 
  xlab("Local slope") + 
  ylab("Density") + 
  geom_function(fun = dens, colour = "slateblue4", linewidth = 1.3, 
                linetype = "dashed")
ls1b

dens = \(x) dnorm(x, mean = mean(na.omit(metric_table$local_quadratic_1)), 
                  sd = sd(na.omit(metric_table$local_quadratic_1)))

lq1 = ggplot(data = metric_table, aes(x = local_quadratic_1)) + 
  geom_histogram(color = "black", fill = "indianred", 
                 binwidth = 0.35, aes(y = after_stat(density))) + 
  theme_bw() + 
  ggtitle("Local quadratic", "Centered, 1 neighbor") + 
  xlab("Local quadratic") + 
  ylab("Density") + 
  geom_function(fun = dens, colour = "slateblue4", linewidth = 1.3, 
                linetype = "dashed")

lq1

dens = \(x) dnorm(x, mean = mean(na.omit(metric_table$local_quadratic_bef_1)), 
                  sd = sd(na.omit(metric_table$local_quadratic_bef_1)))

lq1b = ggplot(data = metric_table, aes(x = local_quadratic_bef_1)) + 
  geom_histogram(color = "black", fill = "indianred", 
                 binwidth = 0.35, aes(y = after_stat(density))) + 
  theme_bw() + 
  ggtitle("Local quadratic", "Uncentered, 1 neighbor") + 
  xlab("Local quadratic") + 
  ylab("Density") + 
  geom_function(fun = dens, colour = "slateblue4", linewidth = 1.3, 
                linetype = "dashed")

lq1b

mr1 = ggplot(data = metric_table, aes(x = monotonic_rho_1)) + 
  geom_histogram(color = "black", fill = "gold1", 
                 binwidth = 0.03, aes(y = after_stat(density))) + 
  theme_bw() + 
  ggtitle("Monotonic rho", "Centered, 1 neighbor") + 
  xlab("Monotonic rho") + 
  ylab("Density")

mr1

mr1b = ggplot(data = metric_table, aes(x = monotonic_rho_bef_1)) + 
  geom_histogram(color = "black", fill = "gold1", 
                 binwidth = 0.04, aes(y = after_stat(density))) + 
  theme_bw() + 
  ggtitle("Monotonic rho", "Uncentered, 1 neighbor") + 
  xlab("Monotonic rho") + 
  ylab("Density")

mr1b

MASS::fitdistr(metric_table$interv_mean_1 + 0.01, "gamma")

dens = \(x) dgamma(x, shape = 3.6671, rate = 1.49723)

im1 = ggplot(data = metric_table, aes(x = interv_mean_1)) + 
  geom_histogram(color = "black", fill = "pink1", 
                 binwidth = 0.3, aes(y = after_stat(density))) + 
  theme_bw() + 
  ggtitle("Intervallic mean", "Centered, 1 neighbor") + 
  xlab("Intervallic mean") + 
  ylab("Density") + 
  geom_function(fun = dens, colour = "slateblue4", linewidth = 1.3, 
                linetype = "dashed")

im1

MASS::fitdistr(metric_table$interv_mean_bef_1 + 0.01, "gamma")

dens = \(x) dgamma(x, shape = 2.2019, rate = 0.9346)

im1b = ggplot(data = metric_table, aes(x = interv_mean_bef_1)) + 
  geom_histogram(color = "black", fill = "pink1", 
                 binwidth = 0.25, aes(y = after_stat(density))) + 
  theme_bw() + 
  ggtitle("Intervallic mean", "Uncentered, 1 neighbor") + 
  xlab("Intervallic mean") + 
  ylab("Density") + 
  geom_function(fun = dens, colour = "slateblue4", linewidth = 1.3, 
                linetype = "dashed")

im1b

library(ggpubr)
ggarrange(mr1, mr1b, im1, im1b, ncol = 2, nrow = 2)


################################################
################################################
################################################
################################################
################################################
################################################
################################################
################################################
################################################
################################################
################################################

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

metric_table = read_csv("metric_table.csv", show_col_types = F)
sorted_tfidf = read_csv("sorted_tfidf.csv", show_col_types = F)
top_stems = sorted_tfidf$stem[sorted_tfidf$tf.idf > 1700]

(top_arias = (metric_table %>% count(didone_id) %>% 
                arrange(desc(n)))[1:25, ])
top_arias$aria_name = top_arias$didone_id
for (j in 1:25) {
  top_arias$aria_name[j] = 
    metric_table$aria_name[which(metric_table$didone_id == 
                                   top_arias$aria_name[j])[1]]
}
one_aria = metric_table[metric_table$didone_id == top_arias$didone_id[2], ]
one_aria = one_aria[one_aria$stem %in% top_stems, ]

ggplot(data = one_aria, aes(x = stem, y = local_slope_1, fill = stem)) + 
  geom_boxplot(show.legend = F) + 
  xlab("Stems") + 
  ylab("Local slope") + 
  ggtitle("Local slope in stems of 'Se mai turbo il tuo riposo' (Ale06M-Ale07M)", "Centered, 1 neighbor") + 
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 0.3))



one_aria = metric_table[metric_table$didone_id == top_arias$didone_id[1], ]
one_aria = one_aria[one_aria$stem %in% top_stems, ]
one_aria = one_aria[!one_aria$stem %in% c("mor", "perd"), ]
ggplot(data = one_aria, aes(x = stem, y = local_quadratic_2, fill = stem)) + 
  geom_boxplot(show.legend = F) + 
  xlab("Stems") + 
  ylab("Local quadratic") + 
  ggtitle("Local quadratic in stems of 'La destra ti chiedo' (Dem18M)", "Centered, 2 neighbors") + 
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 0.3))


one_aria = metric_table[metric_table$didone_id == top_arias$didone_id[3], ]
one_aria = one_aria[one_aria$stem %in% top_stems, ]
one_aria = one_aria[one_aria$stem != "sì", ]
ggplot(data = one_aria, aes(x = stem, y = monotonic_rho_bef_1, fill = stem)) + 
  geom_boxplot(show.legend = F) + 
  xlab("Stems") + 
  ylab("Monotonic rho") + 
  ggtitle("Monotonic rho in stems of 'Vil trofeo d'un'alma imbelle' (Ale03M)", "Uncentered, 1 neighbor") + 
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 0.3))


################################################
################################################
################################################
################################################
################################################
################################################
################################################
################################################
################################################
################################################
################################################

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

# Uncentered, 1 neighbor
mod_ls1u = lmer(local_slope_bef_1 ~ stem + stem:year + (1 | didone_id), 
                data = metric_table[!is.na(metric_table$local_slope_bef_1), ])

bootstrap_lmer(mod_ls1u, here::here("bootstrap_tables", "bootstrap_table_ls1u.csv"))

# Uncentered, 2 neighbors
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

# Uncentered, 1 neighbor
mod_lq1u = lmer(local_quadratic_bef_1 ~ stem + stem:year + (1 | didone_id), 
                data = metric_table[!is.na(metric_table$local_quadratic_bef_1), ])

bootstrap_lmer(mod_lq1u, here::here("bootstrap_tables", "bootstrap_table_lq1u.csv"))

# Uncentered, 2 neighbors
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

# Uncentered, 1 neighbor
mod_mr1u = lmer(monotonic_rho_bef_1 ~ stem + stem:year + (1 | didone_id), 
                data = metric_table[!is.na(metric_table$monotonic_rho_bef_1), ])

bootstrap_lmer(mod_mr1u, here::here("bootstrap_tables", "bootstrap_table_mr1u.csv"))

# Uncentered, 2 neighbors
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

# Uncentered, 1 neighbor
mod_im1u = lmer(interv_mean_bef_1 ~ stem + stem:year + (1 | didone_id), 
                data = metric_table[!is.na(metric_table$interv_mean_bef_1), ])

bootstrap_lmer(mod_im1u, here::here("bootstrap_tables", "bootstrap_table_im1u.csv"))

# Uncentered, 2 neighbors
mod_im2u = lmer(interv_mean_bef_2 ~ stem + stem:year + (1 | didone_id), 
                data = metric_table)

bootstrap_lmer(mod_im2u, here::here("bootstrap_tables", "bootstrap_table_im2u.csv"))



# DIAGNOSTIC PLOTS (fitted vs residuals)
# Bootstrapping not needed for this, so to save
# time if only the plots are to be seen, one 
# can only do fitting and skip bootstrap_lmer.

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
          subtitle = "Local slope. 1 neighbor, centered")

# Local slope, 2 neighbors, uncentered
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
          subtitle = "Local slope. 2 neighbors, uncentered")

ggarrange(plot1, plot2, ncol = 1, nrow = 2)

# Local quadratic, 1 neighbor, centered
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
          subtitle = "Local quadratic. 1 neighbor, centered")

# Monotonic rho, 2 neighbors, uncentered
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
          subtitle = "Monotonic rho. 2 neighbors, uncentered")

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
          subtitle = "Intervallic mean. 1 neighbor, centered")
