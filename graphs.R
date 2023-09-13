# GRAPHS

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
library(ggpubr)

metric_table = read_csv("metric_table.csv", show_col_types = F)
sorted_tfidf = read_csv("sorted_tfidf.csv", show_col_types = F)
top_stems = sorted_tfidf$stem[sorted_tfidf$tf.idf > 1700]


# BOXPLOTS

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

# DISTRIBUTIONS OF METRICS

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


mr1 = ggplot(data = metric_table, aes(x = monotonic_rho_1)) + 
  geom_histogram(color = "black", fill = "gold1", 
                 binwidth = 0.03, aes(y = after_stat(density))) + 
  theme_bw() + 
  ggtitle("Monotonic rho", "Centered, 1 neighbor") + 
  xlab("Monotonic rho") + 
  ylab("Density")


mr1b = ggplot(data = metric_table, aes(x = monotonic_rho_bef_1)) + 
  geom_histogram(color = "black", fill = "gold1", 
                 binwidth = 0.04, aes(y = after_stat(density))) + 
  theme_bw() + 
  ggtitle("Monotonic rho", "Uncentered, 1 neighbor") + 
  xlab("Monotonic rho") + 
  ylab("Density")


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


ggarrange(mr1, mr1b, im1, im1b, ncol = 2, nrow = 2)