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

# SCATTERPLOT + LINE (LOCAL SLOPE)

load(rdatas[799])

df = word_asymmetric_neighbors(aria_df, 2, 0, 407)

coef = kernel_regression_coefficients(df$distance, df$numbers, 1)

graph1 = ggplot(data = df, aes(x = distance, y = numbers)) + 
  geom_point(color = "indianred2", size = 2) + 
  theme_bw() + 
  geom_abline(intercept = coef[1], slope = coef[2], linewidth = 1.1, color = "slateblue") + 
  xlab("Signed distance to the central note") + 
  ylab("MIDI pitch") + 
  ylim(c(65, 80)) +
  ggtitle("\"Se mai turbo il tuo riposo (duetto)\", G. F. de Majo, 1766 (mm. 132-135). Cleofide", 
          "Left neighborhood (size 2), slope = -1.49, intercept = 71.82")


load(rdatas[800])
df = word_asymmetric_neighbors(aria_df, 2, 0, 418)
coef = kernel_regression_coefficients(df$distance, df$numbers, 1)

graph2 = ggplot(data = df, aes(x = distance, y = numbers)) + 
  geom_point(color = "indianred2", size = 2) + 
  theme_bw() + 
  geom_abline(intercept = coef[1], slope = coef[2], linewidth = 1.1, color = "slateblue") + 
  xlab("Signed distance to the central note") + 
  ylab("MIDI pitch") + 
  ylim(c(65, 80)) +
  ggtitle("\"Se mai turbo il tuo riposo (duetto)\", G. F. de Majo, 1766 (mm. 132-135). Poro", 
          "Left neighborhood (size 2), slope = -0.634, intercept = 70.2")

ggarrange(graph1, graph2, ncol = 1, nrow = 2)

# SCATTERPLOT + DEG 2 POLYNOMIAL (LOCAL CURVATURE)

load(rdatas[358])
df = word_neighbors(aria_df, 1, 272, T)
coef = kernel_regression_coefficients(df$distance, df$numbers, 2)
f = function(x) coef[1] + coef[2]*x + coef[3]*x^2

ggplot(data = df, aes(x = distance, y = numbers)) + 
  geom_function(fun = f, linewidth = 1.1, color = "slateblue") + 
  geom_point(color = "indianred2", size = 2) + 
  theme_bw() + 
  xlab("Signed distance to the central note") + 
  ylab("MIDI pitch") + 
  ggtitle("\"O più tremar non voglio\", C. W.Gluck, 1743 (mm. 140-142)", 
          "Centered neighborhood (size 1), quadratic term = -3, intercept = 64")

# SCATTERPLOT (MONOTONIC RHO)

load(rdatas[923])
df = word_asymmetric_neighbors(aria_df, 2, 0, 508)

ggplot(data = df, aes(x = 1:length(df$numbers), y = numbers)) + 
  geom_point(color = "indianred2", size = 2) + 
  theme_bw() + 
  xlab("Integers 1 to n") + 
  ylab("MIDI pitch") + 
  ggtitle("\"Chi vive amante sai che delira\", G. Sarti, 1766 (mm. 118-119)", 
          "Left neighborhood (size 2), monotonic rho = 0.895") + 
  scale_x_continuous(breaks = 1:length(df$numbers))



# BOXPLOTS

top_arias = (metric_table %>% count(didone_id) %>% 
                arrange(desc(n)))[1:25, ]
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
  ggtitle("Local slope in stems of 'Se mai turbo il tuo riposo' (Ale06M-Ale07M)", "Centered neighborhood, 1 neighbor") + 
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 0.3, size = 12))



one_aria = metric_table[metric_table$didone_id == top_arias$didone_id[1], ]
one_aria = one_aria[one_aria$stem %in% top_stems, ]
one_aria = one_aria[!one_aria$stem %in% c("mor", "perd"), ]
ggplot(data = one_aria, aes(x = stem, y = local_quadratic_2, fill = stem)) + 
  geom_boxplot(show.legend = F) + 
  xlab("Stems") + 
  ylab("Local curvature") + 
  ggtitle("Local curvature in stems of 'La destra ti chiedo' (Dem18M)", "Centered neighborhood, 2 neighbors") + 
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 0.3, size = 12))


one_aria = metric_table[metric_table$didone_id == top_arias$didone_id[3], ]
one_aria = one_aria[one_aria$stem %in% top_stems, ]
one_aria = one_aria[one_aria$stem != "sì", ]
ggplot(data = one_aria, aes(x = stem, y = monotonic_rho_bef_1, fill = stem)) + 
  geom_boxplot(show.legend = F) + 
  xlab("Stems") + 
  ylab("Monotonic rho") + 
  ggtitle("Monotonic rho in stems of 'Vil trofeo d'un'alma imbelle' (Ale03M)", "Left neighborhood, 1 neighbor") + 
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 0.3, size = 12))

# DISTRIBUTIONS OF METRICS

metric_table = read_csv(here::here("metric_table.csv"))

dens = \(x) dnorm(x, mean = mean(na.omit(metric_table$local_slope_1)), 
                  sd = sd(na.omit(metric_table$local_slope_1)))

ls1 = ggplot(data = metric_table, aes(x = local_slope_1)) + 
  geom_histogram(color = "black", fill = "seagreen2", 
                 binwidth = 0.20, aes(y = after_stat(density))) + 
  theme_bw() + 
  ggtitle("Local slope", "Centered neighborhood, 1 neighbor") + 
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
  ggtitle("Local slope", "Left neighborhood, 1 neighbor") + 
  xlab("Local slope") + 
  ylab("Density") + 
  xlim(c(-10, 10)) + 
  geom_function(fun = dens, colour = "slateblue4", linewidth = 1.3, 
                linetype = "dashed")


dens = \(x) dnorm(x, mean = mean(na.omit(metric_table$local_quadratic_1)), 
                  sd = sd(na.omit(metric_table$local_quadratic_1)))

lq1 = ggplot(data = metric_table, aes(x = local_quadratic_1)) + 
  geom_histogram(color = "black", fill = "indianred", 
                 binwidth = 0.35, aes(y = after_stat(density))) + 
  theme_bw() + 
  ggtitle("Local curvature", "Centered neighborhood, 1 neighbor") + 
  xlab("Local curvature") + 
  ylab("Density") + 
  geom_function(fun = dens, colour = "slateblue4", linewidth = 1.3, 
                linetype = "dashed")


dens = \(x) dnorm(x, mean = mean(na.omit(metric_table$local_quadratic_bef_1)), 
                  sd = sd(na.omit(metric_table$local_quadratic_bef_1)))

lq1b = ggplot(data = metric_table, aes(x = local_quadratic_bef_1)) + 
  geom_histogram(color = "black", fill = "indianred", 
                 binwidth = 0.35, aes(y = after_stat(density))) + 
  theme_bw() + 
  ggtitle("Local curvature", "Left neighborhood, 1 neighbor") + 
  xlab("Local curvature") + 
  ylab("Density") + 
  xlim(c(-12, 12)) + 
  geom_function(fun = dens, colour = "slateblue4", linewidth = 1.3, 
                linetype = "dashed")

ggarrange(ls1, ls1b, lq1, lq1b, nrow = 2, ncol = 2)


mr1 = ggplot(data = metric_table, aes(x = monotonic_rho_1)) + 
  geom_histogram(color = "black", fill = "gold1", 
                 binwidth = 0.05, aes(y = after_stat(density))) + 
  theme_bw() + 
  ggtitle("Monotonic rho", "Centered neighborhood, 1 neighbor") + 
  xlab("Monotonic rho") + 
  ylab("Density")


mr1b = ggplot(data = metric_table, aes(x = monotonic_rho_bef_1)) + 
  geom_histogram(color = "black", fill = "gold1", 
                 binwidth = 0.05, aes(y = after_stat(density))) + 
  theme_bw() + 
  ggtitle("Monotonic rho", "Left neighborhood, 1 neighbor") + 
  xlab("Monotonic rho") + 
  ylab("Density")


MASS::fitdistr(metric_table$interv_mean_1 + 0.01, "gamma")

dens = \(x) dgamma(x, shape = 3.6671, rate = 1.49723)

im1 = ggplot(data = metric_table, aes(x = interv_mean_1)) + 
  geom_histogram(color = "black", fill = "pink1", 
                 binwidth = 0.3, aes(y = after_stat(density))) + 
  theme_bw() + 
  ggtitle("Intervallic mean", "Centered neighborhood, 1 neighbor") + 
  xlab("Intervallic mean") + 
  ylab("Density") + 
  geom_function(fun = dens, colour = "slateblue4", linewidth = 1.3, 
                linetype = "dashed")


MASS::fitdistr(metric_table$interv_mean_bef_1 + 0.01, "gamma")

dens = \(x) dgamma(x, shape = 2.2019, rate = 0.9346)

im1b = ggplot(data = metric_table, aes(x = interv_mean_bef_1)) + 
  geom_histogram(color = "black", fill = "pink1", 
                 binwidth = 0.3, aes(y = after_stat(density))) + 
  theme_bw() + 
  ggtitle("Intervallic mean", "Left neighborhood, 1 neighbor") + 
  xlab("Intervallic mean") + 
  ylab("Density") + 
  xlim(c(0, 16)) + 
  geom_function(fun = dens, colour = "slateblue4", linewidth = 1.3, 
                linetype = "dashed")


ggarrange(mr1, mr1b, im1, im1b, ncol = 2, nrow = 2)

# BARPLOT OF STEMS
# FIRST 20 WORDS
ggplot(data = words_df[1:20, ], aes(x = reorder(word, -freq), y = freq)) + 
  geom_bar(stat = "identity", color = "black", fill = "skyblue") + 
  theme_bw() + 
  xlab("Stem") + 
  ylab("Frequency") + 
  ggtitle("20 most common stems in our corpus") + 
  theme(axis.text.x = element_text(size = 13))

# 20 HIGHEST TF-IDF
sorted_tfidf = sorted_tfidf %>% arrange(desc(tf.idf))
ggplot(data = sorted_tfidf[1:20, ], aes(x = reorder(stem, -tf.idf), y = tf.idf)) + 
  geom_bar(stat = "identity", color = "black", fill = "lightgreen") + 
  theme_bw() + 
  xlab("Stem") + 
  ylab("tf-idf") + 
  ggtitle("20 stems with highest tf-idf") + 
  theme(axis.text.x = element_text(size = 13))
