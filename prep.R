# Change working directory as needed (root of GitHub repository works)
setwd("D:/Matemáticas/Máster/TFM")

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
library(xml2)
library(lme4)
library(mclust)
library(readxl)
library(MASS)

# Load required functions and files
source(here::here("df_fun.R"))
source(here::here("tm_fun.R"))
source(here::here("gm_fun.R")) # these are not that important
source(here::here("metric_fun.R"))

stopwords = read_lines("stopwords.txt")

lemmatization_it = read_lines(here::here("michmech-italian-stemming",
                                         "lemmatization-it.txt"))
lemmatization_it_stems = stringr::word(lemmatization_it, 1)
lemmatization_it_words = stringr::word(lemmatization_it, -1)
names(lemmatization_it_stems) = lemmatization_it_words

rm(lemmatization_it, lemmatization_it_words)

custom_stemming = read_lines(here::here("custom_stemming.txt"))
custom_before = stringr::word(custom_stemming, 1)
custom_after = stringr::word(custom_stemming, -1)
names(custom_after) = custom_before
rm(custom_stemming, custom_before)

# The following is for the MusicXML files, they are not public
xmls = list.files(
  path = here::here("XML"),
  pattern = "*.xml",
  full.names = TRUE,
  recursive = FALSE
)

# Loading RDatas, placed in ./RDatas
rdatas = list.files(
  path = here::here("RDatas"), 
  pattern = "*.RData", 
  full.names = TRUE, 
  recursive = FALSE
)

# How many parts are we left with?
length(rdatas)

load(rdatas[800])

# ============================= #
# ======   TEXT MINING   ====== #
# ============================= #

# This is how we get the text out of a music df
raw_text
raw_text = get_raw_text(aria_df) 

# =============================== #
# ==  ITERATING THROUGH XML'S  == #
# =============================== #

# To use this, the MusicXML files are needed!
export_lyrics(xmls[1501:1761])

# Warning: it takes about 1 minute to parse ~ 10 arias, so it could take a while
# If one wants to get the lyrics of all the corpus, get_raw_text can be used
# in a loop running through all the RDatas.

# ======================================== #
# =====  TEXT MINING II + WORDCLOUD  ===== #
# ======================================== #

directory_source = DirSource(
  directory = here::here("Lyrics"),
  recursive = FALSE,
  mode = "text"
)

corp = SimpleCorpus(directory_source, control = list(language = "it"))

# Remove punctuations
corp = tm_map(corp, content_transformer(remove_apostrophe)) # remove weird '
# Note that removePunctuation removes the regular apostrophe ', but not U+2019.

corp = tm_map(corp, content_transformer(remove_ellipsis)) # remove ...
# Note that removePunctuation removes single dots but not U+2026 ellipsis.

corp = tm_map(corp, content_transformer(remove_quotations))
# remove curly double quotations, that removePunctuation doesn't remove

corp = tm_map(corp, removePunctuation)

# To lowercase

corp = tm_map(corp, content_transformer(tolower))

# Remove stopwords
corp = tm_map(corp, removeWords, words = c(stopwords))

# Remove strips of >1 white spaces
corp = tm_map(corp, stripWhitespace)
corp = tm_map(corp, content_transformer(str_trim))

# Stemming

# First using lemmatization from github.com/michmech

# lemmatization_it_stems is like a dictionary, where
# we get a stem as lemmatization_it_stems["word"]

corp_lemm = tm_map(corp, lemmatizer, dict = lemmatization_it_stems)

# Now using snowball stemming
corp_stem = tm_map(corp_lemm, stemDocument, language = "italian")

# Correcting using custom stemming
# For some reason tm_map doesn't work (?)

for (i in 1:length(corp_stem)) {
  corp_stem[[i]]$content = lemmatizer(corp_stem[[i]]$content, custom_after)
}

# Document Term Matrix

dtm = DocumentTermMatrix(corp_stem)
dtm_matrix = as.matrix(dtm)

words = sort(colSums(dtm_matrix), decreasing = TRUE)

words_df = data.frame(word = names(words), freq = words)

# This words_df is used in the graphs.R file for the barplot!

# Wordcloud. This allows us to visualize what stems are most frequent.

par(mfrow = c(1, 1))
wordcloud(
  words = words_df$word,
  freq = words_df$freq,
  min.freq = 1,
  max.words = 300,
  random.order = F,
  rot.per = 0.1,
  colors = brewer.pal(8, "Dark2"),
  scale = c(4, 0.2)
)

# ==================================== #
# ======= TF-IDF + FREQUENCIES ======= #
# ==================================== #

tfidf = DocumentTermMatrix(corp_stem, control = list(
  weighting = function(x) weightTfIdf(x, normalize = F)))

sorted_tfidf = sort(colSums(as.matrix(tfidf)))

write_csv(data.frame("stem" = names(sorted_tfidf), 
                     "tf-idf" = sorted_tfidf), 
          "sorted_tfidf.csv")

hist(sorted_tfidf, breaks = 50, col = "slateblue", 
     main = "Tf-idf", xlab = "Tf-idf")

# We will only retain those with tfidf > 1700

stems = names(sorted_tfidf[sorted_tfidf > 1700])
length(stems)

# These represent about 5.2% of the total stems, 65 out of 1240.
quantile(sorted_tfidf, 0.948)

# Words with frequencies

dtm_normal = DocumentTermMatrix(corp)
dtm_normal_matrix = as.matrix(dtm_normal)
rm(dtm_normal)

dtm_lemm_matrix = dtm_matrix
rm(dtm_matrix)

dtm_stem = DocumentTermMatrix(corp_stem)
dtm_stem_matrix = as.matrix(dtm_stem)
rm(dtm_stem)

words_normal = sort(colSums(dtm_normal_matrix), decreasing = TRUE)
words_normal_df = data.frame(word = names(words_normal), freq = words_normal)

words_lemm = sort(colSums(dtm_lemm_matrix), decreasing = TRUE)
words_lemm_df = data.frame(word = names(words_lemm), freq = words_lemm)

words_stem = sort(colSums(dtm_stem_matrix), decreasing = TRUE)
words_stem_df = data.frame(word = names(words_stem), freq = words_stem)

write_csv(words_normal_df, "Raw_freq_table.csv")
write_csv(words_lemm_df, "Lemmatization_freq_table.csv")
write_csv(words_stem_df, "Stemming_freq_table.csv")

# ============================= #
# =======    METRICS    ======= #
# ============================= #

# The following function exports metric_table.csv
metrics_tidy_csv(rdatas, stems = stems)

print(object.size(metric_table_1), units = "Mb")

hist(metric_table_1$local_slope, breaks = 200, prob = T)

curve(dnorm(x, mean = mean(metric_table_1$local_slope),
            sd = sd(metric_table_1$local_slope)), add = T, 
      col = 2, lwd = 3)

hist(metric_table_1$local_quadratic_term, breaks = 150, prob = T)
curve(dnorm(x, mean = mean(na.omit(metric_table_1$local_quadratic_term)),
            sd = sd(na.omit(metric_table_1$local_quadratic_term))), add = T, 
      col = 2, lwd = 3)

plot(ecdf(metric_table_1$local_slope))

qqnorm(metric_table_1$local_slope)
qqline(metric_table_1$local_slope)

hist(metric_table_1$monotonic_behavior)

hist(metric_table_1$intervallic_mean, breaks = 50, prob = T)

aoverb = mean(metric_table_1$intervallic_mean)
aoverbsq = var(metric_table_1$intervallic_mean)

b = aoverb/aoverbsq

a = aoverb * b

curve(dgamma(x, shape = a, rate = b), col = 2, lwd = 3, add = T)

library(MASS)

fitdistr(metric_table_1$intervallic_mean + 0.01, "lognormal")

curve(dlnorm(x, 0.728, 0.6782), col = 3, lwd = 3, add = T)

stem_freq = metric_table %>% count(stem) %>% arrange(desc(n))

# =============================== #
# ======= EXPORTING RDATA ======= #
# =============================== #

# Here we exported RData files. We did so in "chunks" of 300-400 MusicXMLs
# at a time (due to the memory leaking bug). This function needs the XML
# files, and its output in the whole corpus is the RDatas folder.

export_rdata_files(xmls[1601:1761])

# =========================================== #
# ===== GETTING SCORE BACK (PACKAGE gm) ===== #
# =========================================== #

# This part needs the MusicXML files as notes_no_grace is not a file
# present in the RDatas. However, a couple of example RDatas
# (x and y) are left in the root of the GitHub repository (NOT in the
# RDatas folder) with example "notes" so that this part can be seen to 
# work correctly. The output doesn't contain grace notes, title or lyrics
# because these are not supported by the gm package.

get_score(notes_no_grace, anacrusis_duration, type_voice)

