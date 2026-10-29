############################################################
#                    AXIS 2 - REDDIT
#      Word frequencies - Word clouds - Clustering
#      (adapted from the course script Textm_26.R)
############################################################
# Corpus: Reddit posts in which users describe relying on AI to
# decide (corpus/reddit_corpus.csv, created by 02_clean_and_filter.R).
#
# Run from the project folder:
#   Rscript scripts/03_exploration.R
# Outputs: results/exploration/


#============================================================
# 0. PACKAGES
#============================================================

library(tm)
library(wordcloud)
library(RColorBrewer)
library(tidyverse)
library(tidytext)
library(factoextra)
library(writexl)

source("scripts/plot_style.R")

out_dir <- "results/exploration"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)


#============================================================
# 1. LOADING THE CORPUS
#============================================================

posts <- read_csv("corpus/reddit_corpus.csv", show_col_types = FALSE)

# Lemmatised texts (02c_lemmatise.R), in the order of the posts
lemmas <- read_csv("corpus/corpus_lemmas.csv", show_col_types = FALSE)
posts <- posts %>% left_join(lemmas %>% select(id, text_lemmas), by = "id")

cat("Posts:", nrow(posts), "\n")
print(table(posts$audience))

# Turn the lemmatised text into a corpus (one document = one post)
corpus <- Corpus(VectorSource(posts$text_lemmas))


#============================================================
# 2. TEXT PREPROCESSING
#============================================================
# The corpus is in English and already lemmatised ("felt" -> "feel"):
# English stop words, punctuation and numbers are removed.

# Lower case and straight apostrophes
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, content_transformer(function(x) gsub("[\u2018\u2019]", "'", x)))

# Remove English stop words (tm list + the more complete tidytext list)
stop_list <- unique(c(stopwords("en"), stop_words$word))
corpus <- tm_map(corpus, removeWords, stop_list)

# Remove punctuation, numbers and symbols (arrows, emojis)
corpus <- tm_map(corpus, removePunctuation, ucp = TRUE)
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, content_transformer(function(x) gsub("[^[:alpha:][:space:]]", " ", x)))


#------------------------------------------------------------
# Extra uninformative words and search words
#------------------------------------------------------------
# Lists in word_lists.R (shared with 04_topic_models.R).
# The search words are counted separately (table 4b).

source("scripts/word_lists.R")

corpus_with_query <- corpus

corpus <- tm_map(corpus, removeWords, c(extra_stopwords, query_words))

# Remove extra spaces
corpus <- tm_map(corpus, stripWhitespace)


#============================================================
# 3. DOCUMENT-TERM MATRIX
#============================================================
# Words of 3 letters or more (in English, "job", "app", "gpa" carry
# meaning; the course keeps 4 letters or more for French)

course_dtm <- DocumentTermMatrix(corpus, control = list(wordLengths = c(3, Inf)))

course_dtm


#============================================================
# 4. WORD FREQUENCY TABLE
#============================================================

word_freq <- sort(slam::col_sums(course_dtm), decreasing = TRUE)

df_freq <- data.frame(
  word = names(word_freq),
  freq = as.numeric(word_freq),
  # number of posts in which the word appears
  posts = as.numeric(slam::col_sums(course_dtm > 0)[names(word_freq)])
)

freq10 <- head(df_freq, 10)
print(freq10)


#------------------------------------------------------------
# 4b. The search words (counted before they are removed)
#------------------------------------------------------------

dtm_query <- DocumentTermMatrix(
  corpus_with_query,
  control = list(dictionary = query_words, wordLengths = c(2, Inf))
)

freq_query <- data.frame(
  word = colnames(dtm_query),
  freq = as.numeric(slam::col_sums(dtm_query)),
  posts = as.numeric(slam::col_sums(dtm_query > 0))
) %>%
  filter(freq > 0) %>%
  arrange(desc(freq))

print(freq_query)


#============================================================
# 5. PLOT OF THE 20 MOST FREQUENT WORDS
#============================================================

top20 <- df_freq %>%
  slice_max(order_by = freq, n = 20, with_ties = FALSE) %>%
  mutate(word = reorder(word, freq))

plot_freq <- ggplot(top20, aes(x = word, y = freq)) +
  geom_col(fill = "#2471A3") +
  coord_flip() +
  labs(
    title = "The 20 most frequent words",
    subtitle = paste(nrow(posts), "Reddit posts, search words removed"),
    x = NULL,
    y = "Frequency"
  ) +
  theme_report()

ggsave(file.path(out_dir, "top20_words.png"), plot_freq, width = 7, height = 6, dpi = 300)


#------------------------------------------------------------
# 5b. Most frequent words by audience
#------------------------------------------------------------
# "AI" audience: subreddits about AI; "Everyday": subreddits where
# people talk about their everyday decisions

freq_audience <- tibble(doc = seq_len(nrow(posts)), audience = posts$audience) %>%
  inner_join(tidy(course_dtm) %>% mutate(doc = as.integer(document)), by = "doc") %>%
  count(audience, term, wt = count, name = "freq") %>%
  group_by(audience) %>%
  slice_max(freq, n = 15, with_ties = FALSE) %>%
  ungroup()

plot_audience <- freq_audience %>%
  mutate(term = reorder_within(term, freq, audience)) %>%
  ggplot(aes(x = term, y = freq, fill = audience)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  scale_x_reordered() +
  facet_wrap(~ audience, scales = "free") +
  scale_fill_manual(values = audience_colours) +
  labs(title = "The 15 most frequent words by audience", x = NULL, y = "Frequency") +
  theme_report()

ggsave(file.path(out_dir, "top15_words_by_audience.png"), plot_audience, width = 9, height = 6, dpi = 300)


#============================================================
# 6. WORD CLOUD
#============================================================

word_pal <- brewer.pal(8, "Dark2")

png(file.path(out_dir, "wordcloud.png"), width = 2000, height = 2000, res = 300)
set.seed(123)
wordcloud(
  words = df_freq$word,
  freq = df_freq$freq,
  min.freq = 2,
  max.words = 100,
  random.order = FALSE,
  rot.per = 0.10,
  scale = c(3, 0.5),
  colors = word_pal
)
dev.off()


#------------------------------------------------------------
# 6b. One cloud per audience, to compare them (as in the course
# practical, which draws one cloud per phase)
#------------------------------------------------------------

freq_by_audience <- tibble(doc = seq_len(nrow(posts)), audience = posts$audience) %>%
  inner_join(tidy(course_dtm) %>% mutate(doc = as.integer(document)), by = "doc") %>%
  count(audience, term, wt = count, name = "freq")

for (g in c("AI", "Everyday")) {
  f <- freq_by_audience %>% filter(audience == g) %>% arrange(desc(freq))
  png(file.path(out_dir, paste0("wordcloud_", tolower(g), ".png")), width = 2000, height = 2000, res = 300)
  set.seed(123)
  wordcloud(
    words = f$term,
    freq = f$freq,
    min.freq = 2,
    max.words = 80,
    random.order = FALSE,
    rot.per = 0.10,
    scale = c(3, 0.5),
    colors = word_pal
  )
  dev.off()
}


#============================================================
# 7. SELECTING THE WORDS FOR CLUSTERING
#============================================================
# The course keeps words with frequency > 2 in a small corpus. Here
# the vocabulary has thousands of words: a dendrogram would be
# unreadable. The 40 most frequent words are kept.

n_words_hc <- 40

top_words_hc <- df_freq %>%
  slice_max(freq, n = n_words_hc, with_ties = FALSE)


#============================================================
# 8. HIERARCHICAL CLUSTERING
#============================================================

# Keep only the selected words in the matrix
dtm_selected <- course_dtm[, colnames(course_dtm) %in% top_words_hc$word]

# Presence (1) or absence (0) of each word in each post
word_matrix <- as.matrix(dtm_selected) > 0

# Distance between words.
# The course's Euclidean distance on counts groups the words here by
# frequency (one group of 33 words, one group of 6 of the most frequent
# words, and "feel" alone; section 8b): Reddit posts vary a lot in
# length. We measure instead whether two words appear in the same
# posts: Jaccard distance (method = "binary").
distance <- dist(t(word_matrix), method = "binary")

# Hierarchical clustering with Ward's method
hc <- hclust(distance, method = "ward.D2")


#============================================================
# 9-10. DENDROGRAM WITH k CLUSTERS
#============================================================

k_hc <- 3

plot_clusters <- fviz_dend(
  hc,
  k = k_hc,
  rect = TRUE,
  horiz = TRUE,
  cex = 0.8,
  main = paste("Hierarchical clustering of the", n_words_hc, "most frequent words"),
  ylab = "Height (Jaccard distance, Ward's method)"
)

# The height axis shows positive values only (the label area, on the
# right, has no meaning)
plot_clusters <- plot_clusters +
  scale_y_reverse(breaks = seq(0, 1.5, by = 0.5)) +
  theme(plot.title = element_text(size = 11), axis.title = element_text(size = 9))

# Compact format: once the figure is shrunk to the report width, the
# word labels stay readable
ggsave(file.path(out_dir, "dendrogram_jaccard.png"), plot_clusters, width = 5.5, height = 6.6, dpi = 300)
ggsave(file.path(out_dir, "dendrogram_jaccard.pdf"), plot_clusters, width = 5.5, height = 6.6, device = pdf)

# Vertical version for slides (words at the bottom, readable at full width)
plot_vertical <- fviz_dend(hc, k = k_hc, rect = TRUE, horiz = FALSE, cex = 0.95, main = NULL, ylab = "",
                           labels_track_height = 1.1) +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())
ggsave(file.path(out_dir, "dendrogram_vertical.pdf"), plot_vertical, width = 8, height = 3.4, device = pdf)


#------------------------------------------------------------
# 8b. The course method: Euclidean distance on counts
#------------------------------------------------------------
# Same code as Textm_26.R (default dist(), Ward, 3 groups), on the
# same 40 words, to compare with the Jaccard distance.

distance_eucl <- dist(t(as.matrix(dtm_selected)))
hc_eucl <- hclust(distance_eucl, method = "ward.D2")

groups_eucl <- cutree(hc_eucl, k = k_hc)
print(table(groups_eucl))

clusters_eucl <- data.frame(word = names(groups_eucl), cluster = groups_eucl) %>%
  left_join(df_freq %>% select(word, frequency = freq), by = "word") %>%
  arrange(cluster, desc(frequency))

plot_eucl <- fviz_dend(
  hc_eucl,
  k = k_hc,
  rect = TRUE,
  horiz = TRUE,
  cex = 0.8,
  main = "Euclidean distance (course method)",
  ylab = "Height (Euclidean distance, Ward's method)"
) +
  theme(plot.title = element_text(size = 11), axis.title = element_text(size = 9))

ggsave(file.path(out_dir, "dendrogram_euclidean.png"), plot_eucl, width = 5.5, height = 6.6, dpi = 300)
ggsave(file.path(out_dir, "dendrogram_euclidean.pdf"), plot_eucl, width = 5.5, height = 6.6, device = pdf)


#============================================================
# 11. WORDS IN EACH CLUSTER
#============================================================

groups <- cutree(hc, k = k_hc)

clusters_hc <- data.frame(word = names(groups), cluster = groups) %>%
  left_join(df_freq %>% select(word, frequency = freq), by = "word") %>%
  arrange(cluster, desc(frequency))

print(clusters_hc)


#============================================================
# 12. EXPORTING THE TABLES
#============================================================

write_xlsx(
  list(
    frequencies = df_freq,
    search_words = freq_query,
    top15_by_audience = freq_audience,
    clusters_jaccard = clusters_hc,
    clusters_euclidean = clusters_eucl
  ),
  file.path(out_dir, "exploration_axis2.xlsx")
)

cat("\nResults in", out_dir, "\n")
