# ================================================================
#  AXIS 2 - REDDIT
#  Topic models: LDA, then two extensions (STM and BTM)
#  (adapted from the course scripts LDA_28.R and TP_cours28.R)
# ================================================================
# Steps:
#  1. Load the corpus
#  2. Clean the texts (English)
#  3. Build the document-term matrix
#  4. Choose the number of topics K
#  5. Fit the LDA model
#  6. BETA: words that characterise each topic
#  7. GAMMA: topics in each post, dominant topic
#  8. Compare topics by audience and by year
#  9. Evaluate the model (perplexity, coherence)
# 10. Extension 1: STM (effect of audience and year on the topics)
# 11. Extension 2: BTM on the titles (short texts)
#
# Run from the project folder:
#   Rscript scripts/04_topic_models.R
# Outputs: results/topics/


# ================================================================
# 0. PACKAGES
# ================================================================

library(tidyverse)
library(tidytext)
library(tm)
library(topicmodels)
library(ldatuning)
library(textmineR)
library(stm)
library(BTM)
library(textplot)   # BTM biterm plot (with concaveman and ggraph)
library(writexl)

source("scripts/word_lists.R")
source("scripts/plot_style.R")

out_dir <- "results/topics"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)


# ================================================================
# 1. LOADING THE CORPUS
# ================================================================

posts <- read_csv("corpus/reddit_corpus.csv", show_col_types = FALSE)

cat("Posts:", nrow(posts), "\n")


# ================================================================
# 2. CLEANING THE CORPUS
# ================================================================

# 2.1 Lemmatised texts (02c_lemmatise.R: "felt" -> "feel",
# "studies" -> "study"), in lower case
lemmas <- read_csv("corpus/corpus_lemmas.csv", show_col_types = FALSE)
posts_clean <- posts %>%
  left_join(lemmas %>% select(id, text_lemmas), by = "id") %>%
  mutate(text_clean = str_to_lower(text_lemmas) %>% str_replace_all("[\u2018\u2019]", "'"))

# 2.2 Tokenisation
corpus_tidy <- posts_clean %>%
  select(id, text_clean) %>%
  unnest_tokens(output = word, input = text_clean, token = "words")

# 2.3 English stop words, generic words, search words
# (the possessive "'s" is removed first: "chatgpt's" -> "chatgpt")
corpus_tidy <- corpus_tidy %>%
  mutate(word = str_remove(word, "'s$")) %>%
  anti_join(stop_words, by = "word") %>%
  filter(!word %in% c(extra_stopwords, query_words))

# 2.4 Alphabetic words of 3 letters or more
corpus_tidy <- corpus_tidy %>%
  mutate(word = str_remove_all(word, "'")) %>%
  filter(str_detect(word, "^[a-z]+$"), nchar(word) >= 3)

# 2.5 Rare words: present in fewer than 5 posts. They cannot define a
# topic and slow down the estimation.
kept_words <- corpus_tidy %>%
  distinct(id, word) %>%
  count(word) %>%
  filter(n >= 5)

corpus_tidy <- corpus_tidy %>% semi_join(kept_words, by = "word")

cat("Tokens kept:", nrow(corpus_tidy), "\n")
cat("Distinct words:", n_distinct(corpus_tidy$word), "\n")


# ================================================================
# 3. DOCUMENT-TERM MATRIX
# ================================================================

dtm <- corpus_tidy %>%
  count(id, word) %>%
  cast_dtm(document = id, term = word, value = n)

cat("DTM:", nDocs(dtm), "posts x", nTerms(dtm), "terms\n")

# Post variables, in the order of the DTM rows
meta <- tibble(id = Docs(dtm)) %>%
  left_join(posts %>% select(id, audience, year, title, text, subreddit), by = "id")


# ================================================================
# 4. CHOOSING THE NUMBER OF TOPICS K
# ================================================================
# Griffiths2004 and Deveaud2014: higher is better
# CaoJuan2009 and Arun2010: lower is better
# The final choice also takes into account how interpretable the
# topics are.

k_metrics <- FindTopicsNumber(
  dtm,
  topics = 2:12,
  metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
  method = "Gibbs",
  control = list(seed = 77),
  mc.cores = 4,
  verbose = FALSE
)

print(k_metrics)

# The course plot (FindTopicsNumber_plot), redrawn: each criterion is
# rescaled to 0-1
K <- 10   # chosen K (justified in section 5)

plot_k <- k_metrics %>%
  pivot_longer(-topics, names_to = "criterion", values_to = "value") %>%
  group_by(criterion) %>%
  mutate(value = (value - min(value)) / (max(value) - min(value))) %>%
  ungroup() %>%
  mutate(direction = ifelse(criterion %in% c("Griffiths2004", "Deveaud2014"), "Criteria to maximise", "Criteria to minimise")) %>%
  ggplot(aes(topics, value, shape = criterion, linetype = criterion)) +
  geom_vline(xintercept = K, colour = "#E67E22", linewidth = 1, alpha = 0.6) +
  geom_line() +
  geom_point(size = 2.5) +
  facet_wrap(~ direction, ncol = 1) +
  scale_x_continuous(breaks = 2:12) +
  labs(
    title = "Choosing the number of topics K",
    subtitle = paste0("Values rescaled to 0-1; in orange, the chosen K = ", K),
    x = "Number of topics K", y = NULL, shape = NULL, linetype = NULL
  ) +
  theme_report()

ggsave(file.path(out_dir, "choosing_k.png"), plot_k, width = 8, height = 6, dpi = 300)


# ================================================================
# 5. FITTING THE LDA MODEL
# ================================================================
# K chosen after reading the criteria (section 4) and the topics.
# The four criteria do not agree (see choosing_k.png). We therefore
# compare the mean topic coherence (textmineR, section 9) from K = 6
# to K = 10: 0.095 (K = 6), 0.090 (7), 0.094 (8), 0.095 (9), 0.099 (10).
# The differences are small; K = 10 has the best coherence and ten
# distinct topics. At K = 6, studies, work and learning are merged.
# (K is set to 10 in section 4.)

lda_model <- LDA(
  dtm,
  k = K,
  method = "Gibbs",
  control = list(seed = 588, burnin = 1000, iter = 3000, thin = 100)
)

print(terms(lda_model, 10))


# ================================================================
# 6. BETA: WORDS THAT CHARACTERISE EACH TOPIC
# ================================================================

beta <- tidy(lda_model, matrix = "beta")

top_words <- beta %>%
  group_by(topic) %>%
  slice_max(beta, n = 10, with_ties = FALSE) %>%
  ungroup() %>%
  arrange(topic, -beta)

# Topic names: chosen after reading the words and the typical posts
# (section 7.2), in results/topics/topic_names.csv (columns topic,
# name). Until that file exists: the first three words.
topic_names <- top_words %>%
  group_by(topic) %>%
  summarise(name = paste(head(term, 3), collapse = ", ")) %>%
  mutate(name = paste0("T", topic, ": ", name)) %>%
  pull(name)

names_file <- file.path(out_dir, "topic_names.csv")
if (file.exists(names_file)) {
  chosen_names <- read_csv(names_file, show_col_types = FALSE)
  if (nrow(chosen_names) == K) topic_names <- paste0("T", chosen_names$topic, ": ", chosen_names$name)
}

plot_beta <- top_words %>%
  mutate(
    topic = factor(topic_names[topic], levels = topic_names),
    term = reorder_within(term, beta, topic)
  ) %>%
  ggplot(aes(beta, term)) +
  geom_col(fill = "#2471A3") +
  scale_y_reordered() +
  facet_wrap(~ topic, ncol = 2, scales = "free_y") +
  labs(title = "The 10 most probable words of each topic (LDA)", x = expression(beta), y = NULL) +
  theme_report(10)

ggsave(file.path(out_dir, "lda_top_words.png"), plot_beta, width = 9, height = 2.3 * ceiling(K / 2), dpi = 300)


# ================================================================
# 7. GAMMA: TOPICS OF EACH POST
# ================================================================

gamma <- tidy(lda_model, matrix = "gamma") %>%
  rename(id = document)

# 7.1 Dominant topic of each post
dominant <- gamma %>%
  group_by(id) %>%
  slice_max(gamma, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  transmute(id, dominant_topic = topic, dominant_gamma = gamma)

topics_by_post <- gamma %>%
  mutate(topic = paste0("gamma_T", topic)) %>%
  pivot_wider(names_from = topic, values_from = gamma) %>%
  left_join(dominant, by = "id") %>%
  mutate(topic_name = topic_names[dominant_topic])

write_csv(topics_by_post, file.path(out_dir, "topics_by_post.csv"))

# 7.2 Typical posts of each topic (to name the topics)
typical_posts <- gamma %>%
  group_by(topic) %>%
  slice_max(gamma, n = 5, with_ties = FALSE) %>%
  ungroup() %>%
  left_join(meta %>% select(id, subreddit, title, text), by = "id") %>%
  mutate(excerpt = str_trunc(text, 400)) %>%
  select(topic, gamma, id, subreddit, title, excerpt)


# ================================================================
# 8. TOPIC PREVALENCE, BY AUDIENCE AND BY YEAR
# ================================================================

gamma_meta <- gamma %>%
  left_join(meta %>% select(id, audience, year), by = "id") %>%
  mutate(name = factor(topic_names[topic], levels = topic_names))

# 8.1 Mean prevalence in the whole corpus
prevalence <- gamma_meta %>%
  group_by(name) %>%
  summarise(mean_gamma = mean(gamma), .groups = "drop")

plot_prevalence <- ggplot(prevalence, aes(x = reorder(name, mean_gamma), y = mean_gamma)) +
  geom_col(fill = "#2471A3") +
  coord_flip() +
  labs(title = "Mean topic prevalence", x = NULL, y = "Mean gamma") +
  theme_report()

ggsave(file.path(out_dir, "lda_prevalence.png"), plot_prevalence, width = 8, height = 4.5, dpi = 300)

# 8.2 By audience (AI or everyday subreddits), topics ordered by the
# gap between the two audiences
by_audience <- gamma_meta %>%
  group_by(audience, name) %>%
  summarise(mean_gamma = mean(gamma), .groups = "drop")

audience_order <- by_audience %>%
  pivot_wider(names_from = audience, values_from = mean_gamma) %>%
  mutate(gap = Everyday - AI) %>%
  arrange(gap) %>%
  pull(name)

plot_audience <- by_audience %>%
  mutate(name = factor(name, levels = audience_order)) %>%
  ggplot(aes(x = name, y = mean_gamma, fill = audience)) +
  geom_col(position = position_dodge(width = 0.75), width = 0.7) +
  coord_flip() +
  scale_fill_manual(values = audience_colours) +
  labs(title = "Topic prevalence by audience", x = NULL, y = "Mean gamma", fill = "Audience") +
  theme_report()

ggsave(file.path(out_dir, "lda_by_audience.png"), plot_audience, width = 8, height = 5, dpi = 300)

# 8.3 By year: one small plot per topic (a single figure with ten
# lines would be unreadable)
by_year <- gamma_meta %>%
  group_by(year, name) %>%
  summarise(mean_gamma = mean(gamma), .groups = "drop")

n_year <- meta %>% count(year)

plot_year <- ggplot(by_year, aes(x = year, y = mean_gamma)) +
  geom_line(colour = "#2471A3", linewidth = 0.9) +
  geom_point(colour = "#2471A3", size = 1.8) +
  facet_wrap(~ name, ncol = 2) +
  scale_x_continuous(breaks = sort(unique(by_year$year))) +
  labs(
    title = "Change in topic prevalence (2023-2026)",
    caption = paste("Posts per year:", paste(n_year$year, n_year$n, sep = ": ", collapse = "; ")),
    x = NULL, y = "Mean gamma"
  ) +
  theme_report(10)

ggsave(file.path(out_dir, "lda_by_year.png"), plot_year, width = 8, height = 2 * ceiling(K / 2) + 1, dpi = 300)


# ================================================================
# 9. EVALUATING THE MODEL
# ================================================================

# Perplexity (lower is better) and log-likelihood, as in the course
perplexity_value <- perplexity(lda_model, newdata = dtm)
log_likelihood <- as.numeric(logLik(lda_model))

# Probabilistic coherence of each topic (textmineR): do the top words
# of a topic appear together in the same posts?
phi <- exp(lda_model@beta)
colnames(phi) <- lda_model@terms
dtm_sparse <- Matrix::sparseMatrix(i = dtm$i, j = dtm$j, x = dtm$v,
                                   dims = dim(dtm), dimnames = dimnames(dtm))
coherence <- CalcProbCoherence(phi = phi, dtm = dtm_sparse, M = 10)

evaluation <- tibble(
  indicator = c("Perplexity", "Log-likelihood", "Mean coherence"),
  value = c(perplexity_value, log_likelihood, mean(coherence))
)
print(evaluation)

# The same indicators from K = 6 to K = 10 (same settings as the chosen
# model), to justify the choice of K (section 5)
models_k <- map(set_names(6:10), function(k) {
  LDA(dtm, k = k, method = "Gibbs",
      control = list(seed = 588, burnin = 1000, iter = 3000, thin = 100))
})

evaluation_k <- map_dfr(6:10, function(k) {
  m <- models_k[[as.character(k)]]
  phi_k <- exp(m@beta)
  colnames(phi_k) <- m@terms
  tibble(
    K = k,
    perplexity = perplexity(m, newdata = dtm),
    log_likelihood = as.numeric(logLik(m)),
    mean_coherence = mean(CalcProbCoherence(phi = phi_k, dtm = dtm_sparse, M = 10))
  )
})
print(evaluation_k)

# Do the conclusions depend on K? For K = 8, 9 and 10, each topic is
# assigned, from its top words (printed below), to personal life, to
# AI and its technical uses, or to neither (studies, money). The two
# audiences and the change over time are then compared.
for (k in c("8", "9", "10")) {
  cat("\nK =", k, "\n")
  print(terms(models_k[[k]], 6))
}

personal_topics <- list("8" = c(1, 6, 7), "9" = c(2, 7, 8), "10" = c(2, 6, 8, 10))
ai_topics <- list("8" = c(2, 4, 5, 8), "9" = c(1, 4, 5, 9), "10" = c(3, 5, 7, 9))

robustness_k <- map_dfr(c("8", "9", "10"), function(k) {
  g <- tidy(models_k[[k]], matrix = "gamma") %>%
    rename(id = document) %>%
    left_join(meta %>% select(id, audience, year), by = "id")
  dom <- g %>% group_by(id) %>% slice_max(gamma, n = 1, with_ties = FALSE) %>% ungroup()
  personal <- g %>%
    filter(topic %in% personal_topics[[k]]) %>%
    group_by(id, audience, year) %>%
    summarise(gamma = sum(gamma), .groups = "drop")
  trend <- summary(lm(gamma ~ year + audience, data = personal))$coefficients["year", ]
  tibble(
    K = as.integer(k),
    everyday_dominant_personal = mean(dom$topic[dom$audience == "Everyday"] %in% personal_topics[[k]]),
    ai_dominant_personal = mean(dom$topic[dom$audience == "AI"] %in% personal_topics[[k]]),
    ai_dominant_ai = mean(dom$topic[dom$audience == "AI"] %in% ai_topics[[k]]),
    everyday_dominant_ai = mean(dom$topic[dom$audience == "Everyday"] %in% ai_topics[[k]]),
    personal_per_year = trend["Estimate"],
    p_personal_per_year = trend["Pr(>|t|)"]
  )
})
print(robustness_k)


# ================================================================
# 10. EXTENSION 1: STM (STRUCTURAL TOPIC MODEL)
# ================================================================
# The STM estimates the topics and, at the same time, the effect of
# document variables on the prevalence of each topic: here the
# audience (AI or everyday subreddits) and the year.

stm_corpus <- readCorpus(dtm, type = "slam")
meta_stm <- meta %>% mutate(audience = factor(audience), year = as.numeric(year))

stm_model <- stm(
  documents = stm_corpus$documents,
  vocab = stm_corpus$vocab,
  K = K,
  prevalence = ~ audience + year,
  data = meta_stm,
  init.type = "Spectral",
  seed = 588,
  verbose = FALSE
)

# Topic words: the most probable and the most exclusive (FREX)
stm_labels <- labelTopics(stm_model, n = 8)
stm_words <- tibble(
  topic = seq_len(K),
  most_probable = apply(stm_labels$prob, 1, paste, collapse = ", "),
  frex = apply(stm_labels$frex, 1, paste, collapse = ", ")
)
print(stm_words)

# Labels: the four FREX words of each STM topic
stm_names <- paste0("S", seq_len(K), ": ", apply(stm_labels$frex[, 1:4], 1, paste, collapse = ", "))

# Effect of audience and year
effects <- estimateEffect(1:K ~ audience + year, stm_model, metadata = meta_stm)
print(summary(effects))

# Audience and year coefficients for each topic (with p-values)
stm_coefs <- map_dfr(seq_len(K), function(k) {
  tab <- summary(effects)$tables[[k]]
  tibble(
    topic = k, name = stm_names[k],
    effect_everyday = tab["audienceEveryday", "Estimate"],
    p_everyday = tab["audienceEveryday", "Pr(>|t|)"],
    effect_year = tab["year", "Estimate"],
    p_year = tab["year", "Pr(>|t|)"]
  )
})
print(stm_coefs)

# Difference in each topic's prevalence between the two audiences,
# with its 95% confidence interval (the stm plot, redrawn in ggplot to
# be readable)
audience_diff <- plot(
  effects, covariate = "audience", method = "difference",
  cov.value1 = "Everyday", cov.value2 = "AI", omit.plot = TRUE
)

plot_stm_audience <- tibble(
  name = stm_names,
  difference = unlist(audience_diff$means),
  low = map_dbl(audience_diff$cis, 1),
  high = map_dbl(audience_diff$cis, 2)
) %>%
  mutate(
    name = reorder(name, difference),
    side = ifelse(difference > 0, "Everyday", "AI")
  ) %>%
  ggplot(aes(x = difference, y = name, colour = side)) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey50") +
  geom_errorbarh(aes(xmin = low, xmax = high), height = 0.25) +
  geom_point(size = 2.5) +
  scale_colour_manual(values = audience_colours, guide = "none") +
  labs(
    title = "STM: effect of the audience on each topic",
    subtitle = "Difference in proportion (everyday minus AI), 95% confidence interval, holding year constant",
    x = "<- more present in AI subreddits        more present in everyday subreddits ->",
    y = NULL
  ) +
  theme_report()

ggsave(file.path(out_dir, "stm_audience_effect.png"), plot_stm_audience, width = 8.5, height = 4.8, dpi = 300)

# Mean prevalence of each STM topic in the corpus
plot_stm_prop <- tibble(name = stm_names, proportion = colMeans(stm_model$theta)) %>%
  ggplot(aes(x = proportion, y = reorder(name, proportion))) +
  geom_col(fill = "#2471A3") +
  scale_x_continuous(labels = scales::percent) +
  labs(title = "STM: topic prevalence in the corpus", x = "Mean proportion", y = NULL) +
  theme_report()

ggsave(file.path(out_dir, "stm_prevalence.png"), plot_stm_prop, width = 8, height = 4.5, dpi = 300)


# ================================================================
# 11. EXTENSION 2: BTM ON THE TITLES
# ================================================================
# The BTM (biterm topic model) is designed for short texts: it models
# word pairs rather than documents. Titles sum up each post in one
# sentence: we check whether the topics of the titles match those of
# the full posts.

# Lemmatised titles (02c_lemmatise.R), same filters as in section 2
titles_tidy <- lemmas %>%
  transmute(id, title_clean = str_to_lower(title_lemmas) %>% str_replace_all("[\u2018\u2019]", "'")) %>%
  unnest_tokens(word, title_clean) %>%
  anti_join(stop_words, by = "word") %>%
  filter(!word %in% c(extra_stopwords, query_words)) %>%
  mutate(word = str_remove_all(word, "'")) %>%
  filter(str_detect(word, "^[a-z]+$"), nchar(word) >= 3) %>%
  select(doc_id = id, token = word)

set.seed(588)
btm_model <- BTM(titles_tidy, k = K, iter = 1000, window = 15, trace = FALSE, detailed = TRUE)

btm_words <- terms(btm_model, top_n = 10)
btm_words <- tibble(
  topic = seq_len(K),
  share = round(btm_model$theta, 3),
  words = map_chr(btm_words, ~ paste(.x$token, collapse = ", "))
)
print(btm_words)

# Plot from the course script (textplot): the most probable words of
# each BTM topic, linked by the biterms that connect them.
# The placement of the groups is random: the seed only changes where
# the groups sit on the figure, not the model.
set.seed(5)
plot_btm <- plot(
  btm_model, top_n = 7,
  title = "BTM on post titles",
  subtitle = "Most probable words of each topic, linked by their biterms"
) +
  ggplot2::theme(plot.margin = ggplot2::margin(15, 25, 15, 25))

ggsave(file.path(out_dir, "btm_titles.png"), plot_btm, width = 11, height = 8.5, dpi = 250, bg = "white")


# ================================================================
# 12. EXPORTING THE TABLES
# ================================================================

write_xlsx(
  list(
    choosing_k = k_metrics,
    lda_top_words = top_words,
    lda_names = tibble(topic = seq_len(K), name = topic_names),
    lda_prevalence = prevalence,
    lda_by_audience = by_audience,
    lda_by_year = by_year,
    lda_typical_posts = typical_posts,
    lda_evaluation = evaluation,
    lda_evaluation_k = evaluation_k,
    lda_robustness_k = robustness_k,
    lda_coherence = tibble(topic = seq_len(K), coherence = coherence),
    stm_words = stm_words,
    stm_effects = stm_coefs,
    btm_titles = btm_words
  ),
  file.path(out_dir, "topics_axis2.xlsx")
)

save(lda_model, stm_model, btm_model, effects, file = file.path(out_dir, "topic_models.RData"))

cat("\nResults in", out_dir, "\n")
