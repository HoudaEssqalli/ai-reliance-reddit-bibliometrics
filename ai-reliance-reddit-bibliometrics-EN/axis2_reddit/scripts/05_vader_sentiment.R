# ======================================================================
#  AXIS 2 - REDDIT
#  Sentiment analysis with VADER
#  (adapted from the course scripts vader_01_28.R and vader_28_TP.R)
# ======================================================================
# PRINCIPLE (as in the course practical):
# Corpus preparation
#       |
# Sentiment scores sentence by sentence (NEG - NEU - POS - COMPOUND)
#       |
# Mean score of each post, then class: Negative / Neutral / Positive
#       |
# Answers to questions Q1 to Q14
#
# VADER is designed for social-media English and for short sentences:
# as in vader_01_28.R (get_sentences), each sentence is scored, then
# the scores are averaged by post.
#
# Run from the project folder, after 04_topic_models.R:
#   Rscript scripts/05_vader_sentiment.R
# Outputs: results/vader/


# ======================================================================
# 1. PACKAGES
# ======================================================================

library(vader)
library(syuzhet)
library(tidyverse)
library(readxl)
library(writexl)
library(caret)

source("scripts/plot_style.R")

out_dir <- "results/vader"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)


# ======================================================================
# 2. LOADING THE CORPUS
# ======================================================================

posts <- read_csv("corpus/reddit_corpus.csv", show_col_types = FALSE)

# Dominant topic of each post (LDA, 04_topic_models.R)
topics <- read_csv("results/topics/topics_by_post.csv", show_col_types = FALSE) %>%
  select(id, dominant_topic, topic_name)

posts <- posts %>% left_join(topics, by = "id")

cat("Posts:", nrow(posts), "\n")


# ======================================================================
# 3. SPLITTING INTO SENTENCES
# ======================================================================

sentences <- posts %>%
  select(id, text) %>%
  mutate(sentence = map(text, get_sentences)) %>%
  select(-text) %>%
  unnest(sentence) %>%
  filter(str_count(sentence, "\\S+") >= 3)

cat("Sentences:", nrow(sentences), "\n")


# ======================================================================
# 4. VADER SCORES OF EACH SENTENCE
# ======================================================================

sentence_scores <- vader_df(sentences$sentence)

sentences <- sentences %>%
  mutate(
    compound = sentence_scores$compound,
    pos = sentence_scores$pos,
    neu = sentence_scores$neu,
    neg = sentence_scores$neg
  )


# ======================================================================
# 5. SCORE OF EACH POST AND CLASSIFICATION
# ======================================================================
# Compound >= +0.05: Positive
# Compound <= -0.05: Negative
# In between:        Neutral

post_scores <- sentences %>%
  group_by(id) %>%
  summarise(
    VADER_COMPOUND = mean(compound, na.rm = TRUE),
    VADER_POS = mean(pos, na.rm = TRUE),
    VADER_NEU = mean(neu, na.rm = TRUE),
    VADER_NEG = mean(neg, na.rm = TRUE),
    share_negative_sentences = mean(compound <= -0.05, na.rm = TRUE),
    # strongly charged sentences (|compound| >= 0.5)
    n_very_positive = sum(compound >= 0.5, na.rm = TRUE),
    n_very_negative = sum(compound <= -0.5, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(ambivalent = n_very_positive > 0 & n_very_negative > 0)

data <- posts %>%
  left_join(post_scores, by = "id") %>%
  mutate(
    VADER_sentiment = case_when(
      VADER_COMPOUND >= 0.05 ~ "Positive",
      VADER_COMPOUND <= -0.05 ~ "Negative",
      TRUE ~ "Neutral"
    ),
    VADER_sentiment = factor(VADER_sentiment, levels = c("Negative", "Neutral", "Positive"))
  )

colours <- sentiment_colours


# ######################################################################
# Q1. WHAT IS THE OVERALL TONE OF THE POSTS?
# ######################################################################

Q1 <- data %>%
  count(VADER_sentiment) %>%
  mutate(percent = round(100 * n / sum(n), 1))

print(Q1)

# As in vader_01_28.R, the same breakdown at the sentence level
Q1_sentences <- sentences %>%
  mutate(class = case_when(
    compound >= 0.05 ~ "Positive",
    compound <= -0.05 ~ "Negative",
    TRUE ~ "Neutral"
  )) %>%
  count(class) %>%
  mutate(percent = round(100 * n / sum(n), 1))

print(Q1_sentences)

plot_Q1 <- ggplot(Q1, aes(x = VADER_sentiment, y = n, fill = VADER_sentiment)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = paste0(n, " (", percent, "%)")), vjust = -0.5) +
  scale_fill_manual(values = colours) +
  labs(title = "Sentiment of the posts (VADER)", x = NULL, y = "Number of posts") +
  theme_report()

ggsave(file.path(out_dir, "Q1_tone.png"), plot_Q1, width = 6, height = 4.5, dpi = 300)


# ######################################################################
# Q2. HOW STRONG ARE THE SENTIMENTS?
# ######################################################################

Q2 <- data %>%
  summarise(
    mean = mean(VADER_COMPOUND),
    median = median(VADER_COMPOUND),
    sd = sd(VADER_COMPOUND),
    min = min(VADER_COMPOUND),
    max = max(VADER_COMPOUND)
  )

print(Q2)

plot_Q2 <- ggplot(data, aes(x = VADER_COMPOUND)) +
  geom_histogram(binwidth = 0.05, fill = "steelblue", colour = "white") +
  geom_vline(xintercept = c(-0.05, 0.05), linetype = "dashed") +
  labs(
    title = "Distribution of the mean compound score of the posts",
    x = "Mean compound (from -1 to +1)", y = "Number of posts"
  ) +
  theme_report()

ggsave(file.path(out_dir, "Q2_intensity.png"), plot_Q2, width = 7, height = 4.5, dpi = 300)


# ######################################################################
# Q3. ARE THE POSTS STRONGLY POLARISED?
# ######################################################################
# The practical calls a text "polarised" when |compound| >= 0.5. Here a
# post's score is the mean of its sentences, so it is pulled towards 0:
# the threshold is applied to sentences, and we flag ambivalent posts,
# which contain both very positive and very negative sentences.

# Indicator from the practical: share of posts with |compound| >= 0.5 ("Strong")
Q3_course <- data %>%
  mutate(intensity = if_else(abs(VADER_COMPOUND) >= 0.5, "Strong", "Moderate")) %>%
  count(intensity) %>%
  mutate(percent = round(100 * n / sum(n), 1))

print(Q3_course)

Q3 <- tibble(
  share_polarised_sentences = mean(abs(sentences$compound) >= 0.5, na.rm = TRUE),
  share_posts_with_very_negative_sentence = mean(data$n_very_negative > 0),
  share_posts_with_very_positive_sentence = mean(data$n_very_positive > 0),
  share_ambivalent_posts = mean(data$ambivalent)
)

print(Q3)


# ######################################################################
# Q4. WHAT ARE THE MAIN THEMES?
# ######################################################################
# Themes = dominant topic of the LDA model (04_topic_models.R)

Q4 <- data %>%
  count(topic_name) %>%
  mutate(percent = round(100 * n / sum(n), 1)) %>%
  arrange(desc(n))

print(Q4)


# ######################################################################
# Q5. DOES SENTIMENT VARY BY THEME?
# ######################################################################

Q5 <- data %>%
  group_by(topic_name) %>%
  summarise(
    n = n(),
    mean_compound = mean(VADER_COMPOUND),
    share_negative = mean(VADER_sentiment == "Negative"),
    share_positive = mean(VADER_sentiment == "Positive"),
    .groups = "drop"
  ) %>%
  arrange(mean_compound)

print(Q5)

# As in the practical: Kruskal-Wallis test
Q5_kruskal <- kruskal.test(VADER_COMPOUND ~ factor(topic_name), data = data)
print(Q5_kruskal)

plot_Q5 <- data %>%
  count(topic_name, VADER_sentiment) %>%
  group_by(topic_name) %>%
  mutate(share = n / sum(n)) %>%
  ungroup() %>%
  # themes ordered from most negative (top) to least negative
  mutate(topic_name = factor(topic_name, levels = rev(Q5$topic_name[order(-Q5$share_negative)]))) %>%
  ggplot(aes(x = topic_name, y = share, fill = VADER_sentiment)) +
  geom_col() +
  coord_flip() +
  scale_fill_manual(values = colours) +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Sentiment by theme (dominant LDA topic)", x = NULL, y = NULL, fill = NULL) +
  theme_report()

ggsave(file.path(out_dir, "Q5_sentiment_by_theme.png"), plot_Q5, width = 8, height = 4.5, dpi = 300)


# ######################################################################
# Q6. WHICH THEMES ARE THE MOST POLARISED?
# ######################################################################

Q6 <- data %>%
  group_by(topic_name) %>%
  summarise(
    n = n(),
    # indicator from the practical: mean |compound|
    mean_polarisation = mean(abs(VADER_COMPOUND)),
    share_with_very_negative_sentence = mean(n_very_negative > 0),
    share_ambivalent = mean(ambivalent),
    .groups = "drop"
  ) %>%
  arrange(desc(share_ambivalent))

print(Q6)


# ######################################################################
# Q7. DOES SENTIMENT DIFFER BY AUDIENCE?
# ######################################################################
# "AI" audience (AI subreddits) or "Everyday" (everyday-life
# subreddits). Wilcoxon test on the mean compound and chi-squared test
# on the classes.

Q7 <- data %>%
  group_by(audience) %>%
  summarise(
    n = n(),
    mean_compound = mean(VADER_COMPOUND),
    share_negative = mean(VADER_sentiment == "Negative"),
    share_positive = mean(VADER_sentiment == "Positive"),
    .groups = "drop"
  )

print(Q7)
Q7_wilcoxon <- wilcox.test(VADER_COMPOUND ~ audience, data = data)
Q7_chi2 <- chisq.test(table(data$audience, data$VADER_sentiment))
print(Q7_wilcoxon)
print(Q7_chi2)

plot_Q7 <- ggplot(data, aes(x = audience, y = VADER_COMPOUND, fill = audience)) +
  geom_boxplot(show.legend = FALSE, alpha = 0.8) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_fill_manual(values = audience_colours) +
  labs(title = "Mean compound score by audience", x = NULL, y = "Mean compound") +
  theme_report()

ggsave(file.path(out_dir, "Q7_sentiment_by_audience.png"), plot_Q7, width = 6, height = 4.5, dpi = 300)


# ######################################################################
# Q8. DOES SENTIMENT CHANGE OVER TIME?
# ######################################################################

Q8 <- data %>%
  group_by(year) %>%
  summarise(
    n = n(),
    mean_compound = mean(VADER_COMPOUND),
    share_negative = mean(VADER_sentiment == "Negative"),
    .groups = "drop"
  )

print(Q8)

# As in the practical for age: Kruskal-Wallis test
Q8_kruskal <- kruskal.test(VADER_COMPOUND ~ factor(year), data = data)
print(Q8_kruskal)


# ######################################################################
# Q9. DOES SENTIMENT DIFFER BY TYPE OF POST?
# ######################################################################
# The practical compares two settings (Mann-Whitney). Here, within the
# everyday subreddits, we compare the two rules that brought a post
# into the corpus: the person says they rely on AI ("reliance") or
# says they asked it for advice ("advice"). The AI subreddits only
# have the first rule: they are left aside, so as not to mix the
# effect of the rule with that of the audience. The 12 posts selected
# by both rules are also left aside.

data_Q9 <- data %>% filter(audience == "Everyday", selection_rule != "both")

Q9 <- data_Q9 %>%
  group_by(selection_rule) %>%
  summarise(
    n = n(),
    mean_compound = mean(VADER_COMPOUND),
    share_negative = mean(VADER_sentiment == "Negative"),
    share_positive = mean(VADER_sentiment == "Positive"),
    .groups = "drop"
  )

print(Q9)

Q9_mann_whitney <- wilcox.test(VADER_COMPOUND ~ selection_rule, data = data_Q9, exact = FALSE)
print(Q9_mann_whitney)


# ######################################################################
# Q10 TO Q13. DOES VADER AGREE WITH HUMAN CODING?
# ######################################################################
# As in the practical: VADER is compared with human coding on a sample
# of 60 random posts. The file corpus/hand_coding.xlsx has two columns
# filled in by hand: human_sentiment (Negative / Neutral / Positive)
# and complexity (Q12). If it does not exist yet, an empty one is
# created. Posts are identified by their Reddit ID, which does not
# change if the corpus is rebuilt.

coding_file <- "corpus/hand_coding.xlsx"

if (!file.exists(coding_file)) {
  set.seed(2026)
  data %>%
    slice_sample(n = 60) %>%
    transmute(reddit_id, title, text, human_sentiment = NA_character_, complexity = NA_character_) %>%
    write_xlsx(coding_file)
  cat("\nFile", coding_file, "created: fill in the human_sentiment and complexity columns, then run again.\n")
}

coding <- read_excel(coding_file) %>% select(reddit_id, human_sentiment, complexity)

if (all(!is.na(coding$human_sentiment))) {

  validation <- data %>%
    inner_join(coding, by = "reddit_id") %>%
    mutate(human_sentiment = factor(human_sentiment, levels = levels(VADER_sentiment)))

  # Q10. Cross-tabulation VADER / human coding
  Q10 <- table(VADER = validation$VADER_sentiment, Human = validation$human_sentiment)
  print(Q10)

  # Q11. Confusion matrix
  Q11 <- confusionMatrix(validation$VADER_sentiment, validation$human_sentiment)
  print(Q11)

  # Q12. Which types of posts cause the most errors?
  # As in the practical, each coded post has a complexity type (column
  # complexity): Simple, Contrast (positive and negative passages
  # mixed), Politeness (a request for help worded with positive words),
  # Narrative (a situation told through facts, with few sentiment
  # words), Negation, Intensification, Irony, Ambiguity (unclear tone).
  validation <- validation %>%
    mutate(correct = if_else(VADER_sentiment == human_sentiment, 1, 0))

  Q12 <- validation %>%
    group_by(complexity) %>%
    summarise(
      N = n(),
      correct_rate = round(100 * mean(correct), 1),
      error_rate = round(100 - 100 * mean(correct), 1),
      .groups = "drop"
    ) %>%
    arrange(desc(error_rate))

  print(Q12)

  plot_Q12 <- ggplot(Q12, aes(x = reorder(paste0(complexity, " (", N, ")"), error_rate), y = error_rate)) +
    geom_col(fill = "#2471A3") +
    coord_flip() +
    labs(
      title = "VADER error rate by type of post",
      subtitle = "60 hand-coded posts; number of posts in brackets",
      x = NULL, y = "Error rate (%)"
    ) +
    theme_report()

  ggsave(file.path(out_dir, "Q12_errors_by_post_type.png"), plot_Q12, width = 7, height = 4.5, dpi = 300)

  # Q13. Overall performance: accuracy, kappa, then precision, recall
  # and F1 (mean of the three classes, as in the practical, and by class)
  Q13 <- tibble(
    indicator = c("Accuracy", "Kappa", "Precision (macro)", "Recall (macro)", "F1 (macro)"),
    value = round(c(
      Q11$overall["Accuracy"],
      Q11$overall["Kappa"],
      mean(Q11$byClass[, "Pos Pred Value"], na.rm = TRUE),
      mean(Q11$byClass[, "Sensitivity"], na.rm = TRUE),
      mean(Q11$byClass[, "F1"], na.rm = TRUE)
    ), 3)
  )
  print(Q13)

  Q13_by_class <- as_tibble(Q11$byClass[, c("Pos Pred Value", "Sensitivity", "F1")], rownames = "class") %>%
    transmute(
      class = str_remove(class, "Class: "),
      precision = round(`Pos Pred Value`, 2),
      recall = round(Sensitivity, 2),
      F1 = round(F1, 2)
    )
  print(Q13_by_class)

} else {
  Q10 <- Q11 <- NULL
  Q12 <- Q13 <- Q13_by_class <- tibble()
  cat("Human coding incomplete: Q10 to Q13 not computed.\n")
}


# ######################################################################
# Q14. WHAT ARE THE LIMITS OF THE METHOD?
# ######################################################################
# As in the practical: the misclassified posts, to be read.

Q14 <- if (nrow(Q12) > 0) {
  validation %>%
    filter(correct == 0) %>%
    transmute(id, title, complexity, human_sentiment, VADER_sentiment, VADER_COMPOUND,
              excerpt = str_trunc(text, 300))
} else tibble()

print(head(Q14, 15))

# Is the aggregation rule to blame? We compare the mean of the
# sentences (the rule used) with two other rules: VADER on the whole
# text (as in the practical, which scores short comments), and the
# balance of positive and negative sentences.
if (nrow(Q12) > 0) {
  whole_text <- vader_df(validation$text)$compound

  balance <- sentences %>%
    filter(id %in% validation$id) %>%
    group_by(id) %>%
    summarise(balance = (sum(compound >= 0.05) - sum(compound <= -0.05)) / n(), .groups = "drop")

  classify <- function(x) {
    factor(case_when(x >= 0.05 ~ "Positive", x <= -0.05 ~ "Negative", TRUE ~ "Neutral"),
           levels = c("Negative", "Neutral", "Positive"))
  }

  rules <- validation %>%
    mutate(whole_text = whole_text) %>%
    left_join(balance, by = "id")

  Q14_rules <- map_dfr(c("VADER_COMPOUND", "whole_text", "balance"), function(r) {
    cm <- confusionMatrix(classify(rules[[r]]), rules$human_sentiment)
    tibble(rule = r, accuracy = round(cm$overall["Accuracy"], 2), kappa = round(cm$overall["Kappa"], 2))
  }) %>%
    mutate(rule = recode(rule, VADER_COMPOUND = "mean of sentences (used)",
                         whole_text = "whole text", balance = "balance of sentences"))
  print(Q14_rules)
} else Q14_rules <- tibble()

# Do the conclusions depend on the lexicon? The analysis is redone with
# two other lexicons from the syuzhet package (bing and AFINN), sentence
# by sentence and then averaged by post, as for VADER. For these two
# lexicons, a post is negative if its mean is below 0.
sentences <- sentences %>%
  mutate(
    bing = get_sentiment(sentence, method = "bing"),
    afinn = get_sentiment(sentence, method = "afinn")
  )

data <- data %>%
  left_join(
    sentences %>% group_by(id) %>% summarise(bing = mean(bing), afinn = mean(afinn), .groups = "drop"),
    by = "id"
  )

lexicon_class <- function(m) {
  if (m == "VADER_COMPOUND") return(data$VADER_sentiment)
  factor(case_when(data[[m]] > 0 ~ "Positive", data[[m]] < 0 ~ "Negative", TRUE ~ "Neutral"),
         levels = c("Negative", "Neutral", "Positive"))
}

vader_means <- data %>% group_by(topic_name) %>% summarise(v = mean(VADER_COMPOUND), .groups = "drop")

Q14_lexicons <- map_dfr(c("VADER_COMPOUND", "bing", "afinn"), function(m) {
  cl <- lexicon_class(m)
  theme_means <- data %>% group_by(topic_name) %>% summarise(x = mean(.data[[m]]), .groups = "drop") %>%
    left_join(vader_means, by = "topic_name")
  row <- tibble(
    lexicon = recode(m, VADER_COMPOUND = "VADER", bing = "bing", afinn = "AFINN"),
    share_negative_AI = mean(cl[data$audience == "AI"] == "Negative"),
    share_negative_everyday = mean(cl[data$audience == "Everyday"] == "Negative"),
    p_audience = wilcox.test(data[[m]] ~ data$audience, exact = FALSE)$p.value,
    p_theme = kruskal.test(data[[m]] ~ factor(data$topic_name))$p.value,
    theme_ranks_vs_vader = cor(theme_means$x, theme_means$v, method = "spearman")
  )
  # Agreement with the human coding, on the 60 coded posts
  if (nrow(Q12) > 0) {
    v <- data %>% mutate(class = cl) %>% inner_join(coding, by = "reddit_id") %>%
      mutate(h = factor(human_sentiment, levels = c("Negative", "Neutral", "Positive")))
    cm <- confusionMatrix(v$class, v$h)
    row <- row %>% mutate(
      accuracy = cm$overall["Accuracy"],
      kappa = cm$overall["Kappa"],
      spearman_human = cor(v[[m]], as.numeric(v$h), method = "spearman")
    )
  }
  row
})

print(Q14_lexicons, width = Inf)


# ######################################################################
# ADDITION. EMOTIONS (NRC LEXICON, CLASS OF 20 OCTOBER)
# ######################################################################
# Beyond the practical's questions. VADER gives a polarity; the NRC
# lexicon distinguishes eight emotions. Emotion words per 100 words,
# by audience.

emotions <- get_nrc_sentiment(data$text) %>%
  select(anger:trust) %>%
  mutate(across(everything(), ~ 100 * .x / data$n_words)) %>%
  bind_cols(data %>% select(id, audience))

emotions_audience <- emotions %>%
  pivot_longer(anger:trust, names_to = "emotion", values_to = "score") %>%
  group_by(audience, emotion) %>%
  summarise(mean_score = mean(score), .groups = "drop")

print(emotions_audience %>% pivot_wider(names_from = audience, values_from = mean_score))


# ======================================================================
# 10. EXPORTING THE RESULTS
# ======================================================================

write_xlsx(
  list(
    post_scores = data %>% select(id, subreddit, audience, year, topic_name, VADER_COMPOUND,
                                  VADER_POS, VADER_NEU, VADER_NEG, share_negative_sentences,
                                  n_very_positive, n_very_negative, ambivalent, VADER_sentiment),
    Q1_tone = Q1,
    Q1_sentences = Q1_sentences,
    Q2_intensity = Q2,
    Q3_polarisation = Q3,
    Q3_course_indicator = Q3_course,
    Q4_themes = Q4,
    Q5_sentiment_by_theme = Q5,
    Q6_polarisation_by_theme = Q6,
    Q7_audience = Q7,
    Q8_year = Q8,
    Q9_post_type = Q9,
    Q12_errors_by_type = Q12,
    Q13_performance = Q13,
    Q13_by_class = Q13_by_class,
    Q14_errors = Q14,
    Q14_rules = Q14_rules,
    Q14_lexicons = Q14_lexicons,
    addition_nrc_emotions = emotions_audience
  ),
  file.path(out_dir, "vader_axis2.xlsx")
)

cat("\nResults in", out_dir, "\n")
