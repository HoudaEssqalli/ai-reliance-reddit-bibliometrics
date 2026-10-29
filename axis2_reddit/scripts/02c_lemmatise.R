# =========================================================
#  AXIS 2 - REDDIT
#  Lemmatising the corpus (udpipe, English EWT model)
# =========================================================
# Each word is reduced to its lemma ("felt" -> "feel", "studies" ->
# "study", "relying" -> "rely"), so that word frequencies, clustering
# and topic models count the forms of a word together.
#
# Run from the project folder, after the cleaning:
#   Rscript scripts/02c_lemmatise.R
# The model is in models/ (udpipe_download_model("english-ewt")).
# Output: corpus/corpus_lemmas.csv (id, text_lemmas, title_lemmas)

library(udpipe)
library(dplyr)
library(readr)

posts <- read_csv("corpus/reddit_corpus.csv", show_col_types = FALSE)

model <- udpipe_load_model("models/english-ewt-ud-2.5-191206.udpipe")

# Lemmatise a vector of texts: keep the lemmas of words
# (no punctuation, numbers or symbols)
lemmatise <- function(texts, ids) {
  udpipe_annotate(model, x = texts, doc_id = ids, parser = "none") %>%
    as.data.frame() %>%
    filter(!upos %in% c("PUNCT", "NUM", "SYM", "X")) %>%
    # Proper nouns keep their form: the model sometimes lemmatises
    # "AI" as "be" in a question ("Is AI really a threat?")
    mutate(lemma_out = tolower(ifelse(upos == "PROPN", token, lemma))) %>%
    # Two wrong lemmas from the model, fixed by hand
    mutate(lemma_out = recode(lemma_out, addicte = "addict", delegat = "delegate")) %>%
    group_by(doc_id) %>%
    summarise(lemmas = paste(lemma_out, collapse = " "), .groups = "drop") %>%
    rename(id = doc_id)
}

# Full texts (frequencies, clustering, LDA, STM)
text_lemmas <- lemmatise(posts$text, posts$id) %>% rename(text_lemmas = lemmas)

# Titles only (BTM, a model for short texts)
title_lemmas <- lemmatise(posts$title, posts$id) %>% rename(title_lemmas = lemmas)

corpus_lemmas <- posts %>%
  select(id, audience, year) %>%
  left_join(text_lemmas, by = "id") %>%
  left_join(title_lemmas, by = "id")

write_csv(corpus_lemmas, "corpus/corpus_lemmas.csv")

cat("Posts lemmatised:", sum(!is.na(corpus_lemmas$text_lemmas)), "\n")
cat("Lemmatised words:", sum(lengths(strsplit(corpus_lemmas$text_lemmas, " "))), "\n")
