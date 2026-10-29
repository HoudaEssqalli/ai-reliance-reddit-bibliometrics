# =========================================================
#  AXIS 2 - REDDIT
#  Checking the precision of the relevance filter
# =========================================================
# As for the axis 1 search equation: 60 random posts of the final
# corpus, read one by one and sorted into three categories:
#  - "core": a person relies on AI for a decision, a judgment or a
#    task they used to do themselves, or questions this reliance;
#  - "related": reliance on AI is mentioned, but as a general topic
#    (society, companies) or in passing;
#  - "off-topic": the post is not about reliance on AI.
#
# Run from the project folder, after the cleaning:
#   Rscript scripts/02b_precision_check.R
# The file corpus/precision_sample.xlsx is created once, then filled in
# by hand (column judgment); running the script again gives the summary.

library(dplyr)
library(readr)
library(readxl)
library(writexl)

sample_file <- "corpus/precision_sample.xlsx"

if (!file.exists(sample_file)) {
  corpus <- read_csv("corpus/reddit_corpus.csv", show_col_types = FALSE)
  set.seed(2026)
  corpus %>%
    slice_sample(n = 60) %>%
    transmute(reddit_id, subreddit, audience, selection_rule, title,
              excerpt = substr(text, 1, 1500), judgment = NA_character_) %>%
    write_xlsx(sample_file)
  cat("Sample created:", sample_file, "- fill in the judgment column (core / related / off-topic)\n")
}

precision_sample <- read_excel(sample_file)

# The sample was drawn from the 651-post corpus, just before removing
# the near-duplicates and the automated paper summaries: only posts
# still in the final corpus are counted.
if ("in_final_corpus" %in% names(precision_sample)) {
  cat("Sample posts removed since:", sum(!precision_sample$in_final_corpus), "\n")
  precision_sample <- precision_sample %>% filter(in_final_corpus)
}

if (all(!is.na(precision_sample$judgment))) {
  print(precision_sample %>% count(selection_rule, judgment) %>%
          tidyr::pivot_wider(names_from = judgment, values_from = n, values_fill = 0))

  by_audience <- precision_sample %>%
    count(audience, judgment) %>%
    group_by(audience) %>%
    mutate(share = round(100 * n / sum(n))) %>%
    ungroup()

  overall <- precision_sample %>%
    count(judgment) %>%
    mutate(share = round(100 * n / sum(n)))

  print(by_audience)
  print(overall)
  write_csv(bind_rows(by_audience, overall %>% mutate(audience = "All")), "corpus/precision_summary.csv")
} else {
  cat("Judgments incomplete:", sum(is.na(precision_sample$judgment)), "posts still to judge\n")
}
