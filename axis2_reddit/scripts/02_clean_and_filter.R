# =========================================================
#  AXIS 2 - REDDIT
#  Cleaning the Reddit posts and selecting the corpus
# =========================================================
# Run from the project folder, after 01_collect_reddit.R:
#   Rscript scripts/02_clean_and_filter.R


# =========================================================
# 1. Packages
# =========================================================

library(dplyr)
library(stringr)
library(tidytext)
library(readr)
library(writexl)


# =========================================================
# 2. Loading the raw posts
# =========================================================

raw <- bind_rows(readRDS("corpus/reddit_raw.rds"))

steps <- tibble(step = "Posts collected (with duplicates)", n = nrow(raw))
# Counts the posts left after each filter. The number is computed
# before updating steps, otherwise the (lazily evaluated) pipe
# overwrites the intermediate steps.
keep_step <- function(data, label) {
  n <- nrow(data)
  steps <<- bind_rows(steps, tibble(step = label, n = n))
  data
}

posts <- raw %>%
  distinct(id, .keep_all = TRUE) %>%
  keep_step("Unique posts")


# =========================================================
# 3. Quality filters
# =========================================================

clean_text <- function(x) {
  x %>%
    str_replace_all("\\[([^\\]]*)\\]\\([^)]*\\)", "\\1") %>%  # markdown links
    str_replace_all("https?://\\S+", " ") %>%                 # URLs
    str_replace_all("&amp;", "&") %>%
    str_replace_all("&gt;|&lt;|&nbsp;|#x200B;", " ") %>%
    str_replace_all("[*_~>#`]+", " ") %>%                     # formatting
    str_squish()
}

posts <- posts %>%
  filter(!selftext %in% c("[removed]", "[deleted]", ""), !is.na(selftext)) %>%
  keep_step("Post body available (not removed or empty)") %>%
  filter(!coalesce(over_18, FALSE), coalesce(author, "") != "AutoModerator") %>%
  keep_step("No adult content or automated messages") %>%
  mutate(
    text = clean_text(paste0(title, ". ", selftext)),
    n_words = str_count(text, "\\S+")
  ) %>%
  filter(n_words >= 30, n_words <= 1500) %>%
  keep_step("Between 30 and 1,500 words") %>%
  distinct(text, .keep_all = TRUE) %>%
  keep_step("Identical texts removed (cross-posts)")


# English: share of English function words in the text
function_words <- unique(stop_words$word)

english_share <- function(x) {
  w <- str_split(str_to_lower(x), "[^a-z']+")[[1]]
  w <- w[w != ""]
  if (length(w) == 0) return(0)
  mean(w %in% function_words)
}

posts <- posts %>%
  mutate(english = vapply(text, english_share, numeric(1))) %>%
  filter(english >= 0.35) %>%
  keep_step("Text in English")


# =========================================================
# 4. Relevance filter
# =========================================================
# Version 1 (AI + reliance + decision anywhere in the text): of 10
# random posts, only 3 were on topic; the others (product announcements,
# complaints about usage limits) contained the words by chance.
# Version 2: the reliance must be on AI itself, in the same phrase, and
# the post must contain a decision or advice word. On the extended
# collection, version 2 keeps only 123 posts out of 3,565: requiring
# the word "decision" drops posts at the core of the topic ("Over
# reliance on AI is making me slow", "people rely on AI more and more"
# about a wrong answer).
#
# Version 3: a post is kept when a person says they rely on AI, or when
# the post is about reliance on AI:
#  - human subject + reliance verb + AI: "I rely too much on ChatGPT",
#    "students are dependent on AI", "I let ChatGPT decide"
#  - reliance noun + AI: "over-reliance on AI", "my dependence on
#    ChatGPT", "AI dependence", "ChatGPT addiction", "AI as a crutch"
# The human subject rules out sentences where a program depends on a
# model ("dataset quality is dependent on the LLM").
# The decision word is no longer required, but it is recorded
# (column mentions_decision).

ai <- "(chat ?gpt|gpt-?[345o]?|claude|gemini|copilot|llms?|chatbots?|a\\.?i\\.?|artificial intelligence)"
subject <- "(i|i'm|im|i've|ive|we|we're|you|they|people|users?|students?|kids|humans?|everyone|myself|me|many|most|folks|he|she|someone|nobody)"
reliance_verb <- "(rel(y|ies|ied|ying)|depend(s|ed|ing|ent)?|delegat\\w*|outsourc\\w*|addicted|hooked)"
reliance_noun <- "(over-?relian\\w*|relian(ce|t)|dependen(ce|cy)|addiction)"

re_ai_reliance <- regex(paste0(
  "\\b", subject, "\\W+(\\w+\\W+){0,3}", reliance_verb, "\\W+(\\w+\\W+){0,3}", ai, "\\b",
  "|\\b", reliance_noun, "\\W+(\\w+\\W+){0,3}", ai, "\\b",
  "|\\b", ai, "\\W+(dependen\\w*|relian\\w*|addiction|over-?relian\\w*)\\b",
  "|\\b", ai, "\\W+(is|as)\\W+(my|a)\\W+crutch\\b",
  "|\\b(let|letting|have|having|ask|asking|asked)\\W+(\\w+\\W+)?", ai,
  "\\W+(\\w+\\W+){0,2}(decide|choose|pick|make\\W+(\\w+\\W+)?decisions?|tell me what to do)\\b"
), ignore_case = TRUE)

re_decision <- regex(
  "\\b(decision\\w*|decid\\w*|choice\\w*|choos\\w*|chose\\w*|advice|advis\\w*|judg\\w*)\\b",
  ignore_case = TRUE
)

# In the everyday subreddits, people rarely write "I rely on ChatGPT":
# they rather say they asked it for advice ("ChatGPT told me it's a
# great choice", "asking AI for life advice"). This is the advice
# taking studied in the axis 1 literature. These posts are kept when
# they also mention a decision or advice. Not in the AI subreddits,
# where "I asked Claude" mostly introduces technical questions.
re_ai_advice <- regex(paste0(
  "\\b(asked|ask|asking|consulted|consult|consulting|turned to|turn to|talked to|talking to|talk to|ran it by|vent(ed|ing)? to)\\W+(\\w+\\W+){0,2}", ai, "\\b",
  "|\\b", ai, "\\W+(\\w+\\W+){0,1}(told|tells|said|says|suggested|suggests|recommended|recommends|advised|advises)\\b",
  "|\\b(use|used|using|uses)\\W+(\\w+\\W+){0,1}", ai, "\\W+(as|for)\\W+(\\w+\\W+){0,1}(advice|advisor|decisions?|therap\\w*|counsel\\w*|guidance|life coach)\\b"
), ignore_case = TRUE)

everyday_subs <- c("decidingtobebetter", "selfimprovement", "productivity", "college", "advice")

# Technical posts about coding agents, where a program "delegates" to
# another model: off topic
re_technical <- regex("\\b(mcp|sub-?agents?|orchestrat\\w*)\\b", ignore_case = TRUE)

posts <- posts %>%
  mutate(
    ai_reliance = str_detect(text, re_ai_reliance),
    ai_advice = tolower(subreddit) %in% everyday_subs &
      str_detect(text, re_ai_advice) & str_detect(text, re_decision)
  ) %>%
  filter(ai_reliance | ai_advice) %>%
  keep_step("Relevant: reliance on AI, or advice asked of AI (everyday)") %>%
  filter(!str_detect(text, re_technical)) %>%
  keep_step("No technical posts (coding agents)") %>%
  mutate(mentions_decision = str_detect(text, re_decision))

# Summaries of research papers posted in series by one account ("I'm
# finding and summarising interesting AI research papers every day"):
# these are not users speaking for themselves.
posts <- posts %>%
  filter(!str_detect(text, regex("summari[sz]ing interesting AI research papers", ignore_case = TRUE))) %>%
  keep_step("No automated paper summaries")

# Near-duplicates: the same message reposted in another subreddit with
# a few words changed. Two posts are near-duplicates when the Jaccard
# similarity of their vocabularies is above 0.6 (above that, every pair
# read was a repost of the same text). The oldest one is kept.
posts <- posts %>% arrange(created_utc)
post_vocab <- posts %>%
  mutate(rank = row_number()) %>%
  select(rank, text) %>%
  unnest_tokens(word, text) %>%
  distinct()
word_index <- factor(post_vocab$word)
presence <- Matrix::sparseMatrix(
  i = post_vocab$rank, j = as.integer(word_index), x = 1,
  dims = c(nrow(posts), nlevels(word_index))
)
shared <- as.matrix(Matrix::tcrossprod(presence))
sizes <- diag(shared)
jaccard <- shared / (outer(sizes, sizes, "+") - shared)
jaccard[lower.tri(jaccard, diag = TRUE)] <- 0
repost <- apply(jaccard >= 0.6, 2, any)   # an older post looks like it

posts <- posts[!repost, ] %>%
  keep_step("Near-duplicates removed (reposts of the same message)")


# =========================================================
# 5. Final corpus (anonymised: no usernames)
# =========================================================

corpus <- posts %>%
  mutate(
    date = as.Date(as.POSIXct(created_utc, origin = "1970-01-01", tz = "UTC")),
    year = as.integer(format(date, "%Y")),
    half_year = paste0(year, "-H", ifelse(as.integer(format(date, "%m")) <= 6, 1, 2)),
    # Audience: subreddits about AI or about everyday life
    audience = ifelse(tolower(subreddit) %in% everyday_subs, "Everyday", "AI"),
    selection_rule = case_when(
      ai_reliance & ai_advice ~ "both",
      ai_reliance ~ "reliance",
      TRUE ~ "advice"
    )
  ) %>%
  arrange(date) %>%
  # reddit_id first: inside transmute, "id" is then replaced by the
  # anonymous corpus identifier (R0001, R0002...)
  transmute(
    reddit_id = id,
    id = sprintf("R%04d", row_number()),
    subreddit,
    audience,
    date,
    year,
    half_year,
    query,
    score,
    num_comments,
    title,
    text,
    n_words,
    selection_rule,
    mentions_decision
  )

write_xlsx(corpus, "corpus/reddit_corpus.xlsx")
write_csv(corpus, "corpus/reddit_corpus.csv")
write_csv(steps, "corpus/selection_steps.csv")

print(steps, n = Inf)
cat("\nBy year:\n"); print(table(corpus$year))
cat("\nBy subreddit:\n"); print(table(corpus$subreddit))
cat("\nBy audience:\n"); print(table(corpus$audience))
cat("\nSelection rule, by audience:\n"); print(table(corpus$audience, corpus$selection_rule))
cat("\nMentions a decision or advice:\n"); print(table(corpus$mentions_decision))
