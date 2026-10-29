# =========================================================
#  AXIS 2 - REDDIT
#  Collecting Reddit posts (Arctic Shift archive)
#  How users talk about relying on AI to make decisions
# =========================================================
# Run from the project folder:
#   Rscript scripts/01_collect_reddit.R
# If it is run again, the collection resumes where it stopped.


# =========================================================
# 1. Packages
# =========================================================

library(httr2)
library(jsonlite)
library(dplyr)


# =========================================================
# 2. Collection settings
# =========================================================

api <- "https://arctic-shift.photon-reddit.com/api/posts/search"

words <- c(
  "rely", "reliance", "dependent", "delegate",
  # added after the first screening (too few relevant posts)
  "addicted", "overreliance", "crutch"
)

# Period: January 2023 (after ChatGPT was released) to the collection
# date. r/ChatGPT is too large for the archive to search a whole
# quarter: there we take the first two weeks of each quarter.
# The other subreddits are searched by half-year.
quarters <- seq(as.Date("2023-01-01"), as.Date("2026-10-01"), by = "quarter")
half_years <- data.frame(
  start = seq(as.Date("2023-01-01"), as.Date("2026-07-01"), by = "6 months")
)
half_years$end <- c(half_years$start[-1], as.Date("2026-10-22"))

windows <- bind_rows(
  data.frame(sub = "ChatGPT", start = quarters, end = quarters + 14),
  merge(
    data.frame(sub = c(
      "OpenAI", "ClaudeAI", "ArtificialInteligence", "decidingtobebetter",
      # subreddits where people talk about their everyday decisions
      "selfimprovement", "productivity", "college", "Advice"
    )),
    half_years
  )
)

everyday_subs <- c("decidingtobebetter", "selfimprovement", "productivity", "college", "Advice")

# Order of the queries, from most to least useful:
# 1. everyday subreddits, posts that mention ChatGPT or AI: the
#    relevance filter keeps those where people rely on it (they cover
#    the gap on everyday decisions);
# 2. AI subreddits, reliance words.
# Reliance words in the everyday subreddits were tried and dropped:
# of 1,638 posts collected that way, only 15 pass the relevance filter
# (0.9%), against 3.1% for "chatgpt" and "AI" in these subreddits and
# 11.5% for reliance words in the AI subreddits. There, people mostly
# depend on their parents or their partner. The posts already collected
# that way stay in the raw corpus.
queries <- bind_rows(
  merge(data.frame(word = c("chatgpt", "AI")), filter(windows, sub %in% everyday_subs)),
  merge(data.frame(word = words), filter(windows, !sub %in% everyday_subs))
)

fields <- "id,subreddit,title,selftext,created_utc,score,num_comments,over_18,author"

raw_file <- "corpus/reddit_raw.rds"


# =========================================================
# 3. API call (with retries)
# =========================================================

call_api <- function(sub, word, after, before, sort = NULL, timeout = 45) {
  # The archive often answers "Timeout. Maybe slow down a bit":
  # we wait longer and longer between attempts
  for (attempt in 1:2) {
    resp <- tryCatch(
      request(api) |>
        req_url_query(
          subreddit = sub, query = word,
          after = after, before = before, sort = sort,
          limit = 100, fields = fields
        ) |>
        req_timeout(timeout) |>
        req_error(is_error = function(r) FALSE) |>
        req_perform(),
      error = function(e) NULL
    )
    if (!is.null(resp) && resp_status(resp) == 200) {
      content <- fromJSON(resp_body_string(resp))
      if (!is.null(content$data)) return(as.data.frame(content$data))
    }
    Sys.sleep(10 * attempt)
  }
  NULL
}

# The archive returns at most 100 posts per query. For ChatGPT and AI
# in the everyday subreddits, the next page is requested when this cap
# is reached (up to 3 pages, i.e. 300 posts per half-year).
# The next page starts after the last post of the previous page, in
# the order in which the archive returned them.
next_page_bounds <- function(prev, r) {
  t <- prev$created_utc
  if (t[1] <= t[length(t)]) {
    list(after = format(max(t), scientific = FALSE), before = format(r$end), sort = "asc")
  } else {
    list(after = format(r$start), before = format(min(t), scientific = FALSE), sort = "desc")
  }
}


# =========================================================
# 4. Collection loop
# =========================================================
# Several passes: each pass reruns the queries that failed, with a
# longer timeout.

results <- if (file.exists(raw_file)) readRDS(raw_file) else list()

save_result <- function(key, posts, word) {
  if (nrow(posts) > 0) posts$query <- word
  results[[key]] <<- posts
  message("OK     ", key, " : ", nrow(posts), " posts")
  saveRDS(results, raw_file)
  Sys.sleep(3)
}

for (pass in 1:4) {
  timeout <- 45 + 30 * (pass - 1)
  message("--- Pass ", pass, " (timeout ", timeout, " s)")
  failures <- 0

  for (i in seq_len(nrow(queries))) {
    r <- queries[i, ]
    key <- paste(r$sub, r$word, r$start, sep = "|")

    # Page 1
    if (is.null(results[[key]])) {
      posts <- call_api(r$sub, r$word, format(r$start), format(r$end), timeout = timeout)
      if (is.null(posts)) {
        message("FAILED ", key)
        failures <- failures + 1
        next
      }
      save_result(key, posts, r$word)
    }

    # Pages 2 and 3 (ChatGPT and AI in the everyday subreddits)
    if (!(r$sub %in% everyday_subs && r$word %in% c("chatgpt", "AI"))) next
    prev <- results[[key]]
    for (page in 2:3) {
      if (nrow(prev) < 100) break
      page_key <- paste0(key, "|p", page)
      if (is.null(results[[page_key]])) {
        b <- next_page_bounds(prev, r)
        more <- call_api(r$sub, r$word, b$after, b$before, sort = b$sort, timeout = timeout)
        if (is.null(more)) {
          message("FAILED ", page_key)
          failures <- failures + 1
          break
        }
        save_result(page_key, more, r$word)
      }
      prev <- results[[page_key]]
    }
  }

  if (failures == 0) break
  Sys.sleep(60)
}


# =========================================================
# 5. Summary
# =========================================================

raw <- bind_rows(results) %>% distinct(id, .keep_all = TRUE)

cat("\nSuccessful queries:", length(results), "\n")
cat("Unique posts collected:", nrow(raw), "\n")
