# =========================================================
#  AXIS 1 - BIBLIOMETRICS
#  Building and testing the Boolean search equation (OpenAlex)
#  Course method: concepts -> synonyms -> OR blocks -> AND
# =========================================================
# Run from the axis 1 project folder (outputs in search_equation/).
# Each term is tested on its own (number of results + first 3 titles),
# then each block, then the full equation.


# =========================================================
# 1. Packages
# =========================================================

library(httr2)
library(jsonlite)
library(dplyr)
library(readr)


# =========================================================
# 2. Concepts and candidate terms (suggested by an LLM)
# =========================================================

concepts <- list(
  AI = c(
    "artificial intelligence", "AI", "machine learning", "algorithm",
    "algorithmic", "large language model", "LLM", "ChatGPT",
    "generative AI", "chatbot", "decision support system",
    "automated decision", "recommender system", "robo-advisor"
  ),
  RELIANCE = c(
    "reliance", "overreliance", "over-reliance", "appropriate reliance",
    "delegation", "delegate", "dependence", "dependency",
    "algorithm aversion", "algorithm appreciation", "automation bias",
    "advice taking", "trust calibration", "complacency", "deference",
    "cognitive offloading"
  ),
  DECISION = c(
    "decision making", "decision-making", "decision", "judgment",
    "judgement", "choice", "advice", "human-AI collaboration"
  )
)

# Filters shared by all tests (the same as for the export)
common_filters <- paste0(
  ",from_publication_date:2015-01-01",
  ",to_publication_date:2026-10-22",
  ",language:en",
  ",type:article|review"
)

block <- function(terms) {
  paste0("title_and_abstract.search:(", paste0('"', terms, '"', collapse = " OR "), ")")
}


# =========================================================
# 3. Test function
# =========================================================

run_test <- function(filter) {
  for (attempt in 1:5) {
    resp <- request("https://api.openalex.org/works") |>
      req_url_query(
        filter = paste0(filter, common_filters),
        `per-page` = 3, select = "display_name"
      ) |>
      req_error(is_error = function(r) FALSE) |>
      req_perform()
    if (resp_status(resp) == 200) break
    Sys.sleep(5 * attempt)
  }
  res <- fromJSON(resp_body_string(resp))
  Sys.sleep(1)
  tibble(
    results = res$meta$count,
    first_titles = paste(substr(res$results$display_name, 1, 90), collapse = " | ")
  )
}


# =========================================================
# 4. Testing each term on its own
# =========================================================

if (!file.exists("search_equation/term_tests.csv")) {
  term_tests <- bind_rows(lapply(names(concepts), function(concept) {
    bind_rows(lapply(concepts[[concept]], function(term) {
      message(concept, ": ", term)
      bind_cols(tibble(concept = concept, term = term), run_test(block(term)))
    }))
  }))
  write_csv(term_tests, "search_equation/term_tests.csv")
}


# =========================================================
# 5. Terms kept (version 1 of the blocks)
# =========================================================
# Removed because they mostly bring in something else (see
# term_tests.csv):
#  - dependence, dependency: pharmacology, physics, linguistics
#  - deference: philosophy; complacency: ecology
#  - choice: school choice, economics; recommender system: e-commerce
#  - algorithmic: same as "algorithm" (stemmed search)
#  - decision-making: same as "decision making"
#  - human-AI collaboration: about collaboration, not decision

blocks_v1 <- list(
  AI = c(
    "artificial intelligence", "AI", "machine learning", "algorithm",
    "large language model", "LLM", "ChatGPT", "generative AI", "chatbot",
    "decision support system", "automated decision", "robo-advisor"
  ),
  RELIANCE = c(
    "reliance", "overreliance", "over-reliance", "delegation", "delegate",
    "algorithm aversion", "algorithm appreciation", "automation bias",
    "advice taking", "trust calibration", "cognitive offloading"
  ),
  DECISION = c("decision", "judgment", "judgement", "advice")
)

# Version 1 (22/10/2026): 7,393 results, but in a random sample of 12
# titles only 3 or 4 are on topic. The single words "reliance" and
# "delegate" appear in unrelated abstracts (power grids,
# cybersecurity...): the block is too broad.
#
# Version 2: the RELIANCE block keeps only precise phrases.

blocks <- blocks_v1
blocks$RELIANCE <- c(
  "overreliance", "over-reliance", "appropriate reliance",
  "reliance on AI", "reliance on artificial intelligence",
  "reliance on algorithms", "AI reliance",
  "algorithm aversion", "algorithm appreciation", "automation bias",
  "advice taking", "trust calibration", "cognitive offloading",
  "delegation to AI", "delegating to AI", "delegate to AI",
  "decision delegation", "delegation of decisions", "delegating decisions"
)


# =========================================================
# 6. Yield of the combined blocks
# =========================================================

block_tests <- bind_rows(
  bind_cols(tibble(equation = "AI AND RELIANCE"),
            run_test(paste(block(blocks$AI), block(blocks$RELIANCE), sep = ","))),
  bind_cols(tibble(equation = "AI AND RELIANCE AND DECISION"),
            run_test(paste(block(blocks$AI), block(blocks$RELIANCE), block(blocks$DECISION), sep = ",")))
)
print(block_tests, width = 200)


# =========================================================
# 7. Contribution of each term in the full equation
# =========================================================
# For each term: results for the term AND the other two blocks.
# A term that brings in many off-topic titles is removed.

context_tests <- bind_rows(lapply(names(blocks), function(concept) {
  others <- setdiff(names(blocks), concept)
  bind_rows(lapply(blocks[[concept]], function(term) {
    message("Context ", concept, ": ", term)
    f <- paste(c(block(term), sapply(blocks[others], block)), collapse = ",")
    bind_cols(tibble(concept = concept, term = term), run_test(f))
  }))
}))

write_csv(block_tests, "search_equation/block_tests.csv")
write_csv(context_tests, "search_equation/context_tests.csv")
