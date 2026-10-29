# =========================================================
#  AXIS 1 - BIBLIOMETRICS
#  Summary of each database and of the merge (figures in the report)
# =========================================================
# Run from the axis 1 project folder, after 1_prepare_databases.R and
# 2_merge_bases.R.
# Output: database_summary.xlsx (one sheet per table)


# =========================================================
# 1. Packages
# =========================================================

library(readxl)
library(dplyr)
library(stringr)
library(igraph)
library(writexl)


# =========================================================
# 2. The three databases, reduced to the same fields
# =========================================================

base1 <- read.csv("base1.csv", stringsAsFactors = FALSE, check.names = FALSE)
base2 <- read_excel("base2.xlsx")
base3 <- read.csv("base3.csv", stringsAsFactors = FALSE, check.names = FALSE)

is_empty <- function(x) is.na(x) | str_trim(x) == "" | str_detect(x, "^\\[No abstract available\\]$")

standardise <- function(source, title, doi, year, abstract, keywords) {
  tibble(
    SOURCE = source,
    TITLE = title,
    DOI = ifelse(is_empty(doi), NA, doi),
    YEAR = as.integer(year),
    ABSTRACT = !is_empty(abstract),
    KEYWORDS = !is_empty(keywords)
  )
}

databases <- bind_rows(
  standardise("Scopus", base1$Title, base1$DOI, base1$Year, base1$Abstract, base1$`Author Keywords`),
  standardise("Web of Science", base2$TI, base2$DI, base2$PY, base2$AB, base2$DE),
  standardise("OpenAlex", base3$Title, base3$DOI, base3$Year, base3$Abstract, base3$Keyword)
) %>%
  mutate(
    # same normalisation as merge_bases.R
    DOI_CLEAN = DOI %>% tolower() %>% str_trim() %>%
      str_replace("^https?://doi.org/", "") %>% str_replace("^doi:", ""),
    TITLE_CLEAN = TITLE %>% tolower() %>% str_trim() %>%
      str_replace_all("[[:punct:]]", " ") %>% str_squish(),
    id = row_number()
  )


# =========================================================
# 3. Table 1: description and internal duplicates of each database
# =========================================================

table1 <- databases %>%
  group_by(SOURCE) %>%
  summarise(
    records = n(),
    with_DOI = sum(!is.na(DOI_CLEAN)),
    with_abstract = sum(ABSTRACT),
    with_keywords = sum(KEYWORDS),
    first_year = min(YEAR, na.rm = TRUE),
    last_year = max(YEAR, na.rm = TRUE),
    internal_duplicates_DOI = sum(duplicated(DOI_CLEAN[!is.na(DOI_CLEAN)])),
    internal_duplicates_title = sum(duplicated(TITLE_CLEAN[!is.na(TITLE_CLEAN)]))
  ) %>%
  arrange(match(SOURCE, c("Scopus", "Web of Science", "OpenAlex")))


# =========================================================
# 4. Grouping the records of the same document
# =========================================================
# Two records refer to the same document if they have the same DOI or
# the same normalised title (rule of merge_bases.R).

links <- bind_rows(
  databases %>% filter(!is.na(DOI_CLEAN)) %>% group_by(DOI_CLEAN) %>%
    filter(n() > 1) %>% reframe(from = first(id), to = id[-1]) %>%
    select(from, to),
  databases %>% filter(!is.na(TITLE_CLEAN)) %>% group_by(TITLE_CLEAN) %>%
    filter(n() > 1) %>% reframe(from = first(id), to = id[-1]) %>%
    select(from, to)
)

g <- graph_from_data_frame(links, directed = FALSE, vertices = data.frame(name = databases$id))
databases$document <- components(g)$membership[as.character(databases$id)]

documents <- databases %>%
  group_by(document) %>%
  summarise(
    Scopus = any(SOURCE == "Scopus"),
    WoS = any(SOURCE == "Web of Science"),
    OpenAlex = any(SOURCE == "OpenAlex"),
    YEAR = suppressWarnings(min(YEAR, na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  mutate(n_databases = Scopus + WoS + OpenAlex)


# =========================================================
# 5. Table 2: merge (results of merge_bases.R)
# =========================================================
# CORPUS_FINAL.csv keeps the column names of the class merging script
# (SOURCE, TITRE, AUTEURS, ANNEE, DOI).

final_corpus <- read.csv("CORPUS_FINAL.csv", stringsAsFactors = FALSE)

table2 <- tibble(
  step = c("Records imported (3 databases)", "Duplicates removed", "Final corpus (CORPUS_FINAL.csv)"),
  n = c(nrow(databases), nrow(databases) - nrow(final_corpus), nrow(final_corpus))
) %>%
  mutate(percent = round(100 * n / nrow(databases), 1))


# =========================================================
# 6. Table 3: overlap between the databases
# =========================================================

table3 <- documents %>%
  mutate(presence = case_when(
    n_databases == 3 ~ "In all 3 databases",
    Scopus & WoS ~ "Scopus + WoS",
    Scopus & OpenAlex ~ "Scopus + OpenAlex",
    WoS & OpenAlex ~ "WoS + OpenAlex",
    Scopus ~ "Scopus only",
    WoS ~ "WoS only",
    OpenAlex ~ "OpenAlex only"
  )) %>%
  count(presence, name = "documents") %>%
  arrange(desc(documents))


# =========================================================
# 7. Table 4: final corpus by year
# =========================================================

table4 <- final_corpus %>% count(year = ANNEE, name = "records")


# =========================================================
# 8. Export
# =========================================================

write_xlsx(
  list(
    "1_by_database" = table1,
    "2_merge" = table2,
    "3_overlap" = table3,
    "4_by_year" = table4
  ),
  "database_summary.xlsx"
)

print(table1, width = Inf)
print(table2)
print(table3)
cat("Distinct documents (DOI/title graph):", nrow(documents), "\n")
print(as_tibble(table4), n = Inf)
