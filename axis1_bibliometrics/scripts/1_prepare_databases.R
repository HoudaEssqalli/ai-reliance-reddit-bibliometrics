# =========================================================
#  AXIS 1 - BIBLIOMETRICS
#  Preparing the three databases before merging them
#  base1 = Scopus, base2 = Web of Science, base3 = OpenAlex
# =========================================================
# Same Boolean equation in the three databases (version 2, see
# search_equation/0_test_search_equation.R), exports of 23/10/2026 in
# exports/.
#
# Run from the axis 1 project folder, before 2_merge_bases.R
# (the merging script provided in class, not republished here).


# =========================================================
# 1. Packages
# =========================================================

library(dplyr)
library(stringr)
library(bibliometrix)
library(writexl)


# =========================================================
# 2. base1: Scopus
# =========================================================
# A single CSV export: 2,052 results, all exported (citation,
# bibliographic information, abstract and keywords).

base1 <- read.csv(
  "exports/scopus.csv",
  stringsAsFactors = FALSE,
  check.names = FALSE
)

write.csv(base1, "base1.csv", row.names = FALSE, fileEncoding = "UTF-8")

cat("base1 (Scopus):", nrow(base1), "\n")


# =========================================================
# 3. base2: Web of Science
# =========================================================
# 1,475 results exported as three BibTeX files (Full Record), because
# WoS limits each export to 1,000 records: they are combined here.

base2 <- convert2df(
  c("exports/wos_1.bib", "exports/wos_2.bib", "exports/wos_3.bib"),
  dbsource = "wos",
  format = "bibtex"
)

base2 <- base2 %>%
  select(any_of(c("AU", "TI", "PY", "SO", "DI", "TC", "DT", "LA", "AB", "DE", "ID", "C1")))

write_xlsx(base2, "base2.xlsx")

cat("base2 (Web of Science):", nrow(base2), "\n")


# =========================================================
# 4. base3: OpenAlex
# =========================================================
# Export from openalex.org: 3,854 results (2019-2026, articles and
# reviews), sorted by relevance.

openalex <- read.csv(
  "exports/openalex_web_raw.csv",
  stringsAsFactors = FALSE,
  check.names = FALSE
)

# Language: documents marked as non-English are removed, as for
# Scopus and WoS. The language is empty for about a quarter of the
# records, almost all in English: they are kept.
english <- openalex %>%
  filter(Language %in% c("en", "") | is.na(Language))

cat("OpenAlex, non-English documents removed:", nrow(openalex) - nrow(english), "\n")

# Repositories (Zenodo, Figshare, arXiv, university repositories):
# OpenAlex classes them as "article", but unlike the Scopus and WoS
# records they are not peer-reviewed. They are removed.
published <- english %>%
  filter(!str_detect(tolower(Source), "zenodo|figshare|arxiv|repository"))

cat("OpenAlex, repositories removed:", nrow(english) - nrow(published), "\n")

# The 2,000 most relevant (the export keeps the relevance order)
base3 <- published %>% slice_head(n = 2000)

# Authors' countries: the first export did not have this column. A
# second export of the same search (exports/openalex_countries.csv,
# two-letter ISO codes) is matched by DOI, otherwise by title.
# The selection of the 2,000 records does not change.
countries <- read.csv("exports/openalex_countries.csv", stringsAsFactors = FALSE, check.names = FALSE)

clean_doi <- function(x) tolower(str_remove(str_trim(x), "^https?://doi.org/"))
clean_title <- function(x) str_squish(str_replace_all(tolower(x), "[[:punct:]]", " "))

countries_doi <- countries %>% filter(DOI != "") %>% distinct(key = clean_doi(DOI), .keep_all = TRUE)
countries_title <- countries %>% distinct(key = clean_title(Title), .keep_all = TRUE)

base3$Country <- coalesce(
  ifelse(base3$DOI != "", countries_doi$Country[match(clean_doi(base3$DOI), countries_doi$key)], NA),
  countries_title$Country[match(clean_title(base3$Title), countries_title$key)]
)

cat("OpenAlex, records with a country:", sum(!is.na(base3$Country) & base3$Country != ""), "\n")

write.csv(base3, "base3.csv", row.names = FALSE, fileEncoding = "UTF-8")

cat("base3 (OpenAlex):", nrow(base3), "\n")
