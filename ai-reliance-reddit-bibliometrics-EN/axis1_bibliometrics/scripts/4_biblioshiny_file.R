# =========================================================
#  AXIS 1 - BIBLIOMETRICS
#  File for biblioshiny: the unique records of the final corpus,
#  with abstracts and keywords
# =========================================================
# CORPUS_FINAL.csv (the class merging script) keeps only 5 columns.
# Biblioshiny needs abstracts and keywords: the three exports are read
# again in bibliometrix format and duplicates are removed with the
# same rule as merge_bases.R (DOI, then title).
#
# Run from the axis 1 project folder, after 1_prepare_databases.R and
# 2_merge_bases.R. Output: corpus_biblioshiny.xlsx, to load in
# biblioshiny (Data > Load data > bibliometrix file).


# =========================================================
# 1. Packages
# =========================================================

library(bibliometrix)
library(dplyr)
library(stringr)
library(writexl)


# =========================================================
# 2. Scopus and Web of Science, read by bibliometrix
# =========================================================

scopus <- convert2df("exports/scopus.csv", dbsource = "scopus", format = "csv")

wos <- convert2df(
  c("exports/wos_1.bib", "exports/wos_2.bib", "exports/wos_3.bib"),
  dbsource = "wos",
  format = "bibtex"
)


# =========================================================
# 3. OpenAlex (base3) in bibliometrix format
# =========================================================
# Authors "First Last|First Last" -> "LAST F;LAST F".
# OpenAlex keywords are assigned by OpenAlex, not by the authors: they
# go into ID (like WoS Keywords Plus and Scopus Index Keywords), and DE
# stays empty.

short_author <- function(name) {
  parts <- str_split(str_squish(name), " ")[[1]]
  if (length(parts) < 2) return(toupper(name))
  initials <- paste(substr(parts[-length(parts)], 1, 1), collapse = "")
  toupper(paste(parts[length(parts)], initials))
}

authors <- function(x) {
  vapply(x, function(a) {
    if (is.na(a) || a == "") return(NA_character_)
    paste(vapply(str_split(a, "\\|")[[1]], short_author, character(1)), collapse = ";")
  }, character(1), USE.NAMES = FALSE)
}

base3 <- read.csv("base3.csv", stringsAsFactors = FALSE, check.names = FALSE)

# Countries: ISO codes ("DE|DE|GB", one per author) -> names in
# bibliometrix format
country_codes <- read.csv("exports/country_codes_iso2.csv", stringsAsFactors = FALSE)
country_names <- function(x) {
  vapply(x, function(codes) {
    if (is.na(codes) || codes == "") return(NA_character_)
    paste(country_codes$country[match(str_split(codes, "\\|")[[1]], country_codes$code)], collapse = ";")
  }, character(1), USE.NAMES = FALSE)
}

openalex <- data.frame(
  AU = authors(base3$Author),
  TI = toupper(base3$Title),
  SO = toupper(base3$Source),
  J9 = toupper(base3$Source),
  PY = as.integer(base3$Year),
  DT = toupper(base3$Type),
  LA = "ENGLISH",
  AB = toupper(base3$Abstract),
  DE = NA_character_,
  ID = toupper(str_replace_all(base3$Keyword, "\\|", "; ")),
  DI = str_remove(base3$DOI, "^https?://doi.org/"),
  TC = as.numeric(base3$`Citation count`),
  C1 = NA_character_,
  RP = NA_character_,
  COUNTRY_OA = country_names(base3$Country),
  DB = "OPENALEX",
  stringsAsFactors = FALSE
)


# =========================================================
# 4. Combining the three databases (same order as merge_bases.R)
# =========================================================

# C1 (author addresses) and RP (corresponding author address) are used
# by bibliometrix for countries; OpenAlex does not provide them.
# CR (cited references) was not exported (Full Record without the
# references, as in class); biblioshiny still expects the column: it
# stays empty.
columns <- c("AU", "TI", "SO", "J9", "PY", "DT", "LA", "AB", "DE", "ID", "DI", "TC", "C1", "RP", "CR", "COUNTRY_OA", "DB")

keep_columns <- function(d) {
  d <- as.data.frame(d)
  for (col in setdiff(columns, names(d))) d[[col]] <- NA
  d[, columns]
}

M <- bind_rows(
  keep_columns(scopus) %>% mutate(PY = as.integer(PY), TC = as.numeric(TC)),
  keep_columns(wos) %>% mutate(PY = as.integer(PY), TC = as.numeric(TC)),
  keep_columns(openalex)
)

cat("Records combined:", nrow(M), "\n")


# =========================================================
# 5. Duplicates: same rule as merge_bases.R
# =========================================================

M <- M %>%
  mutate(
    DOI_CLEAN = DI %>% tolower() %>% str_trim() %>%
      str_replace("^https?://doi.org/", "") %>% str_replace("^doi:", ""),
    TITLE_CLEAN = TI %>% tolower() %>% str_trim() %>%
      str_replace_all("[[:punct:]]", " ") %>% str_squish()
  )

M$DOI_CLEAN[M$DOI_CLEAN == ""] <- NA
M$TITLE_CLEAN[M$TITLE_CLEAN == ""] <- NA

M <- M %>%
  arrange(is.na(DOI_CLEAN)) %>%
  filter(is.na(DOI_CLEAN) | !duplicated(DOI_CLEAN)) %>%
  arrange(is.na(TITLE_CLEAN)) %>%
  filter(is.na(TITLE_CLEAN) | !duplicated(TITLE_CLEAN)) %>%
  select(-DOI_CLEAN, -TITLE_CLEAN)

cat("Unique records:", nrow(M), "\n")
print(table(M$DB))


# =========================================================
# 6. Fields computed by bibliometrix, and export
# =========================================================

M <- metaTagExtraction(M, Field = "SR")
M <- metaTagExtraction(M, Field = "AU_CO", sep = ";")
# For OpenAlex there are no addresses (C1): countries come from the
# dedicated export
M$AU_CO[M$DB == "OPENALEX"] <- M$COUNTRY_OA[M$DB == "OPENALEX"]
M$COUNTRY_OA <- NULL
M <- metaTagExtraction(M, Field = "AU_UN", sep = ";")
M$CR <- coalesce(M$CR, "")
class(M) <- c("bibliometrixDB", "data.frame")

save(M, file = "corpus_biblioshiny.RData")
write_xlsx(M, "corpus_biblioshiny.xlsx")

cat("corpus_biblioshiny.xlsx and corpus_biblioshiny.RData created\n")
