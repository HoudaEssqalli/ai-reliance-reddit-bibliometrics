# =========================================================
#  MIDTERM CSS - AXE 1
#  Fichier pour biblioshiny : les références uniques du corpus
#  final, avec résumés et mots-clés
# =========================================================
# CORPUS_FINAL.csv (script du prof) ne garde que 5 colonnes.
# Biblioshiny a besoin des résumés et des mots-clés : on reprend
# les trois exports au format bibliometrix et on supprime les
# doublons avec la même règle que merge_bases.R (DOI, puis titre).
#
# Ouvrir "Axe 1.Rproj", lancer 1_preparer_bases.R, 2_merge_bases.R,
# puis ce script. Résultat : corpus_biblioshiny.xlsx, à charger dans
# biblioshiny (Data > Load data > bibliometrix file).


# =========================================================
# 1. Packages
# =========================================================

library(bibliometrix)
library(dplyr)
library(stringr)
library(writexl)


# =========================================================
# 2. Scopus et Web of Science, lus par bibliometrix
# =========================================================

scopus <- convert2df("exports/scopus.csv", dbsource = "scopus", format = "csv")

wos <- convert2df(
  c("exports/wos_1.bib", "exports/wos_2.bib", "exports/wos_3.bib"),
  dbsource = "wos",
  format = "bibtex"
)


# =========================================================
# 3. OpenAlex (base3) mis au format bibliometrix
# =========================================================
# Auteurs "Prénom Nom|Prénom Nom" -> "NOM P;NOM P".
# Les mots-clés d'OpenAlex sont attribués par OpenAlex, pas par les
# auteurs : ils vont dans ID (comme les Keywords Plus de WoS et les
# Index Keywords de Scopus), DE reste vide.

auteur_court <- function(nom) {
  mots <- str_split(str_squish(nom), " ")[[1]]
  if (length(mots) < 2) return(toupper(nom))
  initiales <- paste(substr(mots[-length(mots)], 1, 1), collapse = "")
  toupper(paste(mots[length(mots)], initiales))
}

auteurs <- function(x) {
  vapply(x, function(a) {
    if (is.na(a) || a == "") return(NA_character_)
    paste(vapply(str_split(a, "\\|")[[1]], auteur_court, character(1)), collapse = ";")
  }, character(1), USE.NAMES = FALSE)
}

base3 <- read.csv("base3.csv", stringsAsFactors = FALSE, check.names = FALSE)

# Pays : codes ISO ("DE|DE|GB", un par auteur) -> noms au format bibliometrix
codes_pays <- read.csv("exports/codes_pays_iso2.csv", stringsAsFactors = FALSE)
noms_pays <- function(x) {
  vapply(x, function(codes) {
    if (is.na(codes) || codes == "") return(NA_character_)
    paste(codes_pays$pays[match(str_split(codes, "\\|")[[1]], codes_pays$code)], collapse = ";")
  }, character(1), USE.NAMES = FALSE)
}

openalex <- data.frame(
  AU = auteurs(base3$Author),
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
  PAYS_OA = noms_pays(base3$Country),
  DB = "OPENALEX",
  stringsAsFactors = FALSE
)


# =========================================================
# 4. Réunir les trois bases (même ordre que merge_bases.R)
# =========================================================

# C1 (adresses des auteurs) et RP (adresse de l'auteur correspondant)
# servent à bibliometrix pour les pays ; OpenAlex n'en fournit pas.
# CR (références citées) n'a pas été exporté (Full Record sans les
# références, comme dans le cours) ; biblioshiny attend pourtant la
# colonne : elle reste vide.
colonnes <- c("AU", "TI", "SO", "J9", "PY", "DT", "LA", "AB", "DE", "ID", "DI", "TC", "C1", "RP", "CR", "PAYS_OA", "DB")

garder_colonnes <- function(d) {
  d <- as.data.frame(d)
  for (col in setdiff(colonnes, names(d))) d[[col]] <- NA
  d[, colonnes]
}

M <- bind_rows(
  garder_colonnes(scopus) %>% mutate(PY = as.integer(PY), TC = as.numeric(TC)),
  garder_colonnes(wos) %>% mutate(PY = as.integer(PY), TC = as.numeric(TC)),
  garder_colonnes(openalex)
)

cat("Références réunies :", nrow(M), "\n")


# =========================================================
# 5. Doublons : même règle que merge_bases.R
# =========================================================

M <- M %>%
  mutate(
    DOI_NET = DI %>% tolower() %>% str_trim() %>%
      str_replace("^https?://doi.org/", "") %>% str_replace("^doi:", ""),
    TITRE_NET = TI %>% tolower() %>% str_trim() %>%
      str_replace_all("[[:punct:]]", " ") %>% str_squish()
  )

M$DOI_NET[M$DOI_NET == ""] <- NA
M$TITRE_NET[M$TITRE_NET == ""] <- NA

M <- M %>%
  arrange(is.na(DOI_NET)) %>%
  filter(is.na(DOI_NET) | !duplicated(DOI_NET)) %>%
  arrange(is.na(TITRE_NET)) %>%
  filter(is.na(TITRE_NET) | !duplicated(TITRE_NET)) %>%
  select(-DOI_NET, -TITRE_NET)

cat("Références uniques :", nrow(M), "\n")
print(table(M$DB))


# =========================================================
# 6. Champs calculés par bibliometrix et export
# =========================================================

M <- metaTagExtraction(M, Field = "SR")
M <- metaTagExtraction(M, Field = "AU_CO", sep = ";")
# Pour OpenAlex, pas d'adresses (C1) : les pays viennent de l'export dédié
M$AU_CO[M$DB == "OPENALEX"] <- M$PAYS_OA[M$DB == "OPENALEX"]
M$PAYS_OA <- NULL
M <- metaTagExtraction(M, Field = "AU_UN", sep = ";")
M$CR <- coalesce(M$CR, "")
class(M) <- c("bibliometrixDB", "data.frame")

save(M, file = "corpus_biblioshiny.RData")
write_xlsx(M, "corpus_biblioshiny.xlsx")

cat("corpus_biblioshiny.xlsx et corpus_biblioshiny.RData créés\n")
