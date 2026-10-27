# =========================================================
#  MIDTERM CSS - AXE 1
#  Préparation des trois bases avant la fusion
#  base1 = Scopus, base2 = Web of Science, base3 = OpenAlex
# =========================================================
# Même équation booléenne dans les trois bases (version 2, voir
# equation/0_tests_equation.R), exports du 23/10/2026 dans exports/.
#
# Ouvrir "Axe 1.Rproj", puis lancer ce script avant 2_merge_bases.R


# =========================================================
# 1. Packages
# =========================================================

library(dplyr)
library(stringr)
library(bibliometrix)
library(writexl)


# =========================================================
# 2. base1 : Scopus
# =========================================================
# Un seul export CSV : 2 052 résultats, tous exportés
# (citation, informations bibliographiques, résumé et mots-clés).

base1 <- read.csv(
  "exports/scopus.csv",
  stringsAsFactors = FALSE,
  check.names = FALSE
)

write.csv(base1, "base1.csv", row.names = FALSE, fileEncoding = "UTF-8")

cat("base1 (Scopus) :", nrow(base1), "\n")


# =========================================================
# 3. base2 : Web of Science
# =========================================================
# 1 475 résultats exportés en trois fichiers BibTeX (Full Record),
# car WoS limite chaque export à 1 000 notices : on les réunit.

base2 <- convert2df(
  c("exports/wos_1.bib", "exports/wos_2.bib", "exports/wos_3.bib"),
  dbsource = "wos",
  format = "bibtex"
)

base2 <- base2 %>%
  select(any_of(c("AU", "TI", "PY", "SO", "DI", "TC", "DT", "LA", "AB", "DE", "ID", "C1")))

write_xlsx(base2, "base2.xlsx")

cat("base2 (Web of Science) :", nrow(base2), "\n")


# =========================================================
# 4. base3 : OpenAlex
# =========================================================
# Export du site openalex.org : 3 854 résultats (2019-2026,
# articles et reviews), triés par pertinence.

openalex <- read.csv(
  "exports/openalex_web_brut.csv",
  stringsAsFactors = FALSE,
  check.names = FALSE
)

# Langue : on retire les documents indiqués non anglais, comme pour
# Scopus et WoS. La langue est vide pour environ un quart des notices,
# presque toutes en anglais : on les garde.
anglais <- openalex %>%
  filter(Language %in% c("en", "") | is.na(Language))

cat("OpenAlex, documents non anglais retirés :", nrow(openalex) - nrow(anglais), "\n")

# Dépôts (Zenodo, Figshare, arXiv, dépôts universitaires) : OpenAlex les
# classe en "article", mais ils ne sont pas relus par les pairs, à la
# différence des publications de Scopus et WoS. On les retire.
publies <- anglais %>%
  filter(!str_detect(tolower(Source), "zenodo|figshare|arxiv|repository"))

cat("OpenAlex, dépôts retirés :", nrow(anglais) - nrow(publies), "\n")

# Les 2 000 plus pertinents (l'export garde l'ordre de pertinence)
base3 <- publies %>% slice_head(n = 2000)

# Pays des auteurs : le premier export n'avait pas cette colonne. Un
# second export de la même recherche (exports/openalex_pays.csv, codes
# ISO à deux lettres) est rattaché par DOI, à défaut par titre.
# La sélection des 2 000 références ne change pas.
pays <- read.csv("exports/openalex_pays.csv", stringsAsFactors = FALSE, check.names = FALSE)

doi_net <- function(x) tolower(str_remove(str_trim(x), "^https?://doi.org/"))
titre_net <- function(x) str_squish(str_replace_all(tolower(x), "[[:punct:]]", " "))

pays_doi <- pays %>% filter(DOI != "") %>% distinct(cle = doi_net(DOI), .keep_all = TRUE)
pays_titre <- pays %>% distinct(cle = titre_net(Title), .keep_all = TRUE)

base3$Country <- coalesce(
  ifelse(base3$DOI != "", pays_doi$Country[match(doi_net(base3$DOI), pays_doi$cle)], NA),
  pays_titre$Country[match(titre_net(base3$Title), pays_titre$cle)]
)

cat("OpenAlex, références avec pays :", sum(!is.na(base3$Country) & base3$Country != ""), "\n")

write.csv(base3, "base3.csv", row.names = FALSE, fileEncoding = "UTF-8")

cat("base3 (OpenAlex) :", nrow(base3), "\n")
