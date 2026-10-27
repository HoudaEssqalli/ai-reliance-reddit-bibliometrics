# =========================================================
#  MIDTERM CSS - AXE 1
#  Bilan de chaque base et de la fusion (chiffres du rapport)
# =========================================================
# Ouvrir "Axe 1.Rproj", lancer 1_preparer_bases.R et
# 2_merge_bases.R, puis ce script.
# Résultat : bilan_bases.xlsx (un onglet par tableau)


# =========================================================
# 1. Packages
# =========================================================

library(readxl)
library(dplyr)
library(stringr)
library(igraph)
library(writexl)


# =========================================================
# 2. Les trois bases, ramenées aux mêmes champs
# =========================================================

base1 <- read.csv("base1.csv", stringsAsFactors = FALSE, check.names = FALSE)
base2 <- read_excel("base2.xlsx")
base3 <- read.csv("base3.csv", stringsAsFactors = FALSE, check.names = FALSE)

vide <- function(x) is.na(x) | str_trim(x) == "" | str_detect(x, "^\\[No abstract available\\]$")

uniformiser <- function(source, titre, doi, annee, resume, mots_cles) {
  tibble(
    SOURCE = source,
    TITRE = titre,
    DOI = ifelse(vide(doi), NA, doi),
    ANNEE = as.integer(annee),
    RESUME = !vide(resume),
    MOTS_CLES = !vide(mots_cles)
  )
}

bases <- bind_rows(
  uniformiser("Scopus", base1$Title, base1$DOI, base1$Year, base1$Abstract, base1$`Author Keywords`),
  uniformiser("Web of Science", base2$TI, base2$DI, base2$PY, base2$AB, base2$DE),
  uniformiser("OpenAlex", base3$Title, base3$DOI, base3$Year, base3$Abstract, base3$Keyword)
) %>%
  mutate(
    # même normalisation que merge_bases.R
    DOI_NET = DOI %>% tolower() %>% str_trim() %>%
      str_replace("^https?://doi.org/", "") %>% str_replace("^doi:", ""),
    TITRE_NET = TITRE %>% tolower() %>% str_trim() %>%
      str_replace_all("[[:punct:]]", " ") %>% str_squish(),
    id = row_number()
  )


# =========================================================
# 3. Tableau 1 : description et doublons internes de chaque base
# =========================================================

tableau1 <- bases %>%
  group_by(SOURCE) %>%
  summarise(
    references = n(),
    avec_DOI = sum(!is.na(DOI_NET)),
    avec_resume = sum(RESUME),
    avec_mots_cles = sum(MOTS_CLES),
    premiere_annee = min(ANNEE, na.rm = TRUE),
    derniere_annee = max(ANNEE, na.rm = TRUE),
    doublons_internes_DOI = sum(duplicated(DOI_NET[!is.na(DOI_NET)])),
    doublons_internes_titre = sum(duplicated(TITRE_NET[!is.na(TITRE_NET)]))
  ) %>%
  arrange(match(SOURCE, c("Scopus", "Web of Science", "OpenAlex")))


# =========================================================
# 4. Regrouper les notices d'un même document
# =========================================================
# Deux notices désignent le même document si elles ont le même DOI
# ou le même titre normalisé (règle de merge_bases.R).

liens <- bind_rows(
  bases %>% filter(!is.na(DOI_NET)) %>% group_by(DOI_NET) %>%
    filter(n() > 1) %>% reframe(from = first(id), to = id[-1]) %>%
    select(from, to),
  bases %>% filter(!is.na(TITRE_NET)) %>% group_by(TITRE_NET) %>%
    filter(n() > 1) %>% reframe(from = first(id), to = id[-1]) %>%
    select(from, to)
)

graphe <- graph_from_data_frame(liens, directed = FALSE, vertices = data.frame(name = bases$id))
bases$document <- components(graphe)$membership[as.character(bases$id)]

documents <- bases %>%
  group_by(document) %>%
  summarise(
    Scopus = any(SOURCE == "Scopus"),
    WoS = any(SOURCE == "Web of Science"),
    OpenAlex = any(SOURCE == "OpenAlex"),
    ANNEE = suppressWarnings(min(ANNEE, na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  mutate(nb_bases = Scopus + WoS + OpenAlex)


# =========================================================
# 5. Tableau 2 : fusion (résultats de merge_bases.R)
# =========================================================

corpus_final <- read.csv("CORPUS_FINAL.csv", stringsAsFactors = FALSE)

tableau2 <- tibble(
  etape = c("Références importées (3 bases)", "Doublons supprimés", "Corpus final (CORPUS_FINAL.csv)"),
  n = c(nrow(bases), nrow(bases) - nrow(corpus_final), nrow(corpus_final))
) %>%
  mutate(pourcentage = round(100 * n / nrow(bases), 1))


# =========================================================
# 6. Tableau 3 : recouvrement entre les bases
# =========================================================

tableau3 <- documents %>%
  mutate(presence = case_when(
    nb_bases == 3 ~ "Dans les 3 bases",
    Scopus & WoS ~ "Scopus + WoS",
    Scopus & OpenAlex ~ "Scopus + OpenAlex",
    WoS & OpenAlex ~ "WoS + OpenAlex",
    Scopus ~ "Scopus seulement",
    WoS ~ "WoS seulement",
    OpenAlex ~ "OpenAlex seulement"
  )) %>%
  count(presence, name = "documents") %>%
  arrange(desc(documents))


# =========================================================
# 7. Tableau 4 : corpus final par année
# =========================================================

tableau4 <- corpus_final %>% count(ANNEE, name = "references")


# =========================================================
# 8. Export
# =========================================================

write_xlsx(
  list(
    "1_par_base" = tableau1,
    "2_fusion" = tableau2,
    "3_recouvrement" = tableau3,
    "4_par_annee" = tableau4
  ),
  "bilan_bases.xlsx"
)

print(tableau1, width = Inf)
print(tableau2)
print(tableau3)
cat("Documents distincts (graphe DOI/titre) :", nrow(documents), "\n")
print(as_tibble(tableau4), n = Inf)
