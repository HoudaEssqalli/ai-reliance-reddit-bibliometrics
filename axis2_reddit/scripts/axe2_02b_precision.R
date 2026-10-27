# =========================================================
#  MIDTERM CSS - AXE 2
#  Contrôle de la précision du filtre de pertinence
# =========================================================
# Comme pour l'équation de l'axe 1 : 60 posts du corpus final tirés
# au hasard, lus un par un et classés en trois catégories :
#  - "coeur" : une personne s'en remet à l'IA pour une décision, un
#    jugement ou une tâche qu'elle faisait elle-même, ou s'interroge
#    sur cette dépendance ;
#  - "lie"   : la dépendance à l'IA est évoquée, mais comme sujet
#    général (société, entreprises) ou en passant ;
#  - "hors"  : le post ne parle pas de dépendance à l'IA.
#
# A lancer depuis le dossier Midterm_CSS, après le nettoyage :
#   Rscript scripts/axe2_02b_precision.R
# Le fichier corpus/echantillon_precision.xlsx est créé une fois, puis
# rempli à la main (colonne jugement) ; relancer le script donne le bilan.

library(dplyr)
library(readr)
library(readxl)
library(writexl)

fichier <- "corpus/echantillon_precision.xlsx"

if (!file.exists(fichier)) {
  corpus <- read_csv("corpus/corpus_reddit.csv", show_col_types = FALSE)
  set.seed(2026)
  corpus %>%
    slice_sample(n = 60) %>%
    transmute(id_reddit, subreddit, public, critere, titre, extrait = substr(texte, 1, 1500), jugement = NA_character_) %>%
    write_xlsx(fichier)
  cat("Échantillon créé :", fichier, "- remplir la colonne jugement (coeur / lie / hors)\n")
}

echantillon <- read_excel(fichier)

# L'échantillon a été tiré dans le corpus de 651 posts, juste avant le
# retrait des quasi-doublons et des résumés automatiques d'articles :
# on ne compte que les posts encore présents dans le corpus final.
if ("dans_corpus_final" %in% names(echantillon)) {
  cat("Posts de l'échantillon retirés depuis :", sum(!echantillon$dans_corpus_final), "\n")
  echantillon <- echantillon %>% filter(dans_corpus_final)
}

if (all(!is.na(echantillon$jugement))) {
  print(echantillon %>% count(critere, jugement) %>% tidyr::pivot_wider(names_from = jugement, values_from = n, values_fill = 0))

  bilan <- echantillon %>%
    count(public, jugement) %>%
    group_by(public) %>%
    mutate(part = round(100 * n / sum(n))) %>%
    ungroup()

  total <- echantillon %>%
    count(jugement) %>%
    mutate(part = round(100 * n / sum(n)))

  print(bilan)
  print(total)
  write_csv(bind_rows(bilan, total %>% mutate(public = "Ensemble")), "corpus/precision_bilan.csv")
} else {
  cat("Jugements incomplets :", sum(is.na(echantillon$jugement)), "posts restent à juger\n")
}
