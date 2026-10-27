# =========================================================
#  MIDTERM CSS - AXE 2
#  Lemmatisation du corpus (udpipe, modèle anglais EWT)
# =========================================================
# Chaque mot est ramené à son lemme ("felt" -> "feel", "studies" ->
# "study", "relying" -> "rely"), pour que les fréquences, la
# classification et les topics comptent ensemble les formes d'un même
# mot. udpipe fait partie des extensions vues en cours (TP_cours28.R).
#
# A lancer depuis le dossier Midterm_CSS, après le nettoyage :
#   Rscript scripts/axe2_02c_lemmatisation.R
# Le modèle est dans modeles/ (udpipe_download_model("english-ewt")).
# Résultat : corpus/corpus_lemmes.csv (id, texte_lemmes, titre_lemmes)

library(udpipe)
library(dplyr)
library(readr)

posts <- read_csv("corpus/corpus_reddit.csv", show_col_types = FALSE)

modele <- udpipe_load_model("modeles/english-ewt-ud-2.5-191206.udpipe")

# Lemmatiser un vecteur de textes : on garde les lemmes des mots
# (pas la ponctuation, les nombres, les symboles)
lemmatiser <- function(textes, ids) {
  udpipe_annotate(modele, x = textes, doc_id = ids, parser = "none") %>%
    as.data.frame() %>%
    filter(!upos %in% c("PUNCT", "NUM", "SYM", "X")) %>%
    # Les noms propres gardent leur forme : le modèle lemmatise parfois
    # "AI" en "be" dans une question ("Is AI really a threat?")
    mutate(lemme = tolower(ifelse(upos == "PROPN", token, lemma))) %>%
    # Deux lemmes erronés du modèle, corrigés à la main
    mutate(lemme = recode(lemme, addicte = "addict", delegat = "delegate")) %>%
    group_by(doc_id) %>%
    summarise(lemmes = paste(lemme, collapse = " "), .groups = "drop") %>%
    rename(id = doc_id)
}

# Textes complets (fréquences, classification, LDA, STM)
lemmes_textes <- lemmatiser(posts$texte, posts$id) %>% rename(texte_lemmes = lemmes)

# Titres seuls (BTM, modèle pour textes courts)
lemmes_titres <- lemmatiser(posts$titre, posts$id) %>% rename(titre_lemmes = lemmes)

corpus_lemmes <- posts %>%
  select(id, public, annee) %>%
  left_join(lemmes_textes, by = "id") %>%
  left_join(lemmes_titres, by = "id")

write_csv(corpus_lemmes, "corpus/corpus_lemmes.csv")

cat("Posts lemmatisés :", sum(!is.na(corpus_lemmes$texte_lemmes)), "\n")
cat("Mots lemmatisés :", sum(lengths(strsplit(corpus_lemmes$texte_lemmes, " "))), "\n")
