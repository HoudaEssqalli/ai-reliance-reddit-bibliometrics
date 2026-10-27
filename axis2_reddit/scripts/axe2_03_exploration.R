############################################################
#                  MIDTERM CSS - AXE 2
#      Fréquences - Nuage de mots - Classification
#      (sur le modèle de Textm_26.R du cours)
############################################################
# Corpus : posts Reddit où des utilisateurs racontent s'en remettre
# à l'IA pour décider (corpus/corpus_reddit.csv, créé par
# axe2_02_reddit_nettoyage.R).
#
# A lancer depuis le dossier Midterm_CSS :
#   Rscript scripts/axe2_03_exploration.R
# Résultats : resultats/exploration/


#============================================================
# 0. PACKAGES
#============================================================

library(tm)
library(wordcloud)
library(RColorBrewer)
library(tidyverse)
library(tidytext)
library(factoextra)
library(writexl)

source("scripts/axe2_style.R")

dossier <- "resultats/exploration"
dir.create(dossier, recursive = TRUE, showWarnings = FALSE)


#============================================================
# 1. IMPORTATION DU CORPUS
#============================================================

posts <- read_csv("corpus/corpus_reddit.csv", show_col_types = FALSE)

# Textes lemmatisés (axe2_02c_lemmatisation.R), dans l'ordre des posts
lemmes <- read_csv("corpus/corpus_lemmes.csv", show_col_types = FALSE)
posts <- posts %>% left_join(lemmes %>% select(id, texte_lemmes), by = "id")

cat("Posts :", nrow(posts), "\n")
print(table(posts$public))

# Transformer le texte lemmatisé en corpus (un document = un post)
corpus <- Corpus(VectorSource(posts$texte_lemmes))


#============================================================
# 2. PRÉTRAITEMENT DU TEXTE
#============================================================
# Le corpus est en anglais et déjà lemmatisé ("felt" -> "feel") : on
# retire les mots vides anglais, la ponctuation et les nombres.

# Convertir en minuscules et uniformiser les apostrophes
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, content_transformer(function(x) gsub("[’‘]", "'", x)))

# Supprimer les mots vides anglais (liste tm + liste tidytext, plus complète)
mots_vides <- unique(c(stopwords("en"), stop_words$word))
corpus <- tm_map(corpus, removeWords, mots_vides)

# Supprimer la ponctuation, les nombres et les symboles (flèches, emojis)
corpus <- tm_map(corpus, removePunctuation, ucp = TRUE)
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, content_transformer(function(x) gsub("[^[:alpha:][:space:]]", " ", x)))


#------------------------------------------------------------
# Mots supplémentaires peu informatifs et mots de la requête
#------------------------------------------------------------
# Listes dans axe2_listes_mots.R (communes avec axe2_04_topics.R).
# Les mots de la requête sont comptés à part (tableau 4 bis).

source("scripts/axe2_listes_mots.R")

corpus_avec_requete <- corpus

corpus <- tm_map(corpus, removeWords, c(extra_stopwords, mots_requete))

# Supprimer les espaces multiples
corpus <- tm_map(corpus, stripWhitespace)


#============================================================
# 3. MATRICE DOCUMENTS-TERMES
#============================================================
# Mots de 3 lettres et plus (en anglais, "job", "app", "gpa" portent
# du sens ; le cours garde 4 lettres et plus pour le français)

course_dtm <- DocumentTermMatrix(corpus, control = list(wordLengths = c(3, Inf)))

course_dtm


#============================================================
# 4. TABLEAU DE FRÉQUENCE DES MOTS
#============================================================

mots_freq <- sort(slam::col_sums(course_dtm), decreasing = TRUE)

df_freq <- data.frame(
  word = names(mots_freq),
  freq = as.numeric(mots_freq),
  # nombre de posts où le mot apparaît
  posts = as.numeric(slam::col_sums(course_dtm > 0)[names(mots_freq)])
)

freq10 <- head(df_freq, 10)
print(freq10)


#------------------------------------------------------------
# 4 bis. Les mots de la requête (comptés avant leur retrait)
#------------------------------------------------------------

dtm_requete <- DocumentTermMatrix(
  corpus_avec_requete,
  control = list(dictionary = mots_requete, wordLengths = c(2, Inf))
)

freq_requete <- data.frame(
  word = colnames(dtm_requete),
  freq = as.numeric(slam::col_sums(dtm_requete)),
  posts = as.numeric(slam::col_sums(dtm_requete > 0))
) %>%
  filter(freq > 0) %>%
  arrange(desc(freq))

print(freq_requete)


#============================================================
# 5. GRAPHIQUE DES 20 MOTS LES PLUS FRÉQUENTS
#============================================================

top20 <- df_freq %>%
  slice_max(order_by = freq, n = 20, with_ties = FALSE) %>%
  mutate(word = reorder(word, freq))

graph_freq <- ggplot(top20, aes(x = word, y = freq)) +
  geom_col(fill = "#2471A3") +
  coord_flip() +
  labs(
    title = "Les 20 mots les plus fréquents",
    subtitle = paste(nrow(posts), "posts Reddit, mots de la requête retirés"),
    x = NULL,
    y = "Fréquence"
  ) +
  theme_rapport()

ggsave(file.path(dossier, "top20_mots.png"), graph_freq, width = 7, height = 6, dpi = 300)


#------------------------------------------------------------
# 5 bis. Mots les plus fréquents selon le public
#------------------------------------------------------------
# Public "IA" : subreddits consacrés à l'IA ; "Quotidien" :
# subreddits où l'on parle de ses décisions de tous les jours

freq_public <- tibble(doc = seq_len(nrow(posts)), public = posts$public) %>%
  inner_join(tidy(course_dtm) %>% mutate(doc = as.integer(document)), by = "doc") %>%
  count(public, term, wt = count, name = "freq") %>%
  group_by(public) %>%
  slice_max(freq, n = 15, with_ties = FALSE) %>%
  ungroup()

graph_public <- freq_public %>%
  mutate(term = reorder_within(term, freq, public)) %>%
  ggplot(aes(x = term, y = freq, fill = public)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  scale_x_reordered() +
  facet_wrap(~ public, scales = "free") +
  scale_fill_manual(values = couleurs_public) +
  labs(title = "Les 15 mots les plus fréquents selon le public", x = NULL, y = "Fréquence") +
  theme_rapport()

ggsave(file.path(dossier, "top15_par_public.png"), graph_public, width = 9, height = 6, dpi = 300)


#============================================================
# 6. NUAGE DE MOTS
#============================================================

word_pal <- brewer.pal(8, "Dark2")

png(file.path(dossier, "nuage_de_mots.png"), width = 2000, height = 2000, res = 300)
set.seed(123)
wordcloud(
  words = df_freq$word,
  freq = df_freq$freq,
  min.freq = 2,
  max.words = 100,
  random.order = FALSE,
  rot.per = 0.10,
  scale = c(3, 0.5),
  colors = word_pal
)
dev.off()


#------------------------------------------------------------
# 6 bis. Un nuage par public, pour les comparer (comme le TP du
# cours, qui fait un nuage par phase)
#------------------------------------------------------------

freq_par_public <- tibble(doc = seq_len(nrow(posts)), public = posts$public) %>%
  inner_join(tidy(course_dtm) %>% mutate(doc = as.integer(document)), by = "doc") %>%
  count(public, term, wt = count, name = "freq")

for (g in c("IA", "Quotidien")) {
  f <- freq_par_public %>% filter(public == g) %>% arrange(desc(freq))
  png(file.path(dossier, paste0("nuage_", tolower(g), ".png")), width = 2000, height = 2000, res = 300)
  set.seed(123)
  wordcloud(
    words = f$term,
    freq = f$freq,
    min.freq = 2,
    max.words = 80,
    random.order = FALSE,
    rot.per = 0.10,
    scale = c(3, 0.5),
    colors = word_pal
  )
  dev.off()
}


#============================================================
# 7. SÉLECTION DES MOTS POUR LA CLASSIFICATION
#============================================================
# Le cours garde les mots de fréquence > 2 sur un petit corpus.
# Ici le vocabulaire compte des milliers de mots : un dendrogramme
# serait illisible. On garde les 40 mots les plus fréquents.

n_mots_cah <- 40

mts_freq <- df_freq %>%
  slice_max(freq, n = n_mots_cah, with_ties = FALSE)


#============================================================
# 8. CLASSIFICATION HIÉRARCHIQUE
#============================================================

# Sélectionner dans la matrice uniquement les mots retenus
sel_mots_freq <- course_dtm[, colnames(course_dtm) %in% mts_freq$word]

# Présence (1) ou absence (0) du mot dans chaque post
mat_mots <- as.matrix(sel_mots_freq) > 0

# Calculer la distance entre les mots.
# La distance euclidienne du cours, appliquée aux effectifs, regroupe
# ici les mots selon leur fréquence (un groupe de 33 mots, un groupe
# de 6 mots parmi les plus fréquents, et "feel" seul ; section 8 bis) :
# les posts Reddit sont de longueur très inégale. On mesure plutôt si
# deux mots apparaissent dans les mêmes posts : distance de Jaccard
# (method = "binary").
distance <- dist(t(mat_mots), method = "binary")

# Classification hiérarchique avec méthode de Ward
CH <- hclust(distance, method = "ward.D2")


#============================================================
# 9-10. DENDROGRAMME EN k CLUSTERS
#============================================================

k_cah <- 3

plot_clusters <- fviz_dend(
  CH,
  k = k_cah,
  rect = TRUE,
  horiz = TRUE,
  cex = 0.8,
  main = paste("Classification hiérarchique des", n_mots_cah, "mots les plus fréquents"),
  ylab = "Hauteur (distance de Jaccard, méthode de Ward)"
)

# L'axe des hauteurs ne montre que les valeurs positives (la zone des
# étiquettes, à droite, n'a pas de sens), avec la virgule décimale
plot_clusters <- plot_clusters +
  scale_y_reverse(breaks = seq(0, 1.5, by = 0.5), labels = function(x) format(x, decimal.mark = ",")) +
  theme(plot.title = element_text(size = 11), axis.title = element_text(size = 9))

# Format compact : une fois la figure réduite à la largeur du rapport,
# les étiquettes des mots restent lisibles
ggsave(file.path(dossier, "dendrogramme.png"), plot_clusters, width = 5.5, height = 6.6, dpi = 300)
ggsave(file.path(dossier, "dendrogramme.pdf"), plot_clusters, width = 5.5, height = 6.6, device = pdf)

# Version verticale pour la présentation (mots en bas, lisibles en pleine largeur)
plot_vertical <- fviz_dend(CH, k = k_cah, rect = TRUE, horiz = FALSE, cex = 0.95, main = NULL, ylab = "",
                           labels_track_height = 1.1) +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())
ggsave(file.path(dossier, "dendrogramme_vertical.pdf"), plot_vertical, width = 8, height = 3.4, device = pdf)


#------------------------------------------------------------
# 8 bis. Méthode du cours : distance euclidienne sur les effectifs
#------------------------------------------------------------
# Même code que Textm_26.R (dist() par défaut, Ward, 3 groupes), sur
# les mêmes 40 mots, pour comparer avec la distance de Jaccard.

distance_eucl <- dist(t(as.matrix(sel_mots_freq)))
CH_eucl <- hclust(distance_eucl, method = "ward.D2")

groupes_eucl <- cutree(CH_eucl, k = k_cah)
print(table(groupes_eucl))

resultats_eucl <- data.frame(Mot = names(groupes_eucl), Cluster = groupes_eucl) %>%
  left_join(df_freq %>% select(Mot = word, Frequence = freq), by = "Mot") %>%
  arrange(Cluster, desc(Frequence))

plot_eucl <- fviz_dend(
  CH_eucl,
  k = k_cah,
  rect = TRUE,
  horiz = TRUE,
  cex = 0.8,
  main = "Distance euclidienne (méthode du cours)",
  ylab = "Hauteur (distance euclidienne, méthode de Ward)"
) +
  theme(plot.title = element_text(size = 11), axis.title = element_text(size = 9))

ggsave(file.path(dossier, "dendrogramme_euclidien.png"), plot_eucl, width = 5.5, height = 6.6, dpi = 300)
ggsave(file.path(dossier, "dendrogramme_euclidien.pdf"), plot_eucl, width = 5.5, height = 6.6, device = pdf)


#============================================================
# 11. IDENTIFIER LES MOTS DE CHAQUE CLUSTER
#============================================================

groupes <- cutree(CH, k = k_cah)

resultats_clusters <- data.frame(Mot = names(groupes), Cluster = groupes) %>%
  left_join(df_freq %>% select(Mot = word, Frequence = freq), by = "Mot") %>%
  arrange(Cluster, desc(Frequence))

print(resultats_clusters)


#============================================================
# 12. EXPORT DES TABLEAUX
#============================================================

write_xlsx(
  list(
    frequences = df_freq,
    mots_requete = freq_requete,
    top15_par_public = freq_public,
    clusters_cah = resultats_clusters,
    clusters_cah_euclidien = resultats_eucl
  ),
  file.path(dossier, "exploration_axe2.xlsx")
)

cat("\nRésultats dans", dossier, "\n")
