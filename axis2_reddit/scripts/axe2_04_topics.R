# ================================================================
#  MIDTERM CSS - AXE 2
#  Topics : LDA, puis deux extensions (STM et BTM)
#  (sur le modèle de LDA_28.R et TP_cours28.R du cours)
# ================================================================
# Étapes :
#  1. Importer le corpus
#  2. Nettoyer les textes (anglais)
#  3. Construire la matrice Document-Terme
#  4. Choisir le nombre de topics K
#  5. Estimer le modèle LDA
#  6. BETA : mots caractéristiques des topics
#  7. GAMMA : topics présents dans chaque post, topic dominant
#  8. Comparer les topics selon le public et l'année
#  9. Évaluer le modèle (perplexité, cohérence)
# 10. Extension 1 : STM (l'effet du public et de l'année sur les topics)
# 11. Extension 2 : BTM sur les titres (textes courts)
#
# A lancer depuis le dossier Midterm_CSS :
#   Rscript scripts/axe2_04_topics.R
# Résultats : resultats/topics/


# ================================================================
# 0. PACKAGES
# ================================================================

library(tidyverse)
library(tidytext)
library(tm)
library(topicmodels)
library(ldatuning)
library(textmineR)
library(stm)
library(BTM)
library(textplot)   # graphique des bitermes du BTM (avec concaveman et ggraph)
library(writexl)

source("scripts/axe2_listes_mots.R")
source("scripts/axe2_style.R")

dossier <- "resultats/topics"
dir.create(dossier, recursive = TRUE, showWarnings = FALSE)


# ================================================================
# 1. IMPORTATION DU CORPUS
# ================================================================

posts <- read_csv("corpus/corpus_reddit.csv", show_col_types = FALSE)

cat("Posts :", nrow(posts), "\n")


# ================================================================
# 2. NETTOYAGE DU CORPUS
# ================================================================

# 2.1 Textes lemmatisés (axe2_02c_lemmatisation.R : "felt" -> "feel",
# "studies" -> "study"), en minuscules
lemmes <- read_csv("corpus/corpus_lemmes.csv", show_col_types = FALSE)
posts_nettoyes <- posts %>%
  left_join(lemmes %>% select(id, texte_lemmes), by = "id") %>%
  mutate(texte_net = str_to_lower(texte_lemmes) %>% str_replace_all("[’‘]", "'"))

# 2.2 Tokenisation
corpus_tidy <- posts_nettoyes %>%
  select(id, texte_net) %>%
  unnest_tokens(output = word, input = texte_net, token = "words")

# 2.3 Mots vides anglais, mots génériques, mots de la requête
# (le possessif "'s" est retiré d'abord : "chatgpt's" -> "chatgpt")
corpus_tidy <- corpus_tidy %>%
  mutate(word = str_remove(word, "'s$")) %>%
  anti_join(stop_words, by = "word") %>%
  filter(!word %in% c(extra_stopwords, mots_requete))

# 2.4 Mots alphabétiques de 3 lettres et plus
corpus_tidy <- corpus_tidy %>%
  mutate(word = str_remove_all(word, "'")) %>%
  filter(str_detect(word, "^[a-z]+$"), nchar(word) >= 3)

# 2.5 Mots trop rares : présents dans moins de 5 posts. Ils ne peuvent
# pas définir un topic et alourdissent l'estimation.
mots_gardes <- corpus_tidy %>%
  distinct(id, word) %>%
  count(word) %>%
  filter(n >= 5)

corpus_tidy <- corpus_tidy %>% semi_join(mots_gardes, by = "word")

cat("Tokens conservés :", nrow(corpus_tidy), "\n")
cat("Mots différents :", n_distinct(corpus_tidy$word), "\n")


# ================================================================
# 3. MATRICE DOCUMENT-TERME
# ================================================================

dtm <- corpus_tidy %>%
  count(id, word) %>%
  cast_dtm(document = id, term = word, value = n)

cat("DTM :", nDocs(dtm), "posts x", nTerms(dtm), "termes\n")

# Variables des posts, dans l'ordre des lignes de la DTM
meta <- tibble(id = Docs(dtm)) %>%
  left_join(posts %>% select(id, public, annee, titre, texte, subreddit), by = "id")


# ================================================================
# 4. CHOIX DU NOMBRE DE TOPICS K
# ================================================================
# Griffiths2004 et Deveaud2014 : valeur élevée recherchée
# CaoJuan2009 et Arun2010 : valeur faible recherchée
# Le choix final tient aussi compte de l'interprétabilité des topics.

k_metrics <- FindTopicsNumber(
  dtm,
  topics = 2:12,
  metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
  method = "Gibbs",
  control = list(seed = 77),
  mc.cores = 4,
  verbose = FALSE
)

print(k_metrics)

# Graphique du cours (FindTopicsNumber_plot), refait en français :
# chaque critère est ramené entre 0 et 1
K <- 10   # K retenu (justification en section 5)

graph_k <- k_metrics %>%
  pivot_longer(-topics, names_to = "critere", values_to = "valeur") %>%
  group_by(critere) %>%
  mutate(valeur = (valeur - min(valeur)) / (max(valeur) - min(valeur))) %>%
  ungroup() %>%
  mutate(sens = ifelse(critere %in% c("Griffiths2004", "Deveaud2014"), "Critères à maximiser", "Critères à minimiser")) %>%
  ggplot(aes(topics, valeur, shape = critere, linetype = critere)) +
  geom_vline(xintercept = K, colour = "#E67E22", linewidth = 1, alpha = 0.6) +
  geom_line() +
  geom_point(size = 2.5) +
  facet_wrap(~ sens, ncol = 1) +
  scale_x_continuous(breaks = 2:12) +
  labs(
    title = "Choix du nombre de topics K",
    subtitle = paste0("Valeurs ramenées entre 0 et 1 ; en orange, K = ", K, " retenu"),
    x = "Nombre de topics K", y = NULL, shape = NULL, linetype = NULL
  ) +
  theme_rapport()

ggsave(file.path(dossier, "choix_k.png"), graph_k, width = 8, height = 6, dpi = 300)


# ================================================================
# 5. ESTIMATION DU MODÈLE LDA
# ================================================================
# K retenu après lecture des critères (section 4) et des topics.
# Les quatre critères ne s'accordent pas (voir choix_k.png). On compare
# donc la cohérence moyenne des topics (textmineR, section 9) de K = 6
# à K = 10 : 0,095 (K = 6), 0,090 (7), 0,094 (8), 0,095 (9), 0,099 (10).
# Les écarts sont faibles ; K = 10 a la meilleure cohérence et dix
# topics distincts. À K = 6, études, travail et apprentissage sont
# fusionnés.
# (K est fixé à 10 en section 4.)

modele_lda <- LDA(
  dtm,
  k = K,
  method = "Gibbs",
  control = list(seed = 588, burnin = 1000, iter = 3000, thin = 100)
)

print(terms(modele_lda, 10))


# ================================================================
# 6. BETA : MOTS CARACTÉRISTIQUES DES TOPICS
# ================================================================

beta <- tidy(modele_lda, matrix = "beta")

top_mots <- beta %>%
  group_by(topic) %>%
  slice_max(beta, n = 10, with_ties = FALSE) %>%
  ungroup() %>%
  arrange(topic, -beta)

# Noms des topics : choisis après lecture des mots et des posts typiques
# (section 7.2), dans resultats/topics/noms_topics.csv (colonnes topic,
# nom). Tant que ce fichier n'existe pas : les trois premiers mots.
noms_topics <- top_mots %>%
  group_by(topic) %>%
  summarise(nom = paste(head(term, 3), collapse = ", ")) %>%
  mutate(nom = paste0("T", topic, " : ", nom)) %>%
  pull(nom)

fichier_noms <- file.path(dossier, "noms_topics.csv")
if (file.exists(fichier_noms)) {
  noms_choisis <- read_csv(fichier_noms, show_col_types = FALSE)
  if (nrow(noms_choisis) == K) noms_topics <- paste0("T", noms_choisis$topic, " : ", noms_choisis$nom)
}

graph_beta <- top_mots %>%
  mutate(
    topic = factor(noms_topics[topic], levels = noms_topics),
    term = reorder_within(term, beta, topic)
  ) %>%
  ggplot(aes(beta, term)) +
  geom_col(fill = "#2471A3") +
  scale_y_reordered() +
  facet_wrap(~ topic, ncol = 2, scales = "free_y") +
  labs(title = "Les 10 mots les plus probables de chaque topic (LDA)", x = expression(beta), y = NULL) +
  theme_rapport(10)

ggsave(file.path(dossier, "lda_beta.png"), graph_beta, width = 9, height = 2.3 * ceiling(K / 2), dpi = 300)


# ================================================================
# 7. GAMMA : TOPICS DE CHAQUE POST
# ================================================================

gamma <- tidy(modele_lda, matrix = "gamma") %>%
  rename(id = document)

# 7.1 Topic dominant de chaque post
dominant <- gamma %>%
  group_by(id) %>%
  slice_max(gamma, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  transmute(id, topic_dominant = topic, gamma_dominant = gamma)

topics_par_post <- gamma %>%
  mutate(topic = paste0("gamma_T", topic)) %>%
  pivot_wider(names_from = topic, values_from = gamma) %>%
  left_join(dominant, by = "id") %>%
  mutate(nom_topic = noms_topics[topic_dominant])

write_csv(topics_par_post, file.path(dossier, "topics_par_post.csv"))

# 7.2 Posts typiques de chaque topic (pour nommer les topics)
posts_typiques <- gamma %>%
  group_by(topic) %>%
  slice_max(gamma, n = 5, with_ties = FALSE) %>%
  ungroup() %>%
  left_join(meta %>% select(id, subreddit, titre, texte), by = "id") %>%
  mutate(extrait = str_trunc(texte, 400)) %>%
  select(topic, gamma, id, subreddit, titre, extrait)


# ================================================================
# 8. IMPORTANCE DES TOPICS, SELON LE PUBLIC ET L'ANNÉE
# ================================================================

gamma_meta <- gamma %>%
  left_join(meta %>% select(id, public, annee), by = "id") %>%
  mutate(nom = factor(noms_topics[topic], levels = noms_topics))

# 8.1 Importance moyenne dans tout le corpus
importance <- gamma_meta %>%
  group_by(nom) %>%
  summarise(gamma_moyen = mean(gamma), .groups = "drop")

graph_importance <- ggplot(importance, aes(x = reorder(nom, gamma_moyen), y = gamma_moyen)) +
  geom_col(fill = "#2471A3") +
  coord_flip() +
  labs(title = "Importance moyenne des topics", x = NULL, y = "Gamma moyen") +
  theme_rapport()

ggsave(file.path(dossier, "lda_importance.png"), graph_importance, width = 8, height = 4.5, dpi = 300)

# 8.2 Selon le public (subreddits de l'IA / du quotidien), topics
# rangés selon l'écart entre les deux publics
par_public <- gamma_meta %>%
  group_by(public, nom) %>%
  summarise(gamma_moyen = mean(gamma), .groups = "drop")

ordre_public <- par_public %>%
  pivot_wider(names_from = public, values_from = gamma_moyen) %>%
  mutate(ecart = Quotidien - IA) %>%
  arrange(ecart) %>%
  pull(nom)

graph_public <- par_public %>%
  mutate(nom = factor(nom, levels = ordre_public)) %>%
  ggplot(aes(x = nom, y = gamma_moyen, fill = public)) +
  geom_col(position = position_dodge(width = 0.75), width = 0.7) +
  coord_flip() +
  scale_fill_manual(values = couleurs_public) +
  labs(title = "Importance des topics selon le public", x = NULL, y = "Gamma moyen", fill = "Public") +
  theme_rapport()

ggsave(file.path(dossier, "lda_par_public.png"), graph_public, width = 8, height = 5, dpi = 300)

# 8.3 Selon l'année : un petit graphique par topic (une seule figure
# avec huit courbes serait illisible)
par_annee <- gamma_meta %>%
  group_by(annee, nom) %>%
  summarise(gamma_moyen = mean(gamma), .groups = "drop")

n_annee <- meta %>% count(annee)

graph_annee <- ggplot(par_annee, aes(x = annee, y = gamma_moyen)) +
  geom_line(colour = "#2471A3", linewidth = 0.9) +
  geom_point(colour = "#2471A3", size = 1.8) +
  facet_wrap(~ nom, ncol = 2) +
  scale_x_continuous(breaks = sort(unique(par_annee$annee))) +
  labs(
    title = "Évolution de la place des topics (2023-2026)",
    caption = paste("Posts par année :", paste(n_annee$annee, n_annee$n, sep = " : ", collapse = " ; ")),
    x = NULL, y = "Gamma moyen"
  ) +
  theme_rapport(10)

ggsave(file.path(dossier, "lda_par_annee.png"), graph_annee, width = 8, height = 2 * ceiling(K / 2) + 1, dpi = 300)


# ================================================================
# 9. ÉVALUATION DU MODÈLE
# ================================================================

# Perplexité (plus faible = mieux) et log-vraisemblance, comme dans le TP
perplexite <- perplexity(modele_lda, newdata = dtm)
log_vraisemblance <- as.numeric(logLik(modele_lda))

# Cohérence probabiliste de chaque topic (textmineR) : les mots
# principaux d'un topic apparaissent-ils ensemble dans les mêmes posts ?
phi <- exp(modele_lda@beta)
colnames(phi) <- modele_lda@terms
dtm_creuse <- Matrix::sparseMatrix(i = dtm$i, j = dtm$j, x = dtm$v,
                                   dims = dim(dtm), dimnames = dimnames(dtm))
coherence <- CalcProbCoherence(phi = phi, dtm = dtm_creuse, M = 10)

evaluation <- tibble(
  indicateur = c("Perplexité", "Log-vraisemblance", "Cohérence moyenne"),
  valeur = c(perplexite, log_vraisemblance, mean(coherence))
)
print(evaluation)

# Les mêmes indicateurs de K = 6 à K = 10 (même réglage que le modèle
# retenu), pour justifier le choix de K (section 5)
modeles_k <- map(set_names(6:10), function(k) {
  LDA(dtm, k = k, method = "Gibbs",
      control = list(seed = 588, burnin = 1000, iter = 3000, thin = 100))
})

evaluation_k <- map_dfr(6:10, function(k) {
  m <- modeles_k[[as.character(k)]]
  phi_k <- exp(m@beta)
  colnames(phi_k) <- m@terms
  tibble(
    K = k,
    perplexite = perplexity(m, newdata = dtm),
    log_vraisemblance = as.numeric(logLik(m)),
    coherence_moyenne = mean(CalcProbCoherence(phi = phi_k, dtm = dtm_creuse, M = 10))
  )
})
print(evaluation_k)

# Les conclusions dépendent-elles de K ? Pour K = 8, 9 et 10, chaque
# topic est rangé, d'après ses mots principaux (imprimés ci-dessous),
# dans la vie personnelle, dans l'IA et ses usages techniques, ou dans
# ni l'un ni l'autre (études, argent). On compare ensuite les deux
# publics et l'évolution dans le temps.
for (k in c("8", "9", "10")) {
  cat("\nK =", k, "\n")
  print(terms(modeles_k[[k]], 6))
}

topics_perso <- list("8" = c(1, 6, 7), "9" = c(2, 7, 8), "10" = c(2, 6, 8, 10))
topics_ia <- list("8" = c(2, 4, 5, 8), "9" = c(1, 4, 5, 9), "10" = c(3, 5, 7, 9))

robustesse_k <- map_dfr(c("8", "9", "10"), function(k) {
  g <- tidy(modeles_k[[k]], matrix = "gamma") %>%
    rename(id = document) %>%
    left_join(meta %>% select(id, public, annee), by = "id")
  dom <- g %>% group_by(id) %>% slice_max(gamma, n = 1, with_ties = FALSE) %>% ungroup()
  perso <- g %>%
    filter(topic %in% topics_perso[[k]]) %>%
    group_by(id, public, annee) %>%
    summarise(gamma = sum(gamma), .groups = "drop")
  tendance <- summary(lm(gamma ~ annee + public, data = perso))$coefficients["annee", ]
  tibble(
    K = as.integer(k),
    quotidien_dominant_perso = mean(dom$topic[dom$public == "Quotidien"] %in% topics_perso[[k]]),
    ia_dominant_perso = mean(dom$topic[dom$public == "IA"] %in% topics_perso[[k]]),
    ia_dominant_ia = mean(dom$topic[dom$public == "IA"] %in% topics_ia[[k]]),
    quotidien_dominant_ia = mean(dom$topic[dom$public == "Quotidien"] %in% topics_ia[[k]]),
    perso_par_an = tendance["Estimate"],
    p_perso_par_an = tendance["Pr(>|t|)"]
  )
})
print(robustesse_k)


# ================================================================
# 10. EXTENSION 1 : STM (STRUCTURAL TOPIC MODEL)
# ================================================================
# Le STM estime les topics et, en même temps, l'effet de variables
# des documents sur la place de chaque topic : ici le public
# (subreddits de l'IA ou du quotidien) et l'année.

stm_corpus <- readCorpus(dtm, type = "slam")
meta_stm <- meta %>% mutate(public = factor(public), annee = as.numeric(annee))

modele_stm <- stm(
  documents = stm_corpus$documents,
  vocab = stm_corpus$vocab,
  K = K,
  prevalence = ~ public + annee,
  data = meta_stm,
  init.type = "Spectral",
  seed = 588,
  verbose = FALSE
)

# Mots des topics : les plus probables et les plus exclusifs (FREX)
etiquettes_stm <- labelTopics(modele_stm, n = 8)
mots_stm <- tibble(
  topic = seq_len(K),
  plus_probables = apply(etiquettes_stm$prob, 1, paste, collapse = ", "),
  frex = apply(etiquettes_stm$frex, 1, paste, collapse = ", ")
)
print(mots_stm)

# Étiquettes : les quatre mots FREX de chaque topic du STM
noms_stm <- paste0("S", seq_len(K), " : ", apply(etiquettes_stm$frex[, 1:4], 1, paste, collapse = ", "))

# Effet du public et de l'année
effets <- estimateEffect(1:K ~ public + annee, modele_stm, metadata = meta_stm)
print(summary(effets))

# Coefficients du public et de l'année pour chaque topic (avec p-valeurs)
coefs_stm <- map_dfr(seq_len(K), function(k) {
  tab <- summary(effets)$tables[[k]]
  tibble(
    topic = k, nom = noms_stm[k],
    effet_quotidien = tab["publicQuotidien", "Estimate"],
    p_quotidien = tab["publicQuotidien", "Pr(>|t|)"],
    effet_annee = tab["annee", "Estimate"],
    p_annee = tab["annee", "Pr(>|t|)"]
  )
})
print(coefs_stm)

# Différence de place de chaque topic entre les deux publics, avec son
# intervalle de confiance à 95 % (le graphique de stm, refait en ggplot
# pour être lisible)
diff_public <- plot(
  effets, covariate = "public", method = "difference",
  cov.value1 = "Quotidien", cov.value2 = "IA", omit.plot = TRUE
)

graph_stm_public <- tibble(
  nom = noms_stm,
  difference = unlist(diff_public$means),
  bas = map_dbl(diff_public$cis, 1),
  haut = map_dbl(diff_public$cis, 2)
) %>%
  mutate(
    nom = reorder(nom, difference),
    cote = ifelse(difference > 0, "Quotidien", "IA")
  ) %>%
  ggplot(aes(x = difference, y = nom, colour = cote)) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey50") +
  geom_errorbarh(aes(xmin = bas, xmax = haut), height = 0.25) +
  geom_point(size = 2.5) +
  scale_colour_manual(values = couleurs_public, guide = "none") +
  labs(
    title = "STM : effet du public sur la place de chaque topic",
    subtitle = "Différence de proportion (quotidien moins IA), intervalle de confiance à 95 %, à année égale",
    x = "<- plus présent dans les subreddits de l'IA        plus présent au quotidien ->",
    y = NULL
  ) +
  theme_rapport()

ggsave(file.path(dossier, "stm_effet_public.png"), graph_stm_public, width = 8.5, height = 4.8, dpi = 300)

# Place moyenne de chaque topic du STM dans le corpus
graph_stm_prop <- tibble(nom = noms_stm, proportion = colMeans(modele_stm$theta)) %>%
  ggplot(aes(x = proportion, y = reorder(nom, proportion))) +
  geom_col(fill = "#2471A3") +
  scale_x_continuous(labels = scales::percent) +
  labs(title = "STM : place des topics dans le corpus", x = "Proportion moyenne", y = NULL) +
  theme_rapport()

ggsave(file.path(dossier, "stm_proportions.png"), graph_stm_prop, width = 8, height = 4.5, dpi = 300)


# ================================================================
# 11. EXTENSION 2 : BTM SUR LES TITRES
# ================================================================
# Le BTM (biterm topic model) est conçu pour les textes courts : il
# modélise les paires de mots plutôt que les documents. Les titres
# résument chaque post en une phrase : on vérifie si les topics des
# titres rejoignent ceux des posts entiers.

# Titres lemmatisés (axe2_02c_lemmatisation.R), mêmes filtres qu'en section 2
titres_tidy <- lemmes %>%
  transmute(id, titre_net = str_to_lower(titre_lemmes) %>% str_replace_all("[’‘]", "'")) %>%
  unnest_tokens(word, titre_net) %>%
  anti_join(stop_words, by = "word") %>%
  filter(!word %in% c(extra_stopwords, mots_requete)) %>%
  mutate(word = str_remove_all(word, "'")) %>%
  filter(str_detect(word, "^[a-z]+$"), nchar(word) >= 3) %>%
  select(doc_id = id, token = word)

set.seed(588)
modele_btm <- BTM(titres_tidy, k = K, iter = 1000, window = 15, trace = FALSE, detailed = TRUE)

mots_btm <- terms(modele_btm, top_n = 10)
mots_btm <- tibble(
  topic = seq_len(K),
  part = round(modele_btm$theta, 3),
  mots = map_chr(mots_btm, ~ paste(.x$token, collapse = ", "))
)
print(mots_btm)

# Graphique du script du cours (textplot) : les mots les plus probables
# de chaque topic du BTM, reliés par les bitermes qui les associent.
# La disposition des groupes est tirée au hasard : la graine ne change
# que la place des groupes sur la figure, pas le modèle.
set.seed(5)
graph_btm <- plot(
  modele_btm, top_n = 7,
  title = "BTM sur les titres des posts",
  subtitle = "Mots les plus probables de chaque topic, reliés par leurs bitermes"
) +
  ggplot2::theme(plot.margin = ggplot2::margin(15, 25, 15, 25))

ggsave(file.path(dossier, "btm_titres.png"), graph_btm, width = 11, height = 8.5, dpi = 250, bg = "white")


# ================================================================
# 12. EXPORT DES TABLEAUX
# ================================================================

write_xlsx(
  list(
    choix_k = k_metrics,
    lda_top_mots = top_mots,
    lda_noms = tibble(topic = seq_len(K), nom = noms_topics),
    lda_importance = importance,
    lda_par_public = par_public,
    lda_par_annee = par_annee,
    lda_posts_typiques = posts_typiques,
    lda_evaluation = evaluation,
    lda_evaluation_k = evaluation_k,
    lda_robustesse_k = robustesse_k,
    lda_coherence = tibble(topic = seq_len(K), coherence = coherence),
    stm_mots = mots_stm,
    stm_effets = coefs_stm,
    btm_titres = mots_btm
  ),
  file.path(dossier, "topics_axe2.xlsx")
)

save(modele_lda, modele_stm, modele_btm, effets, file = file.path(dossier, "modeles_topics.RData"))

cat("\nRésultats dans", dossier, "\n")
