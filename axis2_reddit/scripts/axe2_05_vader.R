# ======================================================================
#  MIDTERM CSS - AXE 2
#  Analyse des sentiments avec VADER
#  (sur le modèle de vader_01_28.R et vader_28_TP.R du cours)
# ======================================================================
# PRINCIPE (comme le TP) :
# Préparation du corpus
#       ↓
# Scores de sentiment phrase par phrase (NEG - NEU - POS - COMPOUND)
#       ↓
# Score moyen de chaque post, puis classe : Négatif / Neutre / Positif
#       ↓
# Réponses aux questions Q1 à Q14
#
# VADER est conçu pour l'anglais des réseaux sociaux et pour des
# phrases courtes : comme dans vader_01_28.R (get_sentences), on note
# chaque phrase, puis on fait la moyenne par post.
#
# A lancer depuis le dossier Midterm_CSS, après axe2_04_topics.R :
#   Rscript scripts/axe2_05_vader.R
# Résultats : resultats/vader/


# ======================================================================
# 1. CHARGEMENT DES PACKAGES
# ======================================================================

library(vader)
library(syuzhet)
library(tidyverse)
library(readxl)
library(writexl)
library(caret)

source("scripts/axe2_style.R")

dossier <- "resultats/vader"
dir.create(dossier, recursive = TRUE, showWarnings = FALSE)


# ======================================================================
# 2. IMPORTATION DU CORPUS
# ======================================================================

posts <- read_csv("corpus/corpus_reddit.csv", show_col_types = FALSE)

# Topic dominant de chaque post (LDA, axe2_04_topics.R)
topics <- read_csv("resultats/topics/topics_par_post.csv", show_col_types = FALSE) %>%
  select(id, topic_dominant, nom_topic)

posts <- posts %>% left_join(topics, by = "id")

cat("Posts :", nrow(posts), "\n")


# ======================================================================
# 3. DÉCOUPAGE EN PHRASES
# ======================================================================

phrases <- posts %>%
  select(id, texte) %>%
  mutate(phrase = map(texte, get_sentences)) %>%
  select(-texte) %>%
  unnest(phrase) %>%
  filter(str_count(phrase, "\\S+") >= 3)

cat("Phrases :", nrow(phrases), "\n")


# ======================================================================
# 4. SCORES VADER DE CHAQUE PHRASE
# ======================================================================

scores_phrases <- vader_df(phrases$phrase)

phrases <- phrases %>%
  mutate(
    compound = scores_phrases$compound,
    pos = scores_phrases$pos,
    neu = scores_phrases$neu,
    neg = scores_phrases$neg
  )


# ======================================================================
# 5. SCORE DE CHAQUE POST ET CLASSIFICATION
# ======================================================================
# Compound >= +0.05 : Positif
# Compound <= -0.05 : Négatif
# Entre les deux     : Neutre

scores_posts <- phrases %>%
  group_by(id) %>%
  summarise(
    VADER_COMPOUND = mean(compound, na.rm = TRUE),
    VADER_POS = mean(pos, na.rm = TRUE),
    VADER_NEU = mean(neu, na.rm = TRUE),
    VADER_NEG = mean(neg, na.rm = TRUE),
    part_phrases_negatives = mean(compound <= -0.05, na.rm = TRUE),
    # phrases fortement chargées (|compound| >= 0.5)
    n_tres_positives = sum(compound >= 0.5, na.rm = TRUE),
    n_tres_negatives = sum(compound <= -0.5, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(ambivalent = n_tres_positives > 0 & n_tres_negatives > 0)

data <- posts %>%
  left_join(scores_posts, by = "id") %>%
  mutate(
    Sentiment_VADER = case_when(
      VADER_COMPOUND >= 0.05 ~ "Positif",
      VADER_COMPOUND <= -0.05 ~ "Négatif",
      TRUE ~ "Neutre"
    ),
    Sentiment_VADER = factor(Sentiment_VADER, levels = c("Négatif", "Neutre", "Positif"))
  )

couleurs <- couleurs_sentiment


# ######################################################################
# Q1. QUELLE EST LA TONALITÉ GÉNÉRALE DES POSTS ?
# ######################################################################

Q1 <- data %>%
  count(Sentiment_VADER) %>%
  mutate(pourcentage = round(100 * n / sum(n), 1))

print(Q1)

# Comme vader_01_28.R, la même répartition au niveau des phrases
Q1_phrases <- phrases %>%
  mutate(classe = case_when(
    compound >= 0.05 ~ "Positif",
    compound <= -0.05 ~ "Négatif",
    TRUE ~ "Neutre"
  )) %>%
  count(classe) %>%
  mutate(pourcentage = round(100 * n / sum(n), 1))

print(Q1_phrases)

graph_Q1 <- ggplot(Q1, aes(x = Sentiment_VADER, y = n, fill = Sentiment_VADER)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = paste0(n, " (", pourcentage, " %)")), vjust = -0.5) +
  scale_fill_manual(values = couleurs) +
  labs(title = "Répartition des sentiments (VADER)", x = NULL, y = "Nombre de posts") +
  theme_rapport()

ggsave(file.path(dossier, "Q1_tonalite.png"), graph_Q1, width = 6, height = 4.5, dpi = 300)


# ######################################################################
# Q2. QUELLE EST L'INTENSITÉ DES SENTIMENTS ?
# ######################################################################

Q2 <- data %>%
  summarise(
    moyenne = mean(VADER_COMPOUND),
    mediane = median(VADER_COMPOUND),
    ecart_type = sd(VADER_COMPOUND),
    minimum = min(VADER_COMPOUND),
    maximum = max(VADER_COMPOUND)
  )

print(Q2)

graph_Q2 <- ggplot(data, aes(x = VADER_COMPOUND)) +
  geom_histogram(binwidth = 0.05, fill = "steelblue", colour = "white") +
  geom_vline(xintercept = c(-0.05, 0.05), linetype = "dashed") +
  labs(
    title = "Distribution du score compound moyen des posts",
    x = "Compound moyen (de -1 à +1)", y = "Nombre de posts"
  ) +
  theme_rapport()

ggsave(file.path(dossier, "Q2_intensite.png"), graph_Q2, width = 7, height = 4.5, dpi = 300)


# ######################################################################
# Q3. LES POSTS SONT-ILS FORTEMENT POLARISÉS ?
# ######################################################################
# Le TP juge un texte "polarisé" quand |compound| >= 0.5. Ici le score
# d'un post est la moyenne de ses phrases, donc resserré autour de 0 :
# on applique le seuil aux phrases, et on repère les posts ambivalents,
# qui contiennent à la fois des phrases très positives et très
# négatives.

# Indicateur du TP : part des posts dont |compound| >= 0.5 ("Forte")
Q3_tp <- data %>%
  mutate(Intensite = if_else(abs(VADER_COMPOUND) >= 0.5, "Forte", "Modérée")) %>%
  count(Intensite) %>%
  mutate(pourcentage = round(100 * n / sum(n), 1))

print(Q3_tp)

Q3 <- tibble(
  part_phrases_polarisees = mean(abs(phrases$compound) >= 0.5, na.rm = TRUE),
  part_posts_avec_phrase_tres_negative = mean(data$n_tres_negatives > 0),
  part_posts_avec_phrase_tres_positive = mean(data$n_tres_positives > 0),
  part_posts_ambivalents = mean(data$ambivalent)
)

print(Q3)


# ######################################################################
# Q4. QUELS SONT LES PRINCIPAUX THÈMES ?
# ######################################################################
# Thèmes = topic dominant du modèle LDA (axe2_04_topics.R)

Q4 <- data %>%
  count(nom_topic) %>%
  mutate(pourcentage = round(100 * n / sum(n), 1)) %>%
  arrange(desc(n))

print(Q4)


# ######################################################################
# Q5. LE SENTIMENT VARIE-T-IL SELON LE THÈME ?
# ######################################################################

Q5 <- data %>%
  group_by(nom_topic) %>%
  summarise(
    n = n(),
    compound_moyen = mean(VADER_COMPOUND),
    part_negatifs = mean(Sentiment_VADER == "Négatif"),
    part_positifs = mean(Sentiment_VADER == "Positif"),
    .groups = "drop"
  ) %>%
  arrange(compound_moyen)

print(Q5)

# Comme le TP : test de Kruskal-Wallis
Q5_kruskal <- kruskal.test(VADER_COMPOUND ~ factor(nom_topic), data = data)
print(Q5_kruskal)

graph_Q5 <- data %>%
  count(nom_topic, Sentiment_VADER) %>%
  group_by(nom_topic) %>%
  mutate(part = n / sum(n)) %>%
  ungroup() %>%
  # thèmes rangés du plus négatif (en haut) au moins négatif
  mutate(nom_topic = factor(nom_topic, levels = rev(Q5$nom_topic[order(-Q5$part_negatifs)]))) %>%
  ggplot(aes(x = nom_topic, y = part, fill = Sentiment_VADER)) +
  geom_col() +
  coord_flip() +
  scale_fill_manual(values = couleurs) +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Sentiment selon le thème (topic dominant de la LDA)", x = NULL, y = NULL, fill = NULL) +
  theme_rapport()

ggsave(file.path(dossier, "Q5_sentiment_par_theme.png"), graph_Q5, width = 8, height = 4.5, dpi = 300)


# ######################################################################
# Q6. QUELS THÈMES SONT LES PLUS POLARISÉS ?
# ######################################################################

Q6 <- data %>%
  group_by(nom_topic) %>%
  summarise(
    n = n(),
    # indicateur du TP : |compound| moyen
    polarisation_moyenne = mean(abs(VADER_COMPOUND)),
    part_avec_phrase_tres_negative = mean(n_tres_negatives > 0),
    part_ambivalents = mean(ambivalent),
    .groups = "drop"
  ) %>%
  arrange(desc(part_ambivalents))

print(Q6)


# ######################################################################
# Q7. LE SENTIMENT DIFFÈRE-T-IL SELON LE PUBLIC ?
# ######################################################################
# Public "IA" (subreddits de l'IA) ou "Quotidien" (subreddits de la
# vie de tous les jours). Test de Wilcoxon sur le compound moyen et
# test du khi-deux sur les classes.

Q7 <- data %>%
  group_by(public) %>%
  summarise(
    n = n(),
    compound_moyen = mean(VADER_COMPOUND),
    part_negatifs = mean(Sentiment_VADER == "Négatif"),
    part_positifs = mean(Sentiment_VADER == "Positif"),
    .groups = "drop"
  )

print(Q7)
Q7_wilcoxon <- wilcox.test(VADER_COMPOUND ~ public, data = data)
Q7_khi2 <- chisq.test(table(data$public, data$Sentiment_VADER))
print(Q7_wilcoxon)
print(Q7_khi2)

graph_Q7 <- ggplot(data, aes(x = public, y = VADER_COMPOUND, fill = public)) +
  geom_boxplot(show.legend = FALSE, alpha = 0.8) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_fill_manual(values = couleurs_public) +
  labs(title = "Compound moyen selon le public", x = NULL, y = "Compound moyen") +
  theme_rapport()

ggsave(file.path(dossier, "Q7_sentiment_par_public.png"), graph_Q7, width = 6, height = 4.5, dpi = 300)


# ######################################################################
# Q8. LE SENTIMENT ÉVOLUE-T-IL DANS LE TEMPS ?
# ######################################################################

Q8 <- data %>%
  group_by(annee) %>%
  summarise(
    n = n(),
    compound_moyen = mean(VADER_COMPOUND),
    part_negatifs = mean(Sentiment_VADER == "Négatif"),
    .groups = "drop"
  )

print(Q8)

# Comme le TP pour l'âge : test de Kruskal-Wallis
Q8_kruskal <- kruskal.test(VADER_COMPOUND ~ factor(annee), data = data)
print(Q8_kruskal)


# ######################################################################
# Q9. LE SENTIMENT DIFFÈRE-T-IL SELON LE TYPE DE POST ?
# ######################################################################
# Le TP compare deux milieux (Mann-Whitney). Ici, dans les subreddits
# du quotidien, on compare les deux critères d'entrée dans le corpus :
# la personne dit dépendre de l'IA ("dépendance") ou raconte lui avoir
# demandé conseil ("conseil"). Les subreddits de l'IA n'ont que le
# premier critère : ils sont laissés de côté pour ne pas mélanger
# l'effet du critère et celui du public. Les 12 posts retenus par les
# deux critères sont aussi laissés de côté.

data_Q9 <- data %>% filter(public == "Quotidien", critere != "les deux")

Q9 <- data_Q9 %>%
  group_by(critere) %>%
  summarise(
    n = n(),
    compound_moyen = mean(VADER_COMPOUND),
    part_negatifs = mean(Sentiment_VADER == "Négatif"),
    part_positifs = mean(Sentiment_VADER == "Positif"),
    .groups = "drop"
  )

print(Q9)

Q9_mann_whitney <- wilcox.test(VADER_COMPOUND ~ critere, data = data_Q9, exact = FALSE)
print(Q9_mann_whitney)


# ######################################################################
# Q10 à Q13. VADER EST-IL EN ACCORD AVEC UN CODAGE HUMAIN ?
# ######################################################################
# Comme dans le TP : on compare VADER à un codage humain sur un
# échantillon de 60 posts tirés au hasard. Le fichier
# corpus/codage_manuel.xlsx contient deux colonnes remplies à la main :
# Sentiment_humain (Négatif / Neutre / Positif) et Complexite (Q12).
# S'il n'existe pas encore, on le crée vide. Les posts sont repérés par
# leur identifiant Reddit, qui ne change pas si le corpus est reconstruit.

fichier_codage <- "corpus/codage_manuel.xlsx"

if (!file.exists(fichier_codage)) {
  set.seed(2026)
  data %>%
    slice_sample(n = 60) %>%
    transmute(id_reddit, titre, texte, Sentiment_humain = NA_character_, Complexite = NA_character_) %>%
    write_xlsx(fichier_codage)
  cat("\nFichier", fichier_codage, "créé : remplir les colonnes Sentiment_humain et Complexite puis relancer.\n")
}

codage <- read_excel(fichier_codage) %>% select(id_reddit, Sentiment_humain, Complexite)

if (all(!is.na(codage$Sentiment_humain))) {

  validation <- data %>%
    inner_join(codage, by = "id_reddit") %>%
    mutate(Sentiment_humain = factor(Sentiment_humain, levels = levels(Sentiment_VADER)))

  # Q10. Tableau croisé VADER / codage humain
  Q10 <- table(VADER = validation$Sentiment_VADER, Humain = validation$Sentiment_humain)
  print(Q10)

  # Q11. Matrice de confusion
  Q11 <- confusionMatrix(validation$Sentiment_VADER, validation$Sentiment_humain)
  print(Q11)

  # Q12. Quels types de posts posent le plus de problèmes ?
  # Comme le TP, chaque post codé a un type de complexité (colonne
  # Complexite) : Simple, Contraste (passages positifs et négatifs mêlés),
  # Politesse (demande d'aide formulée avec des mots positifs), Récit
  # (situation racontée par les faits, avec peu de mots de sentiment),
  # Négation, Intensification, Ironie, Ambiguïté (ton incertain).
  validation <- validation %>%
    mutate(Correct = if_else(Sentiment_VADER == Sentiment_humain, 1, 0))

  Q12 <- validation %>%
    group_by(Complexite) %>%
    summarise(
      N = n(),
      Taux_correct = round(100 * mean(Correct), 1),
      Taux_erreur = round(100 - 100 * mean(Correct), 1),
      .groups = "drop"
    ) %>%
    arrange(desc(Taux_erreur))

  print(Q12)

  graph_Q12 <- ggplot(Q12, aes(x = reorder(paste0(Complexite, " (", N, ")"), Taux_erreur), y = Taux_erreur)) +
    geom_col(fill = "#2471A3") +
    coord_flip() +
    labs(
      title = "Taux d'erreur de VADER selon le type de post",
      subtitle = "60 posts codés à la main ; entre parenthèses, le nombre de posts",
      x = NULL, y = "Taux d'erreur (%)"
    ) +
    theme_rapport()

  ggsave(file.path(dossier, "Q12_erreurs_par_type.png"), graph_Q12, width = 7, height = 4.5, dpi = 300)

  # Q13. Performance globale : exactitude, kappa, puis précision, rappel
  # et F1 (moyenne des trois classes, comme le TP, et détail par classe)
  Q13 <- tibble(
    indicateur = c("Exactitude", "Kappa", "Précision (macro)", "Rappel (macro)", "F1 (macro)"),
    valeur = round(c(
      Q11$overall["Accuracy"],
      Q11$overall["Kappa"],
      mean(Q11$byClass[, "Pos Pred Value"], na.rm = TRUE),
      mean(Q11$byClass[, "Sensitivity"], na.rm = TRUE),
      mean(Q11$byClass[, "F1"], na.rm = TRUE)
    ), 3)
  )
  print(Q13)

  Q13_par_classe <- as_tibble(Q11$byClass[, c("Pos Pred Value", "Sensitivity", "F1")], rownames = "classe") %>%
    transmute(
      classe = str_remove(classe, "Class: "),
      precision = round(`Pos Pred Value`, 2),
      rappel = round(Sensitivity, 2),
      F1 = round(F1, 2)
    )
  print(Q13_par_classe)

} else {
  Q10 <- Q11 <- NULL
  Q12 <- Q13 <- Q13_par_classe <- tibble()
  cat("Codage humain incomplet : Q10 à Q13 non calculées.\n")
}


# ######################################################################
# Q14. QUELLES SONT LES LIMITES DE LA MÉTHODE ?
# ######################################################################
# Comme le TP : les posts mal classés, à lire.

Q14 <- if (nrow(Q12) > 0) {
  validation %>%
    filter(Correct == 0) %>%
    transmute(id, titre, Complexite, Sentiment_humain, Sentiment_VADER, VADER_COMPOUND,
              extrait = str_trunc(texte, 300))
} else tibble()

print(head(Q14, 15))

# La règle d'agrégation est-elle en cause ? On compare la moyenne des
# phrases (retenue) à deux autres règles : VADER sur le texte entier
# (comme le TP, qui note des commentaires courts), et le solde des
# phrases positives et négatives.
if (nrow(Q12) > 0) {
  texte_entier <- vader_df(validation$texte)$compound

  solde <- phrases %>%
    filter(id %in% validation$id) %>%
    group_by(id) %>%
    summarise(solde = (sum(compound >= 0.05) - sum(compound <= -0.05)) / n(), .groups = "drop")

  classer <- function(x) {
    factor(case_when(x >= 0.05 ~ "Positif", x <= -0.05 ~ "Négatif", TRUE ~ "Neutre"),
           levels = c("Négatif", "Neutre", "Positif"))
  }

  regles <- validation %>%
    mutate(texte_entier = texte_entier) %>%
    left_join(solde, by = "id")

  Q14_regles <- map_dfr(c("VADER_COMPOUND", "texte_entier", "solde"), function(r) {
    cm <- confusionMatrix(classer(regles[[r]]), regles$Sentiment_humain)
    tibble(regle = r, exactitude = round(cm$overall["Accuracy"], 2), kappa = round(cm$overall["Kappa"], 2))
  }) %>%
    mutate(regle = recode(regle, VADER_COMPOUND = "moyenne des phrases (retenue)",
                          texte_entier = "texte entier", solde = "solde des phrases"))
  print(Q14_regles)
} else Q14_regles <- tibble()

# Les conclusions dépendent-elles du lexique ? On refait le calcul avec
# deux autres lexiques du package syuzhet (bing et AFINN), phrase par
# phrase puis en moyenne par post, comme pour VADER. Pour ces deux
# lexiques, un post est négatif si sa moyenne est inférieure à 0.
phrases <- phrases %>%
  mutate(
    bing = get_sentiment(phrase, method = "bing"),
    afinn = get_sentiment(phrase, method = "afinn")
  )

data <- data %>%
  left_join(
    phrases %>% group_by(id) %>% summarise(bing = mean(bing), afinn = mean(afinn), .groups = "drop"),
    by = "id"
  )

classe_lexique <- function(m) {
  if (m == "VADER_COMPOUND") return(data$Sentiment_VADER)
  factor(case_when(data[[m]] > 0 ~ "Positif", data[[m]] < 0 ~ "Négatif", TRUE ~ "Neutre"),
         levels = c("Négatif", "Neutre", "Positif"))
}

moyennes_vader <- data %>% group_by(nom_topic) %>% summarise(v = mean(VADER_COMPOUND), .groups = "drop")

Q14_lexiques <- map_dfr(c("VADER_COMPOUND", "bing", "afinn"), function(m) {
  cl <- classe_lexique(m)
  theme <- data %>% group_by(nom_topic) %>% summarise(x = mean(.data[[m]]), .groups = "drop") %>%
    left_join(moyennes_vader, by = "nom_topic")
  ligne <- tibble(
    lexique = recode(m, VADER_COMPOUND = "VADER", bing = "bing", afinn = "AFINN"),
    part_negatifs_IA = mean(cl[data$public == "IA"] == "Négatif"),
    part_negatifs_quotidien = mean(cl[data$public == "Quotidien"] == "Négatif"),
    p_public = wilcox.test(data[[m]] ~ data$public, exact = FALSE)$p.value,
    p_theme = kruskal.test(data[[m]] ~ factor(data$nom_topic))$p.value,
    rangs_themes_vs_vader = cor(theme$x, theme$v, method = "spearman")
  )
  # Accord avec le codage humain, sur les 60 posts codés
  if (nrow(Q12) > 0) {
    v <- data %>% mutate(classe = cl) %>% inner_join(codage, by = "id_reddit") %>%
      mutate(h = factor(Sentiment_humain, levels = c("Négatif", "Neutre", "Positif")))
    cm <- confusionMatrix(v$classe, v$h)
    ligne <- ligne %>% mutate(
      exactitude = cm$overall["Accuracy"],
      kappa = cm$overall["Kappa"],
      spearman_humain = cor(v[[m]], as.numeric(v$h), method = "spearman")
    )
  }
  ligne
})

print(Q14_lexiques, width = Inf)


# ######################################################################
# COMPLÉMENT. ÉMOTIONS (LEXIQUE NRC, COURS DU 20/10)
# ######################################################################
# Hors des questions du TP. VADER donne une polarité ; le lexique NRC
# distingue huit émotions. Mots d'émotion pour 100 mots, par public.

emotions <- get_nrc_sentiment(data$texte) %>%
  select(anger:trust) %>%
  mutate(across(everything(), ~ 100 * .x / data$n_mots)) %>%
  bind_cols(data %>% select(id, public))

emotions_fr <- c(anger = "colère", anticipation = "anticipation", disgust = "dégoût", fear = "peur",
                 joy = "joie", sadness = "tristesse", surprise = "surprise", trust = "confiance")

emotions_public <- emotions %>%
  pivot_longer(anger:trust, names_to = "emotion", values_to = "score") %>%
  mutate(emotion = emotions_fr[emotion]) %>%
  group_by(public, emotion) %>%
  summarise(score_moyen = mean(score), .groups = "drop")

print(emotions_public %>% pivot_wider(names_from = public, values_from = score_moyen))


# ======================================================================
# 10. EXPORTATION DES RÉSULTATS
# ======================================================================

write_xlsx(
  list(
    posts_scores = data %>% select(id, subreddit, public, annee, nom_topic, VADER_COMPOUND,
                                   VADER_POS, VADER_NEU, VADER_NEG, part_phrases_negatives,
                                   n_tres_positives, n_tres_negatives, ambivalent, Sentiment_VADER),
    Q1_tonalite = Q1,
    Q1_phrases = Q1_phrases,
    Q2_intensite = Q2,
    Q3_polarisation = Q3,
    Q3_indicateur_tp = Q3_tp,
    Q4_themes = Q4,
    Q5_sentiment_theme = Q5,
    Q6_polarisation_theme = Q6,
    Q7_public = Q7,
    Q8_annee = Q8,
    Q9_type_de_post = Q9,
    Q12_erreurs_par_type = Q12,
    Q13_performance = Q13,
    Q13_par_classe = Q13_par_classe,
    Q14_erreurs = Q14,
    Q14_regles = Q14_regles,
    Q14_lexiques = Q14_lexiques,
    complement_emotions_nrc = emotions_public
  ),
  file.path(dossier, "vader_axe2.xlsx")
)

cat("\nRésultats dans", dossier, "\n")
