# =========================================================
#  MIDTERM CSS - AXE 2
#  Style commun des graphiques (couleurs, thème)
# =========================================================
# Chargé par axe2_03_exploration.R, axe2_04_topics.R et axe2_05_vader.R

library(ggplot2)

# Deux publics : subreddits de l'IA (bleu) et du quotidien (orange),
# lisibles aussi par les personnes daltoniennes
couleurs_public <- c("IA" = "#2471A3", "Quotidien" = "#E67E22")

# Sentiments : négatif rouge, neutre gris, positif bleu (plutôt que
# vert, que les daltoniens confondent avec le rouge)
couleurs_sentiment <- c("Négatif" = "#C0392B", "Neutre" = "#AAB7B8", "Positif" = "#2471A3")

theme_rapport <- function(taille = 11) {
  theme_minimal(base_size = taille) +
    theme(
      plot.title = element_text(face = "bold", size = taille + 1),
      plot.title.position = "plot",
      panel.grid.minor = element_blank(),
      legend.position = "bottom",
      strip.text = element_text(face = "bold", hjust = 0)
    )
}
