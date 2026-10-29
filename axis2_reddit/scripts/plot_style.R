# =========================================================
#  AXIS 2 - REDDIT
#  Shared plot style (colours, theme)
# =========================================================
# Loaded by 03_exploration.R, 04_topic_models.R and 05_vader_sentiment.R

library(ggplot2)

# Two audiences: AI subreddits (blue) and everyday-life subreddits
# (orange), also readable by colour-blind readers
audience_colours <- c("AI" = "#2471A3", "Everyday" = "#E67E22")

# Sentiment: negative red, neutral grey, positive blue (rather than
# green, which colour-blind readers confuse with red)
sentiment_colours <- c("Negative" = "#C0392B", "Neutral" = "#AAB7B8", "Positive" = "#2471A3")

theme_report <- function(size = 11) {
  theme_minimal(base_size = size) +
    theme(
      plot.title = element_text(face = "bold", size = size + 1),
      plot.title.position = "plot",
      panel.grid.minor = element_blank(),
      legend.position = "bottom",
      strip.text = element_text(face = "bold", hjust = 0)
    )
}
