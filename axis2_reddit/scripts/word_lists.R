# =========================================================
#  AXIS 2 - REDDIT
#  Word lists removed before the analyses
# =========================================================
# Loaded by 03_exploration.R and 04_topic_models.R (source())


# Extra words that carry little information
extra_stopwords <- c(
  # Leftovers of contractions and Reddit formatting
  "im", "ive", "dont", "doesnt", "didnt", "cant", "wont", "isnt",
  "thats", "youre", "theyre", "its", "id", "ill", "amp", "xb",
  # Generic words of online talk
  "just", "like", "really", "lot", "thing", "things", "stuff",
  "pretty", "actually", "basically", "literally", "kinda", "gonna",
  "yeah", "okay", "ok", "etc", "edit", "post", "reddit", "anyone",
  "someone", "something", "everything", "anything", "way", "also"
)

# Search words: they appear in almost every post, since they were used
# to select the posts. They are counted separately and then removed,
# otherwise they crowd out everything else.
query_words <- c(
  "ai", "chatgpt", "gpt", "llm", "llms",
  # names of the AI tools and their companies: they mostly reflect the
  # choice of subreddits (r/ClaudeAI, r/OpenAI)
  "claude", "gemini", "copilot", "openai", "anthropic",
  # possessive and plural forms ("ChatGPT's" -> "chatgpts")
  "chatgpts", "ais", "claudes", "gpts",
  "rely", "relying", "relied", "relies", "reliance", "reliant",
  "depend", "depending", "dependent", "dependence", "dependency",
  "delegate", "delegating", "delegated", "delegation",
  "addicted", "addiction", "crutch", "overreliance",
  # lemma of "addicted" after lemmatisation
  "addict"
)
