# =========================================================
#  MIDTERM CSS - AXE 2
#  Listes de mots retirés, communes aux scripts d'analyse
# =========================================================
# Chargé par axe2_03_exploration.R et axe2_04_topics.R (source())


# Mots supplémentaires peu informatifs
extra_stopwords <- c(
  # Restes de contractions et de mise en forme Reddit
  "im", "ive", "dont", "doesnt", "didnt", "cant", "wont", "isnt",
  "thats", "youre", "theyre", "its", "id", "ill", "amp", "xb",
  # Mots génériques du discours en ligne
  "just", "like", "really", "lot", "thing", "things", "stuff",
  "pretty", "actually", "basically", "literally", "kinda", "gonna",
  "yeah", "okay", "ok", "etc", "edit", "post", "reddit", "anyone",
  "someone", "something", "everything", "anything", "way", "also"
)

# Mots de la requête : ils figurent dans presque tous les posts
# puisqu'ils ont servi à les sélectionner. On les compte à part puis
# on les retire, sinon ils écrasent le reste.
mots_requete <- c(
  "ai", "chatgpt", "gpt", "llm", "llms",
  # noms des IA et de leurs entreprises : ils reflètent surtout le
  # choix des subreddits (r/ClaudeAI, r/OpenAI)
  "claude", "gemini", "copilot", "openai", "anthropic",
  # formes possessives et plurielles ("ChatGPT's" -> "chatgpts")
  "chatgpts", "ais", "claudes", "gpts",
  "rely", "relying", "relied", "relies", "reliance", "reliant",
  "depend", "depending", "dependent", "dependence", "dependency",
  "delegate", "delegating", "delegated", "delegation",
  "addicted", "addiction", "crutch", "overreliance",
  # lemme de "addicted" après la lemmatisation
  "addict"
)
