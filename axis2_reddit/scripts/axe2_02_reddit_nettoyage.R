# =========================================================
#  MIDTERM CSS - AXE 2
#  Nettoyage et sélection du corpus Reddit
# =========================================================
# A lancer depuis le dossier Midterm_CSS, après axe2_01_reddit_collecte.R :
#   Rscript scripts/axe2_02_reddit_nettoyage.R


# =========================================================
# 1. Packages
# =========================================================

library(dplyr)
library(stringr)
library(tidytext)
library(readr)
library(writexl)


# =========================================================
# 2. Chargement des posts bruts
# =========================================================

brut <- bind_rows(readRDS("corpus/reddit_brut.rds"))

etapes <- tibble(etape = "Posts collectés (avec doublons)", n = nrow(brut))
# Compte les posts restants après chaque filtre. Le nombre est calculé
# avant de mettre à jour etapes, sinon le pipe (évalué paresseusement)
# écrase les étapes intermédiaires.
garder <- function(data, libelle) {
  n <- nrow(data)
  etapes <<- bind_rows(etapes, tibble(etape = libelle, n = n))
  data
}

posts <- brut %>%
  distinct(id, .keep_all = TRUE) %>%
  garder("Posts uniques")


# =========================================================
# 3. Filtres de qualité
# =========================================================

nettoyer_texte <- function(x) {
  x %>%
    str_replace_all("\\[([^\\]]*)\\]\\([^)]*\\)", "\\1") %>%  # liens markdown
    str_replace_all("https?://\\S+", " ") %>%                 # URL
    str_replace_all("&amp;", "&") %>%
    str_replace_all("&gt;|&lt;|&nbsp;|#x200B;", " ") %>%
    str_replace_all("[*_~>#`]+", " ") %>%                     # mise en forme
    str_squish()
}

posts <- posts %>%
  filter(!selftext %in% c("[removed]", "[deleted]", ""), !is.na(selftext)) %>%
  garder("Corps du post disponible (ni supprimé ni vide)") %>%
  filter(!coalesce(over_18, FALSE), coalesce(author, "") != "AutoModerator") %>%
  garder("Hors contenu adulte et messages automatiques") %>%
  mutate(
    texte = nettoyer_texte(paste0(title, ". ", selftext)),
    n_mots = str_count(texte, "\\S+")
  ) %>%
  filter(n_mots >= 30, n_mots <= 1500) %>%
  garder("Entre 30 et 1 500 mots") %>%
  distinct(texte, .keep_all = TRUE) %>%
  garder("Textes identiques supprimés (cross-posts)")


# Anglais : part des mots outils anglais dans le texte
mots_outils <- unique(stop_words$word)

part_anglais <- function(x) {
  mots <- str_split(str_to_lower(x), "[^a-z']+")[[1]]
  mots <- mots[mots != ""]
  if (length(mots) == 0) return(0)
  mean(mots %in% mots_outils)
}

posts <- posts %>%
  mutate(part_en = vapply(texte, part_anglais, numeric(1))) %>%
  filter(part_en >= 0.35) %>%
  garder("Texte en anglais")


# =========================================================
# 4. Filtre de pertinence
# =========================================================
# Version 1 (IA + dépendance + décision n'importe où dans le texte) :
# sur 10 posts tirés au hasard, 3 seulement portaient sur le sujet ;
# les autres (annonces de produits, plaintes sur les quotas) contenaient
# les mots par hasard. Version 2 : la dépendance doit porter sur l'IA
# elle-même, dans la même expression, et le post doit contenir un mot
# de décision ou de conseil. Sur la collecte élargie, la version 2 ne
# garde que 123 posts sur 3 565 : exiger le mot "decision" écarte des
# posts au coeur du sujet ("Over reliance on AI is making me slow",
# "people rely on AI more and more" à propos d'une réponse fausse).
#
# Version 3 : on garde un post quand une personne dit dépendre de l'IA,
# ou quand le post parle de la dépendance à l'IA :
#  - sujet humain + verbe de dépendance + IA : "I rely too much on
#    ChatGPT", "students are dependent on AI", "I let ChatGPT decide"
#  - nom de dépendance + IA : "over-reliance on AI", "my dependence on
#    ChatGPT", "AI dependence", "ChatGPT addiction", "AI as a crutch"
# Le sujet humain écarte les phrases où c'est un programme qui dépend
# d'un modèle ("dataset quality is dependent on the LLM").
# Le mot de décision n'est plus exigé, mais il est noté
# (colonne mentionne_decision).

ia <- "(chat ?gpt|gpt-?[345o]?|claude|gemini|copilot|llms?|chatbots?|a\\.?i\\.?|artificial intelligence)"
sujet <- "(i|i'm|im|i've|ive|we|we're|you|they|people|users?|students?|kids|humans?|everyone|myself|me|many|most|folks|he|she|someone|nobody)"
verbe_dependance <- "(rel(y|ies|ied|ying)|depend(s|ed|ing|ent)?|delegat\\w*|outsourc\\w*|addicted|hooked)"
nom_dependance <- "(over-?relian\\w*|relian(ce|t)|dependen(ce|cy)|addiction)"

re_dependance_ia <- regex(paste0(
  "\\b", sujet, "\\W+(\\w+\\W+){0,3}", verbe_dependance, "\\W+(\\w+\\W+){0,3}", ia, "\\b",
  "|\\b", nom_dependance, "\\W+(\\w+\\W+){0,3}", ia, "\\b",
  "|\\b", ia, "\\W+(dependen\\w*|relian\\w*|addiction|over-?relian\\w*)\\b",
  "|\\b", ia, "\\W+(is|as)\\W+(my|a)\\W+crutch\\b",
  "|\\b(let|letting|have|having|ask|asking|asked)\\W+(\\w+\\W+)?", ia,
  "\\W+(\\w+\\W+){0,2}(decide|choose|pick|make\\W+(\\w+\\W+)?decisions?|tell me what to do)\\b"
), ignore_case = TRUE)

re_decision <- regex(
  "\\b(decision\\w*|decid\\w*|choice\\w*|choos\\w*|chose\\w*|advice|advis\\w*|judg\\w*)\\b",
  ignore_case = TRUE
)

# Dans les subreddits du quotidien, on écrit rarement "I rely on
# ChatGPT" : on raconte plutôt qu'on lui a demandé conseil ("ChatGPT
# told me it's a great choice", "asking AI for life advice"). C'est la
# prise de conseil (advice taking) étudiée par la littérature de
# l'axe 1. On garde ces posts quand ils parlent aussi de décision ou
# de conseil. Pas dans les subreddits de l'IA, où "I asked Claude"
# introduit surtout des questions techniques.
re_conseil_ia <- regex(paste0(
  "\\b(asked|ask|asking|consulted|consult|consulting|turned to|turn to|talked to|talking to|talk to|ran it by|vent(ed|ing)? to)\\W+(\\w+\\W+){0,2}", ia, "\\b",
  "|\\b", ia, "\\W+(\\w+\\W+){0,1}(told|tells|said|says|suggested|suggests|recommended|recommends|advised|advises)\\b",
  "|\\b(use|used|using|uses)\\W+(\\w+\\W+){0,1}", ia, "\\W+(as|for)\\W+(\\w+\\W+){0,1}(advice|advisor|decisions?|therap\\w*|counsel\\w*|guidance|life coach)\\b"
), ignore_case = TRUE)

subs_quotidien <- c("decidingtobebetter", "selfimprovement", "productivity", "college", "advice")

# Posts techniques sur les agents de code, où c'est un programme qui
# "délègue" à un autre modèle : hors sujet
re_technique <- regex("\\b(mcp|sub-?agents?|orchestrat\\w*)\\b", ignore_case = TRUE)

posts <- posts %>%
  mutate(
    dependance_ia = str_detect(texte, re_dependance_ia),
    conseil_ia = tolower(subreddit) %in% subs_quotidien &
      str_detect(texte, re_conseil_ia) & str_detect(texte, re_decision)
  ) %>%
  filter(dependance_ia | conseil_ia) %>%
  garder("Pertinent : dépendance à l'IA, ou conseil demandé à l'IA (quotidien)") %>%
  filter(!str_detect(texte, re_technique)) %>%
  garder("Hors posts techniques (agents de code)") %>%
  mutate(mentionne_decision = str_detect(texte, re_decision))

# Résumés d'articles scientifiques publiés en série par un même compte
# ("I'm finding and summarising interesting AI research papers every
# day") : ce n'est pas la parole d'un utilisateur.
posts <- posts %>%
  filter(!str_detect(texte, regex("summari[sz]ing interesting AI research papers", ignore_case = TRUE))) %>%
  garder("Hors résumés automatiques d'articles")

# Quasi-doublons : le même message republié dans un autre subreddit
# avec quelques mots changés. Deux posts sont des quasi-doublons quand
# la similarité de Jaccard de leurs vocabulaires dépasse 0,6 (au-delà,
# toutes les paires lues sont des reprises du même texte). On garde le
# plus ancien.
posts <- posts %>% arrange(created_utc)
vocab_posts <- posts %>%
  mutate(rang = row_number()) %>%
  select(rang, texte) %>%
  unnest_tokens(word, texte) %>%
  distinct()
mots_index <- factor(vocab_posts$word)
presence <- Matrix::sparseMatrix(
  i = vocab_posts$rang, j = as.integer(mots_index), x = 1,
  dims = c(nrow(posts), nlevels(mots_index))
)
communs <- as.matrix(Matrix::tcrossprod(presence))
tailles <- diag(communs)
jaccard <- communs / (outer(tailles, tailles, "+") - communs)
jaccard[lower.tri(jaccard, diag = TRUE)] <- 0
reprise <- apply(jaccard >= 0.6, 2, any)   # un post plus ancien lui ressemble

posts <- posts[!reprise, ] %>%
  garder("Quasi-doublons retirés (reprises du même message)")


# =========================================================
# 5. Corpus final (anonymisé : pas de nom d'utilisateur)
# =========================================================

corpus <- posts %>%
  mutate(
    date = as.Date(as.POSIXct(created_utc, origin = "1970-01-01", tz = "UTC")),
    annee = as.integer(format(date, "%Y")),
    semestre = paste0(annee, "-S", ifelse(as.integer(format(date, "%m")) <= 6, 1, 2)),
    # Public : subreddits consacrés à l'IA ou subreddits de la vie quotidienne
    public = ifelse(tolower(subreddit) %in% subs_quotidien, "Quotidien", "IA"),
    critere = case_when(
      dependance_ia & conseil_ia ~ "les deux",
      dependance_ia ~ "dépendance",
      TRUE ~ "conseil"
    )
  ) %>%
  arrange(date) %>%
  # id_reddit d'abord : dans transmute, "id" est ensuite remplacé par
  # l'identifiant anonyme du corpus (R0001, R0002...)
  transmute(
    id_reddit = id,
    id = sprintf("R%04d", row_number()),
    subreddit,
    public,
    date,
    annee,
    semestre,
    requete,
    score,
    num_comments,
    titre = title,
    texte,
    n_mots,
    critere,
    mentionne_decision
  )

write_xlsx(corpus, "corpus/corpus_reddit.xlsx")
write_csv(corpus, "corpus/corpus_reddit.csv")
write_csv(etapes, "corpus/reddit_etapes_selection.csv")

print(etapes, n = Inf)
cat("\nPar année :\n"); print(table(corpus$annee))
cat("\nPar subreddit :\n"); print(table(corpus$subreddit))
cat("\nPar public :\n"); print(table(corpus$public))
cat("\nCritère de sélection, par public :\n"); print(table(corpus$public, corpus$critere))
cat("\nMentionne une décision ou un conseil :\n"); print(table(corpus$mentionne_decision))
