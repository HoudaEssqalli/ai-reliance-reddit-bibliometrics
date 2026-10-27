# =========================================================
#  MIDTERM CSS - AXE 2
#  Collecte des posts Reddit (archive Arctic Shift)
#  Comment les utilisateurs parlent de leur dépendance à l'IA
#  pour prendre des décisions
# =========================================================
# A lancer depuis le dossier Midterm_CSS :
#   Rscript scripts/axe2_01_reddit_collecte.R
# La collecte reprend là où elle s'est arrêtée si on la relance.


# =========================================================
# 1. Packages
# =========================================================

library(httr2)
library(jsonlite)
library(dplyr)


# =========================================================
# 2. Paramètres de collecte
# =========================================================

api <- "https://arctic-shift.photon-reddit.com/api/posts/search"

mots <- c(
  "rely", "reliance", "dependent", "delegate",
  # ajoutés après le premier tri (trop peu de posts pertinents)
  "addicted", "overreliance", "crutch"
)

# Période : janvier 2023 (après la sortie de ChatGPT) -> date de collecte.
# r/ChatGPT est trop volumineux pour que l'archive cherche sur un trimestre
# entier : on y prend les deux premières semaines de chaque trimestre.
# Les autres subreddits sont interrogés par semestre.
trimestres <- seq(as.Date("2023-01-01"), as.Date("2026-10-01"), by = "quarter")
semestres <- data.frame(
  debut = seq(as.Date("2023-01-01"), as.Date("2026-07-01"), by = "6 months")
)
semestres$fin <- c(semestres$debut[-1], as.Date("2026-10-22"))

fenetres <- bind_rows(
  data.frame(sub = "ChatGPT", debut = trimestres, fin = trimestres + 14),
  merge(
    data.frame(sub = c(
      "OpenAI", "ClaudeAI", "ArtificialInteligence", "decidingtobebetter",
      # subreddits où l'on parle de ses décisions au quotidien
      "selfimprovement", "productivity", "college", "Advice"
    )),
    semestres
  )
)

subs_quotidien <- c("decidingtobebetter", "selfimprovement", "productivity", "college", "Advice")

# Ordre des requêtes, du plus utile au moins utile :
# 1. subreddits du quotidien, posts qui citent ChatGPT ou l'IA : le filtre
#    de pertinence garde ceux où l'on s'en remet à elle (ce sont eux qui
#    couvrent le gap des décisions de tous les jours) ;
# 2. subreddits de l'IA, mots de la dépendance.
# Les mots de la dépendance dans les subreddits du quotidien ont été
# essayés puis abandonnés : sur 1 638 posts collectés ainsi, 15 seulement
# passent le filtre de pertinence (0,9 %), contre 3,1 % pour "chatgpt"
# et "AI" dans ces subreddits et 11,5 % pour les mots de la dépendance
# dans les subreddits de l'IA. On y dépend surtout de ses parents ou de
# son couple. Les posts déjà collectés ainsi restent dans le corpus brut.
requetes <- bind_rows(
  merge(data.frame(mot = c("chatgpt", "AI")), filter(fenetres, sub %in% subs_quotidien)),
  merge(data.frame(mot = mots), filter(fenetres, !sub %in% subs_quotidien))
)

champs <- "id,subreddit,title,selftext,created_utc,score,num_comments,over_18,author"

fichier_brut <- "corpus/reddit_brut.rds"


# =========================================================
# 3. Fonction d'appel à l'API (avec nouvelles tentatives)
# =========================================================

appeler <- function(sub, mot, apres, avant, tri = NULL, delai = 45) {
  # L'archive renvoie souvent "Timeout. Maybe slow down a bit" :
  # on attend de plus en plus longtemps entre les essais
  for (essai in 1:2) {
    rep <- tryCatch(
      request(api) |>
        req_url_query(
          subreddit = sub, query = mot,
          after = apres, before = avant, sort = tri,
          limit = 100, fields = champs
        ) |>
        req_timeout(delai) |>
        req_error(is_error = function(r) FALSE) |>
        req_perform(),
      error = function(e) NULL
    )
    if (!is.null(rep) && resp_status(rep) == 200) {
      contenu <- fromJSON(resp_body_string(rep))
      if (!is.null(contenu$data)) return(as.data.frame(contenu$data))
    }
    Sys.sleep(10 * essai)
  }
  NULL
}

# L'archive renvoie au plus 100 posts par requête. Pour ChatGPT et l'IA
# dans les subreddits du quotidien, on demande la page suivante quand
# ce plafond est atteint (jusqu'à 3 pages, soit 300 posts par semestre).
# La page suivante commence après le dernier post de la page précédente,
# dans l'ordre où l'archive les a renvoyés.
bornes_page_suivante <- function(prec, r) {
  t <- prec$created_utc
  if (t[1] <= t[length(t)]) {
    list(apres = format(max(t), scientific = FALSE), avant = format(r$fin), tri = "asc")
  } else {
    list(apres = format(r$debut), avant = format(min(t), scientific = FALSE), tri = "desc")
  }
}


# =========================================================
# 4. Boucle de collecte
# =========================================================
# Plusieurs passes : chaque passe refait les requêtes qui ont échoué,
# avec un délai d'attente plus long.

resultats <- if (file.exists(fichier_brut)) readRDS(fichier_brut) else list()

enregistrer <- function(cle, posts, mot) {
  if (nrow(posts) > 0) posts$requete <- mot
  resultats[[cle]] <<- posts
  message("OK     ", cle, " : ", nrow(posts), " posts")
  saveRDS(resultats, fichier_brut)
  Sys.sleep(3)
}

for (passe in 1:4) {
  delai <- 45 + 30 * (passe - 1)
  message("--- Passe ", passe, " (délai ", delai, " s)")
  echecs <- 0

  for (i in seq_len(nrow(requetes))) {
    r <- requetes[i, ]
    cle <- paste(r$sub, r$mot, r$debut, sep = "|")

    # Page 1
    if (is.null(resultats[[cle]])) {
      posts <- appeler(r$sub, r$mot, format(r$debut), format(r$fin), delai = delai)
      if (is.null(posts)) {
        message("ECHEC  ", cle)
        echecs <- echecs + 1
        next
      }
      enregistrer(cle, posts, r$mot)
    }

    # Pages 2 et 3 (ChatGPT et IA dans les subreddits du quotidien)
    if (!(r$sub %in% subs_quotidien && r$mot %in% c("chatgpt", "AI"))) next
    prec <- resultats[[cle]]
    for (page in 2:3) {
      if (nrow(prec) < 100) break
      cle_page <- paste0(cle, "|p", page)
      if (is.null(resultats[[cle_page]])) {
        b <- bornes_page_suivante(prec, r)
        suite <- appeler(r$sub, r$mot, b$apres, b$avant, tri = b$tri, delai = delai)
        if (is.null(suite)) {
          message("ECHEC  ", cle_page)
          echecs <- echecs + 1
          break
        }
        enregistrer(cle_page, suite, r$mot)
      }
      prec <- resultats[[cle_page]]
    }
  }

  if (echecs == 0) break
  Sys.sleep(60)
}


# =========================================================
# 5. Bilan
# =========================================================

brut <- bind_rows(resultats) %>% distinct(id, .keep_all = TRUE)

cat("\nRequêtes réussies :", length(resultats), "\n")
cat("Posts uniques collectés :", nrow(brut), "\n")
