# =========================================================
#  MIDTERM CSS - AXE 1
#  Construction et test de l'équation booléenne (OpenAlex)
#  Méthode du cours : concepts -> synonymes -> blocs OR -> AND
# =========================================================
# Ouvrir "Axe 1.Rproj", puis lancer ce script
# (résultats dans le dossier equation/)
# Chaque terme est testé seul (nombre de résultats + 3 premiers
# titres), puis chaque bloc, puis l'équation complète.


# =========================================================
# 1. Packages
# =========================================================

library(httr2)
library(jsonlite)
library(dplyr)
library(readr)


# =========================================================
# 2. Concepts et termes candidats (proposés par un LLM)
# =========================================================

concepts <- list(
  IA = c(
    "artificial intelligence", "AI", "machine learning", "algorithm",
    "algorithmic", "large language model", "LLM", "ChatGPT",
    "generative AI", "chatbot", "decision support system",
    "automated decision", "recommender system", "robo-advisor"
  ),
  DEPENDANCE = c(
    "reliance", "overreliance", "over-reliance", "appropriate reliance",
    "delegation", "delegate", "dependence", "dependency",
    "algorithm aversion", "algorithm appreciation", "automation bias",
    "advice taking", "trust calibration", "complacency", "deference",
    "cognitive offloading"
  ),
  DECISION = c(
    "decision making", "decision-making", "decision", "judgment",
    "judgement", "choice", "advice", "human-AI collaboration"
  )
)

# Filtres communs à tous les tests (mêmes que pour l'export)
filtres_communs <- paste0(
  ",from_publication_date:2015-01-01",
  ",to_publication_date:2026-10-22",
  ",language:en",
  ",type:article|review"
)

bloc <- function(termes) {
  paste0("title_and_abstract.search:(", paste0('"', termes, '"', collapse = " OR "), ")")
}


# =========================================================
# 3. Fonction de test
# =========================================================

tester <- function(filtre) {
  for (essai in 1:5) {
    rep <- request("https://api.openalex.org/works") |>
      req_url_query(
        filter = paste0(filtre, filtres_communs),
        `per-page` = 3, select = "display_name"
      ) |>
      req_error(is_error = function(r) FALSE) |>
      req_perform()
    if (resp_status(rep) == 200) break
    Sys.sleep(5 * essai)
  }
  res <- fromJSON(resp_body_string(rep))
  Sys.sleep(1)
  tibble(
    resultats = res$meta$count,
    premiers_titres = paste(substr(res$results$display_name, 1, 90), collapse = " | ")
  )
}


# =========================================================
# 4. Test de chaque terme seul
# =========================================================

if (!file.exists("equation/tests_termes.csv")) {
  tests_termes <- bind_rows(lapply(names(concepts), function(concept) {
    bind_rows(lapply(concepts[[concept]], function(terme) {
      message(concept, " : ", terme)
      bind_cols(tibble(concept = concept, terme = terme), tester(bloc(terme)))
    }))
  }))
  write_csv(tests_termes, "equation/tests_termes.csv")
}


# =========================================================
# 5. Termes retenus (version 1 des blocs)
# =========================================================
# Retirés car ils ramènent surtout autre chose (voir tests_termes.csv) :
#  - dependence, dependency : pharmacologie, physique, linguistique
#  - deference : philosophie ; complacency : écologie
#  - choice : choix scolaire, économie ; recommender system : e-commerce
#  - algorithmic : identique à "algorithm" (recherche racinisée)
#  - decision-making : identique à "decision making"
#  - human-AI collaboration : relève de la collaboration, pas de la décision

blocs_v1 <- list(
  IA = c(
    "artificial intelligence", "AI", "machine learning", "algorithm",
    "large language model", "LLM", "ChatGPT", "generative AI", "chatbot",
    "decision support system", "automated decision", "robo-advisor"
  ),
  DEPENDANCE = c(
    "reliance", "overreliance", "over-reliance", "delegation", "delegate",
    "algorithm aversion", "algorithm appreciation", "automation bias",
    "advice taking", "trust calibration", "cognitive offloading"
  ),
  DECISION = c("decision", "judgment", "judgement", "advice")
)

# Version 1 (22/10/2026) : 7 393 résultats, mais sur un échantillon
# aléatoire de 12 titres, seuls 3 ou 4 portent sur le sujet. Les mots
# isolés "reliance" et "delegate" apparaissent dans des résumés sans
# rapport (réseaux électriques, cybersécurité...) : bloc trop large.
#
# Version 2 : le bloc DEPENDANCE ne garde que des expressions précises.

blocs <- blocs_v1
blocs$DEPENDANCE <- c(
  "overreliance", "over-reliance", "appropriate reliance",
  "reliance on AI", "reliance on artificial intelligence",
  "reliance on algorithms", "AI reliance",
  "algorithm aversion", "algorithm appreciation", "automation bias",
  "advice taking", "trust calibration", "cognitive offloading",
  "delegation to AI", "delegating to AI", "delegate to AI",
  "decision delegation", "delegation of decisions", "delegating decisions"
)


# =========================================================
# 6. Rendement des blocs combinés
# =========================================================

tests_blocs <- bind_rows(
  bind_cols(tibble(equation = "IA AND DEPENDANCE"),
            tester(paste(bloc(blocs$IA), bloc(blocs$DEPENDANCE), sep = ","))),
  bind_cols(tibble(equation = "IA AND DEPENDANCE AND DECISION"),
            tester(paste(bloc(blocs$IA), bloc(blocs$DEPENDANCE), bloc(blocs$DECISION), sep = ",")))
)
print(tests_blocs, width = 200)


# =========================================================
# 7. Contribution de chaque terme dans l'équation complète
# =========================================================
# Pour chaque terme : résultats du terme ET des deux autres blocs.
# Un terme qui ramène beaucoup de titres hors sujet se retire.

tests_contexte <- bind_rows(lapply(names(blocs), function(concept) {
  autres <- setdiff(names(blocs), concept)
  bind_rows(lapply(blocs[[concept]], function(terme) {
    message("Contexte ", concept, " : ", terme)
    filtre <- paste(c(bloc(terme), sapply(blocs[autres], bloc)), collapse = ",")
    bind_cols(tibble(concept = concept, terme = terme), tester(filtre))
  }))
}))

write_csv(tests_blocs, "equation/tests_blocs.csv")
write_csv(tests_contexte, "equation/tests_contexte.csv")
