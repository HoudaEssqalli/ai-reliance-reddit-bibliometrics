# =========================================================
#  MIDTERM CSS - AXE 1
#  Analyse bibliométrique : thématiques émergentes et gaps
# =========================================================
# Mêmes analyses que dans biblioshiny, lancées par le code pour
# garder les chiffres exacts. Ouvrir "Axe 1.Rproj" et lancer
# 4_fichier_biblioshiny.R avant ce script.
# Résultats : dossier resultats/ (figures PNG + resultats_axe1.xlsx)


# =========================================================
# 1. Packages et données
# =========================================================

library(bibliometrix)
library(dplyr)
library(stringr)
library(ggplot2)
library(writexl)

load("corpus_biblioshiny.RData")
dir.create("resultats", showWarnings = FALSE)


# =========================================================
# 2. Harmoniser les mots-clés (étape 4 des slides du cours)
# =========================================================
# Tirets longs -> tirets simples, puis synonymes regroupés sous un
# seul terme (le premier de chaque ligne). "ARTIFICIAL INTELLIGENCE"
# et "AI" sont retirés des cartes : tous les documents en parlent,
# ils écrasent les autres thèmes.

M$DE <- str_replace_all(M$DE, "[–—]", "-")

synonymes <- c(
  "ARTIFICIAL INTELLIGENCE;AI;ARTIFICIAL INTELLIGENCE (AI)",
  "GENERATIVE AI;GENERATIVE ARTIFICIAL INTELLIGENCE;GENAI;GENERATIVE AI (GENAI);GENERATIVE ARTIFICIAL INTELLIGENCE (GENAI)",
  "LARGE LANGUAGE MODELS;LARGE LANGUAGE MODEL;LLM;LLMS;LARGE LANGUAGE MODELS (LLMS)",
  "EXPLAINABLE AI;EXPLAINABLE ARTIFICIAL INTELLIGENCE;XAI;EXPLAINABLE ARTIFICIAL INTELLIGENCE (XAI);EXPLAINABLE AI (XAI);EXPLAINABILITY",
  "DECISION-MAKING;DECISION MAKING",
  "HUMAN-AI DECISION-MAKING;HUMAN-AI DECISION MAKING;AI-ASSISTED DECISION-MAKING;AI-ASSISTED DECISION MAKING",
  "CLINICAL DECISION SUPPORT;CLINICAL DECISION SUPPORT SYSTEMS;CLINICAL DECISION SUPPORT SYSTEM;CLINICAL DECISION SUPPORT SYSTEMS (CDSS)",
  "DECISION SUPPORT SYSTEMS;DECISION SUPPORT SYSTEM;DECISION SUPPORT",
  "ALGORITHM AVERSION;ALGORITHMIC AVERSION",
  "OVERRELIANCE;OVER-RELIANCE",
  "ADVICE TAKING;ADVICE-TAKING",
  "CHATGPT;CHAT GPT",
  "TRUST;TRUST IN AI;TRUST IN ARTIFICIAL INTELLIGENCE"
)

a_retirer <- c("ARTIFICIAL INTELLIGENCE", "AI")


# =========================================================
# 3. Description du corpus
# =========================================================

res <- biblioAnalysis(M, sep = ";")
resume <- summary(res, k = 15, pause = FALSE, verbose = FALSE)

# AU_CO est déjà calculé dans 4_fichier_biblioshiny.R (adresses pour
# Scopus et WoS, export dédié pour OpenAlex) : on ne le recalcule pas.
M_pays <- M
pays <- tibble(pays = unlist(str_split(na.omit(M_pays$AU_CO), ";"))) %>%
  filter(pays != "") %>%
  distinct() %>%
  count(pays, name = "documents")
# distinct() ci-dessus porterait sur tout le corpus : on compte donc
# les documents par pays ligne par ligne
pays <- tibble(
  doc = rep(seq_len(nrow(M_pays)), lengths(str_split(M_pays$AU_CO, ";"))),
  pays = unlist(str_split(M_pays$AU_CO, ";"))
) %>%
  filter(!is.na(pays), pays != "NA", pays != "") %>%
  distinct(doc, pays) %>%
  count(pays, name = "documents", sort = TRUE)

docs_avec_pays <- sum(!is.na(M_pays$AU_CO) & M_pays$AU_CO != "")


# =========================================================
# 4. Mots-clés les plus fréquents (harmonisés)
# =========================================================

mots <- tibble(mot = unlist(str_split(na.omit(M$DE), ";"))) %>%
  mutate(mot = str_squish(mot)) %>%
  filter(mot != "")

table_syn <- tibble(ligne = synonymes) %>%
  mutate(canon = str_extract(ligne, "^[^;]+"), variante = str_split(ligne, ";")) %>%
  tidyr::unnest(variante)

mots_cles <- mots %>%
  left_join(table_syn %>% select(variante, canon), by = c("mot" = "variante")) %>%
  mutate(mot = coalesce(canon, mot)) %>%
  count(mot, name = "documents", sort = TRUE)


# =========================================================
# 5. Carte thématique (tout le corpus)
# =========================================================

# Test de robustesse (minfreq 5 et 10, regroupement walktrap et
# louvain) : les mêmes quatre familles de thèmes reviennent à chaque
# fois, mais leur quadrant change d'un réglage à l'autre. On garde donc
# les familles, pas les quadrants ; l'émergence est mesurée en 7 bis.

carte <- thematicMap(
  M, field = "DE", n = 250, minfreq = 5, ngrams = 1,
  stemming = FALSE, size = 0.5, n.labels = 3, repel = TRUE,
  remove.terms = a_retirer, synonyms = synonymes, cluster = "walktrap"
)

ggsave("resultats/carte_thematique.png", carte$map, width = 10, height = 8, dpi = 200)

clusters <- carte$clusters %>%
  select(cluster = name, occurrences = freq, centralite = rcentrality,
         densite = rdensity, mots = words)


# =========================================================
# 6. Evolution thématique : avant ChatGPT, 2023-2024, 2025-2026
# =========================================================

evolution <- thematicEvolution(
  M, field = "DE", years = c(2022, 2024), n = 250, minFreq = 5,
  ngrams = 1, stemming = FALSE, size = 0.5, n.labels = 3, repel = TRUE,
  remove.terms = a_retirer, synonyms = synonymes, cluster = "walktrap"
)

periodes <- c("2015-2022", "2023-2024", "2025-2027")
for (i in seq_along(evolution$TM)) {
  ggsave(sprintf("resultats/carte_%s.png", periodes[i]), evolution$TM[[i]]$map,
         width = 10, height = 8, dpi = 200)
}

clusters_periodes <- bind_rows(lapply(seq_along(evolution$TM), function(i) {
  evolution$TM[[i]]$clusters %>%
    transmute(periode = periodes[i], cluster = name, occurrences = freq,
              centralite = rcentrality, densite = rdensity, mots = words)
}))


# =========================================================
# 7. Thèmes en tendance (année médiane d'apparition)
# =========================================================

tendances <- fieldByYear(
  M, field = "DE", timespan = c(2015, 2026), min.freq = 10, n.items = 4,
  remove.terms = a_retirer, synonyms = synonymes, graph = TRUE
)

ggsave("resultats/themes_tendance.png", tendances$graph, width = 9, height = 10, dpi = 200)


# =========================================================
# 7 bis. Part des documents par mot-clé et par période
# =========================================================
# Mesure de l'émergence qui ne dépend pas du regroupement : parmi les
# documents qui ont des mots-clés d'auteurs, part de ceux qui portent
# chaque mot-clé (harmonisé), par période.

mots_docs <- tibble(
  doc = rep(seq_len(nrow(M)), lengths(str_split(coalesce(M$DE, ""), ";"))),
  mot = str_squish(unlist(str_split(coalesce(M$DE, ""), ";")))
) %>%
  filter(mot != "") %>%
  left_join(table_syn %>% select(variante, canon), by = c("mot" = "variante")) %>%
  mutate(mot = coalesce(canon, mot)) %>%
  distinct(doc, mot) %>%
  mutate(periode = cut(M$PY[doc], c(-Inf, 2022, 2024, Inf), labels = periodes))

docs_par_periode <- mots_docs %>% distinct(doc, periode) %>% count(periode, name = "docs")

mots_suivis <- c(
  "ALGORITHM AVERSION", "ADVICE TAKING", "ALGORITHM APPRECIATION", "DECISION SUPPORT SYSTEMS",
  "AUTOMATION BIAS", "TRUST", "EXPLAINABLE AI", "TRUST CALIBRATION", "APPROPRIATE RELIANCE",
  "HUMAN-AI COLLABORATION", "GENERATIVE AI", "LARGE LANGUAGE MODELS", "CHATGPT",
  "COGNITIVE OFFLOADING", "CRITICAL THINKING", "AI LITERACY", "HIGHER EDUCATION"
)

parts_periodes <- mots_docs %>%
  filter(mot %in% mots_suivis) %>%
  count(mot, periode, name = "documents") %>%
  tidyr::complete(mot, periode, fill = list(documents = 0)) %>%
  left_join(docs_par_periode, by = "periode") %>%
  mutate(part = round(100 * documents / docs, 1)) %>%
  select(mot, periode, documents, part) %>%
  arrange(mot, periode)


# =========================================================
# 8. Gaps : domaines de décision, méthodes, régions
# =========================================================
# Pour chaque document, on cherche des mots dans le titre, le résumé
# et les mots-clés des auteurs (pas les mots-clés attribués par les
# bases). Un document peut compter dans plusieurs lignes. Les motifs
# évitent les faux amis : "invest" sans "investigate", "judges" sans
# le paradigme "judge-advisor system".

texte <- tolower(paste(M$TI, M$AB, M$DE))
apres_2022 <- M$PY >= 2023

compter <- function(motifs) {
  bind_rows(lapply(names(motifs), function(nom) {
    trouve <- str_detect(texte, regex(motifs[[nom]]))
    tibble(
      categorie = nom,
      documents = sum(trouve),
      part = round(100 * mean(trouve), 1),
      avant_2023 = sum(trouve & !apres_2022),
      depuis_2023 = sum(trouve & apres_2022)
    )
  })) %>% arrange(desc(documents))
}

domaines <- compter(c(
  "Santé, médecine" = "\\b(health|clinical|clinician|medical|medicine|patient|radiolog|diagnos|nurs)",
  "Éducation" = "\\b(student|education|teacher|classroom|universit|learner)",
  "Finance, investissement" = "\\b(financ|invest(ment|ments|or|ors|ing)\\b|robo-advis|credit\\b|banking|stock market)",
  "Ressources humaines, recrutement" = "\\b(recruit|hiring|human resource|personnel selection|job applicant|employee selection)",
  "Justice, droit" = "\\b(judges\\b|judicial|courts?\\b|criminal|recidivism|bail\\b|sentencing)",
  "Secteur public, administration" = "\\b(public sector|government|public administration|civil servant|caseworker)",
  "Conduite, véhicules" = "\\b(driving|driver|vehicle|autonomous car)",
  "Militaire, défense" = "\\b(military|defen[cs]e|warfare|weapon)",
  "Consommation" = "\\b(consumer|shopping|purchase|e-commerce)",
  "Vie quotidienne, décisions personnelles" = "\\b(everyday|daily life|personal decision|life decision|lay user|laypeople)",
  "Compagnons, santé mentale, relations" = "\\b(companion|loneliness|mental health|romantic|friendship|emotional support|emotional dependence)"
))

methodes <- compter(c(
  "Expérience (en ligne ou en laboratoire)" = "\\b(experiment|randomi[sz]ed|between-subject|within-subject|vignette|mturk|prolific)",
  "Enquête par questionnaire" = "\\b(survey|questionnaire|structural equation|pls-sem|sem)\\b",
  "Qualitative (entretiens, focus groups)" = "\\b(interview|qualitative|focus group|thematic analysis|grounded theory)",
  "Revue de littérature, méta-analyse" = "\\b(systematic review|literature review|scoping review|meta-analy)",
  "Étude de terrain ou longitudinale" = "\\b(field study|field experiment|in the wild|longitudinal|diary study|panel data)"
))

regions <- tibble(
  region = c("États-Unis", "Europe (UE + Royaume-Uni)", "Chine", "Afrique", "Maroc", "Monde arabe"),
  motif = c(
    "\\bUSA\\b",
    "GERMANY|UNITED KINGDOM|ITALY|NETHERLANDS|FRANCE|SPAIN|SWITZERLAND|AUSTRIA|BELGIUM|SWEDEN|DENMARK|FINLAND|NORWAY|IRELAND|PORTUGAL|POLAND|GREECE|CZECH",
    "\\bCHINA\\b",
    "MOROCCO|ALGERIA|TUNISIA|EGYPT|LIBYA|SUDAN|NIGERIA|SOUTH AFRICA|KENYA|GHANA|ETHIOPIA|SENEGAL|CAMEROON|UGANDA|TANZANIA|RWANDA|BENIN|BOTSWANA|GAMBIA|SIERRA LEONE|SOMALIA|ZAMBIA|CONGO",
    "MOROCCO",
    "MOROCCO|ALGERIA|TUNISIA|EGYPT|LIBYA|SUDAN|SOMALIA|SAUDI ARABIA|ARAB EMIRATES|QATAR|JORDAN|LEBANON|KUWAIT|OMAN|BAHRAIN|IRAQ|SYRIA|PALESTINE|YEMEN"
  )
) %>%
  rowwise() %>%
  mutate(documents = sum(str_detect(coalesce(M_pays$AU_CO, ""), motif))) %>%
  ungroup() %>%
  mutate(part_des_documents_avec_pays = round(100 * documents / docs_avec_pays, 1)) %>%
  select(-motif)


# =========================================================
# 9. Export
# =========================================================

write_xlsx(
  list(
    infos_generales = resume$MainInformationDF,
    sources = resume$MostRelSources,
    articles_les_plus_cites = resume$MostCitedPapers,
    pays = pays,
    mots_cles = head(mots_cles, 50),
    carte_clusters = clusters,
    evolution_clusters = clusters_periodes,
    mots_cles_par_periode = parts_periodes,
    gaps_domaines = domaines,
    gaps_methodes = methodes,
    gaps_regions = regions
  ),
  "resultats/resultats_axe1.xlsx"
)

print(head(mots_cles, 25))
print(clusters, width = Inf)
print(clusters_periodes, n = Inf)
print(docs_par_periode)
print(tidyr::pivot_wider(parts_periodes %>% select(-documents), names_from = periode, values_from = part), n = Inf)
print(domaines)
print(methodes)
print(regions)
cat("Documents avec pays :", docs_avec_pays, "\n")
