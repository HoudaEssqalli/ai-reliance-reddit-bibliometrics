# =========================================================
#  AXIS 1 - BIBLIOMETRICS
#  Bibliometric analysis: emerging themes and gaps
# =========================================================
# The same analyses as in biblioshiny, run from code to keep the exact
# numbers. Run 4_biblioshiny_file.R before this script, from the axis 1
# project folder.
# Outputs: results/ folder (PNG figures + results_axis1.xlsx)


# =========================================================
# 1. Packages and data
# =========================================================

library(bibliometrix)
library(dplyr)
library(stringr)
library(ggplot2)
library(writexl)

load("corpus_biblioshiny.RData")
dir.create("results", showWarnings = FALSE)


# =========================================================
# 2. Harmonising the keywords (step 4 of the course slides)
# =========================================================
# Long dashes -> simple hyphens, then synonyms grouped under a single
# term (the first one on each line). "ARTIFICIAL INTELLIGENCE" and
# "AI" are removed from the maps: every document is about them, so they
# crowd out the other themes.

M$DE <- str_replace_all(M$DE, "[\u2013\u2014]", "-")

synonyms <- c(
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

to_remove <- c("ARTIFICIAL INTELLIGENCE", "AI")


# =========================================================
# 3. Describing the corpus
# =========================================================

res <- biblioAnalysis(M, sep = ";")
overview <- summary(res, k = 15, pause = FALSE, verbose = FALSE)

# AU_CO is already computed in 4_biblioshiny_file.R (addresses for
# Scopus and WoS, a dedicated export for OpenAlex): it is not
# recomputed here. Documents are counted by country row by row.
M_countries <- M
countries <- tibble(
  doc = rep(seq_len(nrow(M_countries)), lengths(str_split(M_countries$AU_CO, ";"))),
  country = unlist(str_split(M_countries$AU_CO, ";"))
) %>%
  filter(!is.na(country), country != "NA", country != "") %>%
  distinct(doc, country) %>%
  count(country, name = "documents", sort = TRUE)

docs_with_country <- sum(!is.na(M_countries$AU_CO) & M_countries$AU_CO != "")


# =========================================================
# 4. Most frequent keywords (harmonised)
# =========================================================

keywords_raw <- tibble(keyword = unlist(str_split(na.omit(M$DE), ";"))) %>%
  mutate(keyword = str_squish(keyword)) %>%
  filter(keyword != "")

synonym_table <- tibble(line = synonyms) %>%
  mutate(canonical = str_extract(line, "^[^;]+"), variant = str_split(line, ";")) %>%
  tidyr::unnest(variant)

keywords <- keywords_raw %>%
  left_join(synonym_table %>% select(variant, canonical), by = c("keyword" = "variant")) %>%
  mutate(keyword = coalesce(canonical, keyword)) %>%
  count(keyword, name = "documents", sort = TRUE)


# =========================================================
# 5. Thematic map (whole corpus)
# =========================================================

# Robustness check (minfreq 5 and 10, walktrap and louvain clustering):
# the same four families of themes come back every time, but their
# quadrant changes from one setting to the next. We therefore keep the
# families, not the quadrants; emergence is measured in section 7b.

thematic_map <- thematicMap(
  M, field = "DE", n = 250, minfreq = 5, ngrams = 1,
  stemming = FALSE, size = 0.5, n.labels = 3, repel = TRUE,
  remove.terms = to_remove, synonyms = synonyms, cluster = "walktrap"
)

ggsave("results/thematic_map.png", thematic_map$map, width = 10, height = 8, dpi = 200)

clusters <- thematic_map$clusters %>%
  select(cluster = name, occurrences = freq, centrality = rcentrality,
         density = rdensity, words = words)


# =========================================================
# 6. Thematic evolution: before ChatGPT, 2023-2024, 2025-2026
# =========================================================

evolution <- thematicEvolution(
  M, field = "DE", years = c(2022, 2024), n = 250, minFreq = 5,
  ngrams = 1, stemming = FALSE, size = 0.5, n.labels = 3, repel = TRUE,
  remove.terms = to_remove, synonyms = synonyms, cluster = "walktrap"
)

periods <- c("2015-2022", "2023-2024", "2025-2027")
for (i in seq_along(evolution$TM)) {
  ggsave(sprintf("results/thematic_map_%s.png", periods[i]), evolution$TM[[i]]$map,
         width = 10, height = 8, dpi = 200)
}

clusters_periods <- bind_rows(lapply(seq_along(evolution$TM), function(i) {
  evolution$TM[[i]]$clusters %>%
    transmute(period = periods[i], cluster = name, occurrences = freq,
              centrality = rcentrality, density = rdensity, words = words)
}))


# =========================================================
# 7. Trending themes (median year of appearance)
# =========================================================

trends <- fieldByYear(
  M, field = "DE", timespan = c(2015, 2026), min.freq = 10, n.items = 4,
  remove.terms = to_remove, synonyms = synonyms, graph = TRUE
)

ggsave("results/trend_topics.png", trends$graph, width = 9, height = 10, dpi = 200)


# =========================================================
# 7b. Share of documents by keyword and by period
# =========================================================
# A measure of emergence that does not depend on clustering: among
# documents with author keywords, the share carrying each
# (harmonised) keyword, by period.

doc_keywords <- tibble(
  doc = rep(seq_len(nrow(M)), lengths(str_split(coalesce(M$DE, ""), ";"))),
  keyword = str_squish(unlist(str_split(coalesce(M$DE, ""), ";")))
) %>%
  filter(keyword != "") %>%
  left_join(synonym_table %>% select(variant, canonical), by = c("keyword" = "variant")) %>%
  mutate(keyword = coalesce(canonical, keyword)) %>%
  distinct(doc, keyword) %>%
  mutate(period = cut(M$PY[doc], c(-Inf, 2022, 2024, Inf), labels = periods))

docs_per_period <- doc_keywords %>% distinct(doc, period) %>% count(period, name = "docs")

tracked_keywords <- c(
  "ALGORITHM AVERSION", "ADVICE TAKING", "ALGORITHM APPRECIATION", "DECISION SUPPORT SYSTEMS",
  "AUTOMATION BIAS", "TRUST", "EXPLAINABLE AI", "TRUST CALIBRATION", "APPROPRIATE RELIANCE",
  "HUMAN-AI COLLABORATION", "GENERATIVE AI", "LARGE LANGUAGE MODELS", "CHATGPT",
  "COGNITIVE OFFLOADING", "CRITICAL THINKING", "AI LITERACY", "HIGHER EDUCATION"
)

shares_by_period <- doc_keywords %>%
  filter(keyword %in% tracked_keywords) %>%
  count(keyword, period, name = "documents") %>%
  tidyr::complete(keyword, period, fill = list(documents = 0)) %>%
  left_join(docs_per_period, by = "period") %>%
  mutate(share = round(100 * documents / docs, 1)) %>%
  select(keyword, period, documents, share) %>%
  arrange(keyword, period)


# =========================================================
# 8. Gaps: decision domains, methods, regions
# =========================================================
# For each document, words are searched in the title, the abstract and
# the author keywords (not the keywords assigned by the databases). A
# document can count in several rows. The patterns avoid false
# friends: "invest" without "investigate", "judges" without the
# "judge-advisor system" paradigm.

text <- tolower(paste(M$TI, M$AB, M$DE))
since_2023 <- M$PY >= 2023

count_matches <- function(patterns) {
  bind_rows(lapply(names(patterns), function(name) {
    found <- str_detect(text, regex(patterns[[name]]))
    tibble(
      category = name,
      documents = sum(found),
      share = round(100 * mean(found), 1),
      before_2023 = sum(found & !since_2023),
      since_2023 = sum(found & since_2023)
    )
  })) %>% arrange(desc(documents))
}

domains <- count_matches(c(
  "Health, medicine" = "\\b(health|clinical|clinician|medical|medicine|patient|radiolog|diagnos|nurs)",
  "Education" = "\\b(student|education|teacher|classroom|universit|learner)",
  "Finance, investment" = "\\b(financ|invest(ment|ments|or|ors|ing)\\b|robo-advis|credit\\b|banking|stock market)",
  "Human resources, recruitment" = "\\b(recruit|hiring|human resource|personnel selection|job applicant|employee selection)",
  "Justice, law" = "\\b(judges\\b|judicial|courts?\\b|criminal|recidivism|bail\\b|sentencing)",
  "Public sector, administration" = "\\b(public sector|government|public administration|civil servant|caseworker)",
  "Driving, vehicles" = "\\b(driving|driver|vehicle|autonomous car)",
  "Military, defence" = "\\b(military|defen[cs]e|warfare|weapon)",
  "Consumer choices" = "\\b(consumer|shopping|purchase|e-commerce)",
  "Everyday life, personal decisions" = "\\b(everyday|daily life|personal decision|life decision|lay user|laypeople)",
  "Companions, mental health, relationships" = "\\b(companion|loneliness|mental health|romantic|friendship|emotional support|emotional dependence)"
))

methods <- count_matches(c(
  "Experiment (online or in the lab)" = "\\b(experiment|randomi[sz]ed|between-subject|within-subject|vignette|mturk|prolific)",
  "Questionnaire survey" = "\\b(survey|questionnaire|structural equation|pls-sem|sem)\\b",
  "Qualitative (interviews, focus groups)" = "\\b(interview|qualitative|focus group|thematic analysis|grounded theory)",
  "Literature review, meta-analysis" = "\\b(systematic review|literature review|scoping review|meta-analy)",
  "Field or longitudinal study" = "\\b(field study|field experiment|in the wild|longitudinal|diary study|panel data)"
))

regions <- tibble(
  region = c("United States", "Europe (EU + United Kingdom)", "China", "Africa", "Morocco", "Arab world"),
  pattern = c(
    "\\bUSA\\b",
    "GERMANY|UNITED KINGDOM|ITALY|NETHERLANDS|FRANCE|SPAIN|SWITZERLAND|AUSTRIA|BELGIUM|SWEDEN|DENMARK|FINLAND|NORWAY|IRELAND|PORTUGAL|POLAND|GREECE|CZECH",
    "\\bCHINA\\b",
    "MOROCCO|ALGERIA|TUNISIA|EGYPT|LIBYA|SUDAN|NIGERIA|SOUTH AFRICA|KENYA|GHANA|ETHIOPIA|SENEGAL|CAMEROON|UGANDA|TANZANIA|RWANDA|BENIN|BOTSWANA|GAMBIA|SIERRA LEONE|SOMALIA|ZAMBIA|CONGO",
    "MOROCCO",
    "MOROCCO|ALGERIA|TUNISIA|EGYPT|LIBYA|SUDAN|SOMALIA|SAUDI ARABIA|ARAB EMIRATES|QATAR|JORDAN|LEBANON|KUWAIT|OMAN|BAHRAIN|IRAQ|SYRIA|PALESTINE|YEMEN"
  )
) %>%
  rowwise() %>%
  mutate(documents = sum(str_detect(coalesce(M_countries$AU_CO, ""), pattern))) %>%
  ungroup() %>%
  mutate(share_of_documents_with_country = round(100 * documents / docs_with_country, 1)) %>%
  select(-pattern)


# =========================================================
# 9. Export
# =========================================================

write_xlsx(
  list(
    main_information = overview$MainInformationDF,
    sources = overview$MostRelSources,
    most_cited_papers = overview$MostCitedPapers,
    countries = countries,
    keywords = head(keywords, 50),
    map_clusters = clusters,
    evolution_clusters = clusters_periods,
    keywords_by_period = shares_by_period,
    gaps_domains = domains,
    gaps_methods = methods,
    gaps_regions = regions
  ),
  "results/results_axis1.xlsx"
)

print(head(keywords, 25))
print(clusters, width = Inf)
print(clusters_periods, n = Inf)
print(docs_per_period)
print(tidyr::pivot_wider(shares_by_period %>% select(-documents), names_from = period, values_from = share), n = Inf)
print(domains)
print(methods)
print(regions)
cat("Documents with a country:", docs_with_country, "\n")
