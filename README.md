# Why do people rely on AI to decide?

Houda Es-sqalli · Computational Social Science (DAT602), taught by Prof. Aomar Ibourk · University Mohammed VI Polytechnic (UM6P), Rabat · October 2026

This repository holds the code and the shareable data behind my course project. The question: why do people sometimes rely on AI too much when they make decisions, and sometimes not enough? The project looks at it from two sides: what researchers study (a bibliometric corpus), and what people say about their own use of AI (a Reddit corpus). All analyses were done in R.

## Axis 1: what the research literature covers

- One search equation with three blocks (AI, relying on or delegating to it, decision). The terms were tested one by one and on random samples of titles before the final run ([`axis1_bibliometrics/search_equation/`](axis1_bibliometrics/search_equation/)).
- Three databases on the same day: Scopus (2,052 records), Web of Science (1,475) and OpenAlex (2,000 most relevant of 3,854). After merging and removing 2,022 duplicates: **3,505 publications**.
- Precision checked by hand on 60 random records: 40% core, 37% related, 23% off-topic. Off-topic records came mostly from OpenAlex, so the keyword analyses use only Scopus and Web of Science author keywords.
- Analyses with bibliometrix: thematic maps by period, keyword trends, and gaps.

Main results: the field took off in 2023 and changed object. The share of publications with the keyword *generative AI* went from 0 to 13.9%, while *algorithm aversion* fell from 16.5% to 6.4%. Gaps: everyday decisions (3.1% of publications), long-term or field studies, African countries, and the link between thinking less for oneself and relying on AI to decide (31 publications).

## Axis 2: what people say on Reddit

- Posts collected through the public Arctic Shift archive of Reddit, January 2023 to October 2026, in four AI subreddits (r/ChatGPT, r/OpenAI, r/ClaudeAI, r/ArtificialInteligence) and five everyday-life subreddits (r/DecidingToBeBetter, r/selfimprovement, r/productivity, r/college, r/Advice).
- From 12,443 collected posts to **633** after cleaning and a relevance filter: the person says they depend on AI or, in everyday subreddits, asks AI for advice. Each step is in [`selection_steps.csv`](axis2_reddit/data/selection_steps.csv). Precision checked by hand on 56 posts: 46% core, 43% related, 11% off-topic.
- Word frequencies and clustering, then topic models: LDA (K = 10, chosen with coherence and readability), a structural topic model (STM) with audience and year as covariates, and a biterm topic model (BTM) on titles.
- Sentiment with VADER, then **checked against my own hand coding** of 60 random posts.

Main results: AI subreddits talk about the tool and the tasks given to it (code, reliability of answers, the future of AI); everyday subreddits talk about the self, family, studies, therapy and habits. Over time, posts move from what AI is worth to the place it has taken in daily life. VADER rates most posts as positive, but my hand coding found 37 negative posts out of 60. VADER agreed with it on only 40% of posts (Cohen's kappa = 0.15), less than a naive rule that calls every post negative (62%). Lexicon-based sentiment should not be trusted on this kind of text without human validation.

## Repository

```
axis1_bibliometrics/
  scripts/            prepare each base, summarise, export for biblioshiny, analyses
  search_equation/    term tests and the precision sample (titles and judgments only)
  figures/
axis2_reddit/
  scripts/            01 collection, 02 cleaning and filter, 02b precision, 02c lemmatisation,
                      03 exploration, 04 topic models, 05 VADER and validation
  data/               post IDs, selection steps, precision sample, hand-coded sentiment
  figures/
```

The scripts were run from the original project folders, so the paths inside them are relative to those folders (for example `corpus/` for axis 2).

## What is not included, and why

- **Scopus and Web of Science exports**: their licences do not allow redistribution. The search equations are documented so the exports can be rebuilt.
- **Full text of the Reddit posts**: to respect the authors and Reddit's terms, only post IDs and metadata are shared. Usernames were never kept. The corpus can be rebuilt from the IDs with Arctic Shift.
- **`merge_bases.R`**: the merging script was provided in class by Prof. Ibourk and was used with only the file paths changed. It is his code, so it is not republished here.

## Main R packages

bibliometrix, httr2 and jsonlite (collection), tidytext, tm, udpipe, topicmodels, ldatuning, textmineR, stm, BTM, vader, caret, tidyverse.
