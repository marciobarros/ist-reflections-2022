cat("\014")
rm(list = ls())

library(googlesheets4)
library(tidyverse)

data <- read_sheet("https://docs.google.com/spreadsheets/d/1cBqcR3uX8GSCmfI8ZRlN7JTfTj_AC_0iUFc5PsZAvUE/edit#gid=0") %>% 
  filter(!is.na(Language))


#
# Numero total de referencias - 88
#
data %>% 
  summarize(n = n()) %>% 
  pull()


#
# Analise por linguagem
#
# English       74
# Spanish        9
# Portuguese     4
# Chinese        1
#
data %>% 
  group_by(Language) %>% 
  summarize(n = n()) %>% 
  arrange(desc(n))


#
# Analise por tipo
#
# Primary      72
# Secondary    16
#
data %>% 
  group_by(Type) %>% 
  summarize(n = n()) %>% 
  arrange(desc(n))


#
# Analise por ano
#
# 2022     5
# 2021    12
# 2020    11
# 2019    16
# 2018    19
# 2017    13
# 2016    11
# 2015     1
#
data %>% 
  group_by(PubYear) %>% 
  summarize(n = n()) %>% 
  arrange(desc(PubYear))


#
# Analise por tipo de veiculo
#
# Journal             50
# Conference          26
# DSc Thesis           6
# MSc Dissertation     3
# Book                 2
# Thesis               1
#
data %>% 
  group_by(VehicleType) %>% 
  summarize(n = n()) %>% 
  arrange(desc(n))


#
# Analise por palavra chave
#
# requirements prioritization          26
# search-based software engineering    20
# next release problem                 16
# multi-objective optimization          9
# requirements engineering              9
# requirements selection                9
# software requirements                 7
# analytic hierarchy process            5
# meta-heuristics                       5
# release planning                      5
#
data %>% 
  select(Keywords) %>% 
  separate_rows(Keywords, sep=",") %>% 
  filter(!is.na(Keywords)) %>% 
  mutate(Keywords = str_to_lower(str_trim(Keywords))) %>% 
  group_by(Keywords) %>% 
  summarize(n = n()) %>% 
  arrange(desc(n))


#
# Analise por grupo de palavra chave
#
# Research field        96
# Software process      91
# N/A                   59
# Concept               38
# Algorithm             30
# Quality indicator     15
# Research technique    14
# Business              11
#
keyword_to_group <- tribble(
  ~Keywords,                                           ~KeywordsGroup,
  "requirements prioritization",                       "Software process",
  "search-based software engineering",                 "Research field",
  "next release problem",                              "Research field",
  "multi-objective optimization",                      "Research field",
  "requirements selection",                            "Software process",
  "requirements engineering",                          "Software process",
  "software requirements",                             "Software process",
  "analytic hierarchy process",                        "Algorithm",
  "meta-heuristics",                                   "Concept",
  "release planning",                                  "Software process",
  "software",                                          "Concept",
  "systematic review",                                 "Research technique",
  "dependencies",                                      "Concept",
  "functional requirements",                           "Software process",
  "fuzzy",                                             "N/A",
  "integer linear programming",                        "Algorithm",
  "optimization",                                      "Research field",
  "software engineering",                              "Research field",
  "stakeholders",                                      "Concept",
  "machine learning",                                  "Research field",
  "mobile learning",                                   "N/A",
  "multiple stakeholders",                             "Concept",
  "requirements specification",                        "Software process",
  "systematic mapping",                                "Research technique",
  "bibliometric analysis",                             "Research technique",
  "business process",                                  "Business",
  "case-based reasoning",                              "Research field",
  "component-based systems",                           "Software process",
  "evolutionary algorithms",                           "Research field",
  "fuzzy graphs",                                      "Research field",
  "fuzzy logic",                                       "Research field",
  "fuzzy numbers",                                     "Research field",
  "fuzzy set theory",                                  "Research field",
  "genetic algorithms",                                "Algorithm",
  "goal oriented requirements engineering",            "Software process",
  "hyper-heuristics",                                  "Concept",
  "institute examination system",                      "N/A",
  "m-learning requirements",                           "N/A",
  "many-objective optimization",                       "Research field",
  "multi-criteria decision making",                    "Research field",
  "pareto front",                                      "Concept",
  "reliability",                                       "Quality indicator",
  "risk-aware decision making",                        "Research field",
  "rough-set theory",                                  "Research field",
  "semantic analysis",                                 "Research field",
  "soft computing",                                    "Research field",
  "software product line",                             "Concept",
  "spanish community",                                 "N/A",
  "traceability",                                      "Quality indicator",
  "uncertainty",                                       "Quality indicator",
  "agile development",                                 "Software process",
  "agreement measure",                                 "Quality indicator",
  "anytime algorithm",                                 "Algorithm",
  "architecture decision support",                     "Software process",
  "artificial chemical reaction optimization algorithm", "Algorithm",
  "artificial intelligence",                           "Research field",
  "aspect extractions",                                "Software process",
  "binary artificial algae algorithm",                 "Algorithm",
  "binary search trees",                               "Concept",
  "business information systems",                      "Business",
  "cbranking",                                         "N/A",
  "challenges",                                        "N/A",
  "clustering algorithms",                             "Research field",
  "collaboration",                                     "Concept",
  "combinatorial optimization",                        "Research field",
  "companies",                                         "Business",
  "competitive market",                                "Business",
  "component-based software engineering",              "Research field",
  "components management",                             "Software process",
  "conferences",                                       "N/A",
  "configurable systems",                              "Software process",
  "configuration prioritization",                      "Software process",
  "configurations",                                    "N/A",
  "conformity",                                        "Quality indicator",
  "consistency",                                       "Quality indicator",
  "content analysis",                                  "N/A",
  "cooperative experimentation",                       "N/A",
  "cvcost-value",                                      "N/A",
  "decision making",                                   "N/A",
  "decision sciences",                                 "Research field",
  "decision-making",                                   "N/A",
  "differential evolution",                            "Algorithm",
  "domain requirements",                               "Software process",
  "effort estimation",                                 "Software process",
  "elicitation",                                       "N/A",
  "empirical software engineering",                    "Research field",
  "enterprise application integration",                "Business",
  "evolve",                                            "Algorithm",
  "fairness",                                          "Quality indicator",
  "feasibility evaluation",                            "Software process",
  "fuzzy analytic hierarchy process",                  "Algorithm",
  "fuzzy inference system",                            "Algorithm",
  "games",                                             "N/A",
  "global software development",                       "Software process",
  "goal model",                                        "Concept",
  "heuristics",                                        "Concept",
  "hierarchical clustering",                           "Algorithm",
  "hierarchical process",                              "N/A",
  "human preferences",                                 "Concept",
  "hybridized ranking model",                          "Algorithm",
  "ict project management",                            "N/A",
  "ict project risk management",                       "N/A",
  "ict project risk treatment strategies",             "N/A",
  "incomplete linguistic preference relation",         "N/A",
  "industrial relevance",                              "Business",
  "informatics",                                       "N/A",
  "information economics",                             "Business",
  "information system",                                "Business",
  "information technology investment",                 "Business",
  "interaction",                                       "N/A",
  "interactive optimization",                          "Algorithm",
  "interdependent requirements",                       "N/A",
  "iteration planning",                                "Software process",
  "k-means",                                           "Algorithm",
  "large set of requirements",                         "N/A",
  "link analysis",                                     "Software process",
  "literature survey",                                 "Research technique",
  "macbeth",                                           "N/A",
  "market research",                                   "Business",
  "master-slave model",                                "N/A",
  "mining",                                            "N/A",
  "model-driven engineering",                          "Software process",
  "moscow",                                            "Algorithm",
  "neural network",                                    "Algorithm",
  "non-functional requirements",                       "Software process",
  "numerical regression",                              "Algorithm",
  "operations research",                               "Research field",
  "pairwise comparison matrix",                        "N/A",
  "particle swarm optimization",                       "Algorithm",
  "performance metrics",                               "Quality indicator",
  "planning",                                          "Software process",
  "prioritization techniques",                         "Research field",
  "quality indicators",                                "Quality indicator",
  "quantitative criterion",                            "N/A",
  "rank prediction",                                   "N/A",
  "ranking algorithm",                                 "Algorithm",
  "requirements collision",                            "Software process",
  "requirements dependencies",                         "Software process",
  "requirements elicitation",                          "Software process",
  "research trends",                                   "N/A",
  "reuse",                                             "N/A",
  "rfp",                                               "N/A",
  "risk factors",                                      "N/A",
  "risk management",                                   "Software process",
  "runtime system",                                    "N/A",
  "scoring rubrics",                                   "N/A",
  "search algorithms",                                 "Research field",
  "search space reduction",                            "Research field",
  "search space smoothing",                            "Research field",
  "search space transformation",                       "Research field",
  "semantic clustering",                               "Research field",
  "serum",                                             "N/A",
  "similarity measures",                               "Quality indicator",
  "sns",                                               "N/A",
  "software analysis",                                 "Software process",
  "software development process",                      "Software process",
  "software life cycle",                               "Software process",
  "software management process",                       "Software process",
  "software systems",                                  "Concept",
  "solution model",                                    "N/A",
  "spider search",                                     "Algorithm",
  "technology acceptance model",                       "Research field",
  "tensor",                                            "Concept",
  "text mining",                                       "Concept",
  "tooling infrastructure",                            "N/A",
  "top ten ict project",                               "N/A",
  "topsis",                                            "N/A",
  "usability",                                         "Quality indicator",
  "user interface",                                    "Concept",
  "user preferences",                                  "Concept",
  "vagueness",                                         "Quality indicator",
  "value",                                             "Concept",
  "value-added features",                              "N/A",
  "value-related dependencies",                        "N/A",
  "vop",                                               "N/A",
  "agile software developmnets",                       "Software process",
  "data analytics",                                    "Research field",
  "software enhancements projects",                    "N/A",
  "delivery speed prediction",                         "N/A",
  "feature construction",                              "N/A",
  "search methods",                                    "Algorithm",
  "simulated annealing",                               "Algorithm",
  "isbsg",                                             "N/A",
  "system mapping study",                              "Research technique",
  "evidence-based software engineering",               "Research field",
  "large-scale requirements",                          "N/A"
)

missing_keywords <- data %>% 
  select(Keywords) %>% 
  separate_rows(Keywords, sep=",") %>% 
  filter(!is.na(Keywords)) %>% 
  mutate(Keywords = str_to_lower(str_trim(Keywords))) %>% 
  anti_join(keyword_to_group, by="Keywords")

if (nrow(missing_keywords) > 0) {
  message(paste(nrow(missing_keywords), "keywords without a group."))
  missing_keywords %>% print(n = 1000)
}

data %>% 
  select(Keywords) %>% 
  separate_rows(Keywords, sep=",") %>% 
  filter(!is.na(Keywords)) %>% 
  mutate(Keywords = str_to_lower(str_trim(Keywords))) %>% 
  inner_join(keyword_to_group, by="Keywords") %>% 
  group_by(KeywordsGroup) %>% 
  summarize(n = n()) %>% 
  arrange(desc(n))


#
# Analise por universidade
#
# islamic azad university                        5
# jamia millia islamia                           5
# universidad of almeria                         5
# flinders university                            4
# beihang university                             3
# indian institute of science                    3
# universidad de córdoba                         3
# universidad de málaga                          3
# universidade tecnológica federal do paraná     3
# universiti malaysia pahang                     3
#
data %>% 
  select(University) %>% 
  separate_rows(University, sep=",") %>% 
  filter(!is.na(University)) %>% 
  mutate(University = str_to_lower(str_trim(University))) %>% 
  filter(University != "") %>% 
  group_by(University) %>% 
  summarize(n = n()) %>% 
  arrange(desc(n))


#
# Analise por pais
#
# spain        14
# brazil       13
# india        11
# china        10
# malasya       7
# uk            6
# australia     5
# iran          5
# paquistan     4
# canada        3
#
data %>% 
  select(Country) %>% 
  separate_rows(Country, sep=",") %>% 
  filter(!is.na(Country)) %>% 
  mutate(Country = str_to_lower(str_trim(Country))) %>% 
  group_by(Country) %>% 
  summarize(n = n()) %>% 
  arrange(desc(n))


#
# Analise por continente
#
# asia             48
# europe           31
# south america    14
# north america     7
# oceania           7
# africa            5
#
country_to_continent <- tribble(
  ~Country,           ~Continent,
  "spain",            "europe",
  "brazil",           "south america",
  "india",            "asia",
  "china",            "asia",
  "malasya",          "asia",
  "uk",               "europe",
  "australia",        "oceania",
  "iran",             "asia",
  "paquistan",        "asia",
  "canada",           "north america",
  "italy",            "europe",
  "nigeria",          "africa",
  "saudi arabia",     "asia",
  "usa",              "north america",
  "jordan",           "asia",
  "netherlands",      "europe",
  "sweden",           "europe",
  "vietnam",          "asia",
  "chile",            "south america",
  "france",           "europe",
  "indonesia",        "asia",
  "japan",            "asia",
  "new zeland",       "oceania",
  "norway",           "europe",
  "oman",             "asia",
  "south africa",     "africa",
  "south korea",      "asia",
  "tunisia",          "africa",
  "mexico",           "north america"
)

missing_countries <- data %>% 
  select(Country) %>% 
  separate_rows(Country, sep=",") %>% 
  filter(!is.na(Country)) %>% 
  mutate(Country = str_to_lower(str_trim(Country))) %>% 
  anti_join(country_to_continent, by="Country")

if (nrow(missing_countries) > 0) {
  message(paste(nrow(missing_countries), "countries without a continent."))
  missing_countries %>% print(n = 1000)
}

data %>% 
  select(Country) %>% 
  separate_rows(Country, sep=",") %>% 
  filter(!is.na(Country)) %>% 
  mutate(Country = str_to_lower(str_trim(Country))) %>% 
  inner_join(country_to_continent, by="Country") %>% 
  group_by(Continent) %>% 
  summarize(n = n()) %>% 
  arrange(desc(n))

