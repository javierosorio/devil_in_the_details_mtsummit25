#####################################################################
# UN Multilingual Corpus
# Dependency trees
# Javier Osorio 
# 9-12-2024
#####################################################################


# GOOGLE COLAB --------------------------------------------------

# This scripts requires the generation of dependency distances in EN, AR, and ES using Google Colab.
# This is the script to do so xxxx





# SETUP --------------------------------------------------

# Load all packages here

if (!require("pacman")) install.packages("pacman")
pacman::p_load(here, glue, openxlsx, tidyverse, readxl, dplyr, stringr, stringi, 
               ggplot2, grid, GGally , tidyverse, broom, stargazer,
               udpipe, textplot, ggraph, stringdist) 



# GET THE DATA --------------------------------------------------

data.ddm.en <- read.csv('data/output_data/ddm_en.csv')
data.ddm.es <- read.csv('data/output_data/ddm_es.csv')
data.ddm.ar <- read.csv('data/output_data/ddm_ar.csv')

# Check names
names(data.ddm.en)
names(data.ddm.es)
names(data.ddm.ar)


# Check means
summary(data.ddm.en)
summary(data.ddm.es)
summary(data.ddm.ar)





# PIVOT THE DATA --------------------------------------------------


# EN Pivot longer for box plot
names(data.ddm.en)


# Dependency distance
ddm.en.long <- data.ddm.en  %>% 
  pivot_longer(
    cols = c(ddm_en, ddm_ar_en_DEEP, ddm_ar_en_DEEPL, ddm_ar_en_GOOGLE, ddm_ar_en_TRANSFORMERS, ddm_es_en_DEEP, ddm_es_en_DEEPL, ddm_es_en_GOOGLE, ddm_es_en_TRANSOFMERS), 
    names_to = "model", 
    values_to = "ddm") %>%
  select(c(model, ddm)) %>%
  mutate(mt.tool = case_when(
    model=="ddm_en"  ~ "Native",
    model=="ddm_ar_en_DEEP" | model=="ddm_es_en_DEEP" ~ "Deep",
    model=="ddm_ar_en_DEEPL" | model=="ddm_es_en_DEEPL" ~ "DeepL",
    model=="ddm_ar_en_GOOGLE" | model=="ddm_es_en_GOOGLE" ~ "GT",
    model=="ddm_ar_en_TRANSFORMERS" | model=="ddm_es_en_TRANSOFMERS" ~ "OPUS")) %>%
  mutate(lang="EN") %>%
  mutate(trans.lang = case_when(
    model=="ddm_en"  ~ "Native",
    model=="ddm_ar_en_DEEP" | model=="ddm_ar_en_DEEPL" | model=="ddm_ar_en_GOOGLE" | model=="ddm_ar_en_TRANSFORMERS" ~ "AR to EN",
    model=="ddm_es_en_DEEP" | model=="ddm_es_en_DEEPL" | model=="ddm_es_en_GOOGLE" | model=="ddm_es_en_TRANSOFMERS" ~ "ES to EN" )) %>%
  select(-model)


# Dependency distance difference
ddm.diff.en.long <- data.ddm.en  %>% 
  pivot_longer(
    cols = c(diff_ddm_ar_en_DEEP, diff_ddm_ar_en_DEEPL, diff_ddm_ar_en_GOOGLE, diff_ddm_ar_en_TRANSFORMERS, diff_ddm_es_en_DEEP, diff_ddm_es_en_DEEPL, diff_ddm_es_en_GOOGLE, diff_ddm_es_en_TRANSOFMERS),
    names_to = "model", 
    values_to = "ddm.diff") %>%
  select(c(model, ddm.diff))  %>%
  mutate(mt.tool = case_when(
    model=="diff_ddm_ar_en_DEEP" | model=="diff_ddm_es_en_DEEP" ~ "Deep",
    model=="diff_ddm_ar_en_DEEPL" | model=="diff_ddm_es_en_DEEPL" ~ "DeepL",
    model=="diff_ddm_ar_en_GOOGLE" | model=="diff_ddm_es_en_GOOGLE" ~ "GT",
    model=="diff_ddm_ar_en_TRANSFORMERS" | model=="diff_ddm_es_en_TRANSOFMERS" ~ "OPUS")) %>%
  mutate(lang="EN") %>%
  mutate(trans.lang = case_when(
    model=="diff_ddm_ar_en_DEEP" | model=="diff_ddm_ar_en_DEEPL" | model=="diff_ddm_ar_en_GOOGLE" | model=="diff_ddm_ar_en_TRANSFORMERS" ~ "AR to EN",
    model=="diff_ddm_es_en_DEEP" | model=="diff_ddm_es_en_DEEPL" | model=="diff_ddm_es_en_GOOGLE" | model=="diff_ddm_es_en_TRANSOFMERS" ~ "ES to EN" )) %>%
  select(-model)




# ES Pivot longer for box plot
names(data.ddm.es)


# Dependency distance
ddm.es.long <- data.ddm.es  %>% 
  pivot_longer(
    cols = c(ddm_es,ddm_en_es_DEEP,ddm_en_es_DEEPL,ddm_en_es_GOOGLE,ddm_en_es_TRANSFORMERS), 
    names_to = "model", 
    values_to = "ddm") %>%
  select(c(model, ddm)) %>%
  mutate(mt.tool = case_when(
    model=="ddm_es"  ~ "Native",
    model=="ddm_en_es_DEEP" ~ "Deep",
    model=="ddm_en_es_DEEPL"  ~ "DeepL",
    model=="ddm_en_es_GOOGLE"  ~ "GT",
    model=="ddm_en_es_TRANSFORMERS"  ~ "OPUS")) %>%
  mutate(lang="ES") %>%
  mutate(trans.lang = case_when(
    model=="ddm_es"  ~ "Native",
    model=="ddm_en_es_DEEP" | model=="ddm_en_es_DEEPL" | model=="ddm_en_es_GOOGLE" | model=="ddm_en_es_TRANSFORMERS" ~ "EN to ES" )) %>%
  select(-model)


# Dependency distance difference
ddm.diff.es.long <- data.ddm.es  %>% 
  pivot_longer(
    cols = c(diff_ddm_en_es_DEEP,diff_ddm_en_es_DEEPL,diff_ddm_en_es_GOOGLE,diff_ddm_en_es_TRANSFORMERS),
    names_to = "model", 
    values_to = "ddm.diff") %>%
  select(c(model, ddm.diff))  %>%
  mutate(mt.tool = case_when(
    model=="diff_ddm_en_es_DEEP" ~ "Deep",
    model=="diff_ddm_en_es_DEEPL"  ~ "DeepL",
    model=="diff_ddm_en_es_GOOGLE" ~ "GT",
    model=="diff_ddm_en_es_TRANSFORMERS"  ~ "OPUS")) %>%
  mutate(lang="ES") %>%
  mutate(trans.lang = case_when(
    model=="diff_ddm_en_es_DEEP" | model=="diff_ddm_en_es_DEEPL" | model=="diff_ddm_en_es_GOOGLE" | model=="diff_ddm_en_es_TRANSFORMERS" ~ "EN to ES" )) %>%
  select(-model)



# AR Pivot longer for box plot
names(data.ddm.ar)


# Dependency distance
ddm.ar.long <- data.ddm.ar  %>% 
  pivot_longer(
    cols = c(ddm_ar,ddm_en_ar_DEEP,ddm_en_ar_DEEPL,ddm_en_ar_GOOGLE,ddm_en_ar_TRANSFORMERS), 
    names_to = "model", 
    values_to = "ddm") %>%
  select(c(model, ddm)) %>%
  mutate(mt.tool = case_when(
    model=="ddm_ar"  ~ "Native",
    model=="ddm_en_ar_DEEP" ~ "Deep",
    model=="ddm_en_ar_DEEPL"  ~ "DeepL",
    model=="ddm_en_ar_GOOGLE"  ~ "GT",
    model=="ddm_en_ar_TRANSFORMERS"  ~ "OPUS")) %>%
  mutate(lang="AR") %>%
  mutate(trans.lang = case_when(
    model=="ddm_ar"  ~ "Native",
    model=="ddm_en_ar_DEEP" | model=="ddm_en_ar_DEEPL" | model=="ddm_en_ar_GOOGLE" | model=="ddm_en_ar_TRANSFORMERS" ~ "EN to AR" )) %>%
  select(-model)


# Dependency distance difference
ddm.diff.ar.long <- data.ddm.ar  %>% 
  pivot_longer(
    cols = c(diff_ddm_en_ar_DEEP,diff_ddm_en_ar_DEEPL,diff_ddm_en_ar_GOOGLE,diff_ddm_en_ar_TRANSFORMERS),
    names_to = "model", 
    values_to = "ddm.diff") %>%
  select(c(model, ddm.diff))  %>%
  mutate(mt.tool = case_when(
    model=="diff_ddm_en_ar_DEEP" ~ "Deep",
    model=="diff_ddm_en_ar_DEEPL"  ~ "DeepL",
    model=="diff_ddm_en_ar_GOOGLE" ~ "GT",
    model=="diff_ddm_en_ar_TRANSFORMERS"  ~ "OPUS")) %>%
  mutate(lang="AR") %>%
  mutate(trans.lang = case_when(
    model=="diff_ddm_en_ar_DEEP" | model=="diff_ddm_en_ar_DEEPL" | model=="diff_ddm_en_ar_GOOGLE" | model=="diff_ddm_en_ar_TRANSFORMERS" ~ "EN to AR" )) %>%
  select(-model)



## Append all data frames

ddm.diff.all <- rbind(ddm.diff.en.long,ddm.diff.es.long,ddm.diff.ar.long)
names(ddm.diff.all)


ddm.all <- rbind(ddm.en.long,ddm.es.long,ddm.ar.long)
names(ddm.all)




# EXPLORE THE DATA --------------------------------------------------


# Explore the distribution of dependency distance differences
names(ddm.diff.all)
table(ddm.diff.all$mt.tool)
table(ddm.diff.all$trans.lang)
table(ddm.diff.all$lang)



# Plot
ggplot(ddm.diff.all, aes(x=mt.tool, y=ddm.diff)) + 
  geom_violin(fill="#A4A4A4") +
  facet_grid(~factor(trans.lang, levels=c('EN to AR', 'AR to EN', 'EN to ES', "ES to EN"))) +
  labs(x = NULL, y = "DDM difference ") +
  theme_light() +
  scale_x_discrete(guide = guide_axis(angle = 90))+
  theme(strip.text = element_text(colour = 'black'))

# Save plot 
ggsave(glue("graphs/ddm/ddm_diff_v1.pdf"), width = 5, height = 2.5, units = "in")




# Plot DDM
ggplot(ddm.all, aes(x=mt.tool, y=ddm)) + 
  geom_violin(fill="#A4A4A4") +
  facet_grid(~factor(trans.lang, levels=c('EN to AR', 'AR to EN', 'EN to ES', "ES to EN", "Native"))) +
  labs(x = NULL, y = "DDM ") +
  theme_light() +
  scale_x_discrete(guide = guide_axis(angle = 90))+
  theme(strip.text = element_text(colour = 'black'))


# Save plot 
ggsave(glue("graphs/ddm/ddm_v1.pdf"), width = 5, height = 2.5, units = "in")






#  CLEAN UP DATA FOR REGRESSION ---------------




# Get prediction data
data.pred <- read.csv('results/prediction_merged_Data_Master.csv')

data.pred <- cbind(data.pred,data.ddm.en,data.ddm.es,data.ddm.ar)

names(data.pred)


# Check names
names(data.ddm.en)
names(data.ddm.es)
names(data.ddm.ar)




##  Eliminate variables ---------------

data.pred <- data.pred %>%	
  select(-c(	
    ar,
    ar_en_DEEP,
    ar_en_DEEP_to_en_difference_general_rarity,
    ar_en_DEEP_to_en_difference_genre_rarity,
    ar_en_DEEP_to_en_difference_lemma_counts,
    ar_en_DEEP_to_en_difference_noun_counts,
    ar_en_DEEP_to_en_difference_verb_counts,
    ar_en_DEEPL,
    ar_en_DEEPL_to_en_difference_general_rarity,
    ar_en_DEEPL_to_en_difference_genre_rarity,
    ar_en_DEEPL_to_en_difference_lemma_counts,
    ar_en_DEEPL_to_en_difference_noun_counts,
    ar_en_DEEPL_to_en_difference_verb_counts,
    ar_en_GOOGLE,
    ar_en_GOOGLE_to_en_difference_general_rarity,
    ar_en_GOOGLE_to_en_difference_genre_rarity,
    ar_en_GOOGLE_to_en_difference_lemma_counts,
    ar_en_GOOGLE_to_en_difference_noun_counts,
    ar_en_GOOGLE_to_en_difference_verb_counts,
    ar_en_TRANSFORMERS,
    ar_en_TRANSFORMERS_to_en_difference_general_rarity,
    ar_en_TRANSFORMERS_to_en_difference_genre_rarity,
    ar_en_TRANSFORMERS_to_en_difference_lemma_counts,
    ar_en_TRANSFORMERS_to_en_difference_noun_counts,
    ar_en_TRANSFORMERS_to_en_difference_verb_counts,
    en,
    en_ar_DEEP,
    en_ar_DEEP_to_ar_difference_general_rarity,
    en_ar_DEEP_to_ar_difference_genre_rarity,
    en_ar_DEEP_to_ar_difference_lemma_counts,
    en_ar_DEEP_to_ar_difference_noun_counts,
    en_ar_DEEP_to_ar_difference_verb_counts,
    en_ar_DEEPL,
    en_ar_DEEPL_to_ar_difference_general_rarity,
    en_ar_DEEPL_to_ar_difference_genre_rarity,
    en_ar_DEEPL_to_ar_difference_lemma_counts,
    en_ar_DEEPL_to_ar_difference_noun_counts,
    en_ar_DEEPL_to_ar_difference_verb_counts,
    en_ar_GOOGLE,
    en_ar_GOOGLE_to_ar_difference_general_rarity,
    en_ar_GOOGLE_to_ar_difference_genre_rarity,
    en_ar_GOOGLE_to_ar_difference_lemma_counts,
    en_ar_GOOGLE_to_ar_difference_noun_counts,
    en_ar_GOOGLE_to_ar_difference_verb_counts,
    en_ar_TRANSFORMERS,
    en_ar_TRANSFORMERS_to_ar_difference_general_rarity,
    en_ar_TRANSFORMERS_to_ar_difference_genre_rarity,
    en_ar_TRANSFORMERS_to_ar_difference_lemma_counts,
    en_ar_TRANSFORMERS_to_ar_difference_noun_counts,
    en_ar_TRANSFORMERS_to_ar_difference_verb_counts,
    en_es_DEEP,
    en_es_DEEP_to_es_difference_general_rarity,
    en_es_DEEP_to_es_difference_genre_rarity,
    en_es_DEEP_to_es_difference_lemma_counts,
    en_es_DEEP_to_es_difference_noun_counts,
    en_es_DEEP_to_es_difference_verb_counts,
    en_es_DEEPL,
    en_es_DEEPL_to_es_difference_general_rarity,
    en_es_DEEPL_to_es_difference_genre_rarity,
    en_es_DEEPL_to_es_difference_lemma_counts,
    en_es_DEEPL_to_es_difference_noun_counts,
    en_es_DEEPL_to_es_difference_verb_counts,
    en_es_GOOGLE,
    en_es_GOOGLE_to_es_difference_general_rarity,
    en_es_GOOGLE_to_es_difference_genre_rarity,
    en_es_GOOGLE_to_es_difference_lemma_counts,
    en_es_GOOGLE_to_es_difference_noun_counts,
    en_es_GOOGLE_to_es_difference_verb_counts,
    en_es_TRANSFORMERS,
    en_es_TRANSFORMERS_to_es_difference_general_rarity,
    en_es_TRANSFORMERS_to_es_difference_genre_rarity,
    en_es_TRANSFORMERS_to_es_difference_lemma_counts,
    en_es_TRANSFORMERS_to_es_difference_noun_counts,
    en_es_TRANSFORMERS_to_es_difference_verb_counts,
    es,
    es_en_DEEP,
    es_en_DEEP_to_en_difference_general_rarity,
    es_en_DEEP_to_en_difference_genre_rarity,
    es_en_DEEP_to_en_difference_lemma_counts,
    es_en_DEEP_to_en_difference_noun_counts,
    es_en_DEEP_to_en_difference_verb_counts,
    es_en_DEEPL,
    es_en_DEEPL_to_en_difference_general_rarity,
    es_en_DEEPL_to_en_difference_genre_rarity,
    es_en_DEEPL_to_en_difference_lemma_counts,
    es_en_DEEPL_to_en_difference_noun_counts,
    es_en_DEEPL_to_en_difference_verb_counts,
    es_en_GOOGLE,
    es_en_GOOGLE_to_en_difference_general_rarity,
    es_en_GOOGLE_to_en_difference_genre_rarity,
    es_en_GOOGLE_to_en_difference_lemma_counts,
    es_en_GOOGLE_to_en_difference_noun_counts,
    es_en_GOOGLE_to_en_difference_verb_counts,
    es_en_TRANSFORMERS_to_en_difference_general_rarity,
    es_en_TRANSFORMERS_to_en_difference_genre_rarity,
    es_en_TRANSFORMERS_to_en_difference_lemma_counts,
    es_en_TRANSFORMERS_to_en_difference_noun_counts,
    es_en_TRANSFORMERS_to_en_difference_verb_counts,
    es_en_TRANSOFMERS
    ))	



# Check number of variables
dim(data.pred)[2]

# Check var names
names(data.pred)




#  SUBSET THE DATA  ---------------

## AR ---------------
data.ar  <- data.pred %>%  
  select(c(ar_lemma_counts,
           ar_noun_counts,
           ar_Prediction_Score,
           ar_genre_rarity,
           ar_general_rarity,
           ar_verb_counts,
           en_ar_DEEP_Classification,
           en_ar_DEEP_lemma_counts,
           en_ar_DEEP_difference_lemma_counts,
           en_ar_DEEP_noun_counts,
           en_ar_DEEP_difference_noun_counts,
           en_ar_DEEP_Prediction_Score,
           en_ar_DEEP_genre_rarity,
           en_ar_DEEP_to_en_difference_genre_rarity,
           en_ar_DEEP_general_rarity,
           en_ar_DEEP_to_en_difference_general_rarity,
           en_ar_DEEP_verb_counts,
           en_ar_DEEP_difference_verb_counts,
           en_ar_DEEPL_Classification,
           en_ar_DEEPL_lemma_counts,
           en_ar_DEEPL_difference_lemma_counts,
           en_ar_DEEPL_noun_counts,
           en_ar_DEEPL_difference_noun_counts,
           en_ar_DEEPL_Prediction_Score,
           en_ar_DEEPL_genre_rarity,
           en_ar_DEEPL_to_en_difference_genre_rarity,
           en_ar_DEEPL_general_rarity,
           en_ar_DEEPL_to_en_difference_general_rarity,
           en_ar_DEEPL_verb_counts,
           en_ar_DEEPL_difference_verb_counts,
           en_ar_GOOGLE_Classification,
           en_ar_GOOGLE_lemma_counts,
           en_ar_GOOGLE_difference_lemma_counts,
           en_ar_GOOGLE_noun_counts,
           en_ar_GOOGLE_difference_noun_counts,
           en_ar_GOOGLE_Prediction_Score,
           en_ar_GOOGLE_genre_rarity,
           en_ar_GOOGLE_to_en_difference_genre_rarity,
           en_ar_GOOGLE_general_rarity,
           en_ar_GOOGLE_to_en_difference_general_rarity,
           en_ar_GOOGLE_verb_counts,
           en_ar_GOOGLE_difference_verb_counts,
           en_ar_TRANSFORMERS_Classification,
           en_ar_TRANSFORMERS_lemma_counts,
           en_ar_TRANSFORMERS_difference_lemma_counts,
           en_ar_TRANSFORMERS_noun_counts,
           en_ar_TRANSFORMERS_difference_noun_counts,
           en_ar_TRANSFORMERS_Prediction_Score,
           en_ar_TRANSFORMERS_genre_rarity,
           en_ar_TRANSFORMERS_to_en_difference_genre_rarity,
           en_ar_TRANSFORMERS_general_rarity,
           en_ar_TRANSFORMERS_to_en_difference_general_rarity,
           en_ar_TRANSFORMERS_verb_counts,
           en_ar_TRANSFORMERS_difference_verb_counts,
           ar_Classification,
           # new section
           ddm_en_ar_DEEP,
           ddm_en_ar_DEEPL,
           ddm_en_ar_GOOGLE,
           ddm_en_ar_TRANSFORMERS,
           diff_ddm_en_ar_DEEPL,
           diff_ddm_en_ar_GOOGLE,
           diff_ddm_en_ar_TRANSFORMERS,
           diff_ddm_en_ar_DEEP))




## EN ---------------
data.en  <- data.pred %>%  
  select(c(ar_en_DEEP_Classification,
           ar_en_DEEP_lemma_counts,
           ar_en_DEEP_difference_lemma_counts,
           ar_en_DEEP_noun_counts,
           ar_en_DEEP_difference_noun_counts,
           ar_en_DEEP_Prediction_Score,
           ar_en_DEEP_genre_rarity,
           ar_en_DEEP_to_ar_difference_genre_rarity,
           ar_en_DEEP_general_rarity,
           ar_en_DEEP_to_ar_difference_general_rarity,
           ar_en_DEEP_verb_counts,
           ar_en_DEEP_difference_verb_counts,
           ar_en_DEEPL_Classification,
           ar_en_DEEPL_lemma_counts,
           ar_en_DEEPL_difference_lemma_counts,
           ar_en_DEEPL_noun_counts,
           ar_en_DEEPL_difference_noun_counts,
           ar_en_DEEPL_Prediction_Score,
           ar_en_DEEPL_genre_rarity,
           ar_en_DEEPL_to_ar_difference_genre_rarity,
           ar_en_DEEPL_general_rarity,
           ar_en_DEEPL_to_ar_difference_general_rarity,
           ar_en_DEEPL_verb_counts,
           ar_en_DEEPL_difference_verb_counts,
           ar_en_GOOGLE_Classification,
           ar_en_GOOGLE_lemma_counts,
           ar_en_GOOGLE_difference_lemma_counts,
           ar_en_GOOGLE_noun_counts,
           ar_en_GOOGLE_difference_noun_counts,
           ar_en_GOOGLE_Prediction_Score,
           ar_en_GOOGLE_genre_rarity,
           ar_en_GOOGLE_to_ar_difference_genre_rarity,
           ar_en_GOOGLE_general_rarity,
           ar_en_GOOGLE_to_ar_difference_general_rarity,
           ar_en_GOOGLE_verb_counts,
           ar_en_GOOGLE_difference_verb_counts,
           ar_en_TRANSFORMERS_Classification,
           ar_en_TRANSFORMERS_lemma_counts,
           ar_en_TRANSFORMERS_difference_lemma_counts,
           ar_en_TRANSFORMERS_noun_counts,
           ar_en_TRANSFORMERS_difference_noun_counts,
           ar_en_TRANSFORMERS_Prediction_Score,
           ar_en_TRANSFORMERS_genre_rarity,
           ar_en_TRANSFORMERS_to_ar_difference_genre_rarity,
           ar_en_TRANSFORMERS_general_rarity,
           ar_en_TRANSFORMERS_to_ar_difference_general_rarity,
           ar_en_TRANSFORMERS_verb_counts,
           ar_en_TRANSFORMERS_difference_verb_counts,
           es_en_DEEP_Classification,
           es_en_DEEP_lemma_counts,
           es_en_DEEP_difference_lemma_counts,
           es_en_DEEP_noun_counts,
           es_en_DEEP_difference_noun_counts,
           es_en_DEEP_Prediction_Score,
           es_en_DEEP_genre_rarity,
           es_en_DEEP_to_es_difference_genre_rarity,
           es_en_DEEP_general_rarity,
           es_en_DEEP_to_es_difference_general_rarity,
           es_en_DEEP_verb_counts,
           es_en_DEEP_difference_verb_counts,
           es_en_DEEPL_Classification,
           es_en_DEEPL_lemma_counts,
           es_en_DEEPL_difference_lemma_counts,
           es_en_DEEPL_noun_counts,
           es_en_DEEPL_difference_noun_counts,
           es_en_DEEPL_Prediction_Score,
           es_en_DEEPL_genre_rarity,
           es_en_DEEPL_to_es_difference_genre_rarity,
           es_en_DEEPL_general_rarity,
           es_en_DEEPL_to_es_difference_general_rarity,
           es_en_DEEPL_verb_counts,
           es_en_DEEPL_difference_verb_counts,
           es_en_GOOGLE_Classification,
           es_en_GOOGLE_lemma_counts,
           es_en_GOOGLE_difference_lemma_counts,
           es_en_GOOGLE_noun_counts,
           es_en_GOOGLE_difference_noun_counts,
           es_en_GOOGLE_Prediction_Score,
           es_en_GOOGLE_genre_rarity,
           es_en_GOOGLE_to_es_difference_genre_rarity,
           es_en_GOOGLE_general_rarity,
           es_en_GOOGLE_to_es_difference_general_rarity,
           es_en_GOOGLE_verb_counts,
           es_en_GOOGLE_difference_verb_counts,
           es_en_TRANSOFMERS_Classification,
           es_en_TRANSFORMERS_lemma_counts,
           es_en_TRANSFORMERS_difference_lemma_counts,
           es_en_TRANSFORMERS_noun_counts,
           es_en_TRANSFORMERS_difference_noun_counts,
           es_en_TRANSOFMERS_Prediction_Score,
           es_en_TRANSFORMERS_genre_rarity,
           es_en_TRANSFORMERS_to_es_difference_genre_rarity,
           es_en_TRANSFORMERS_general_rarity,
           es_en_TRANSFORMERS_to_es_difference_general_rarity,
           es_en_TRANSFORMERS_verb_counts,
           es_en_TRANSFORMERS_difference_verb_counts,
           en_Classification,
           en_lemma_counts,
           en_noun_counts,
           en_Prediction_Score,
           en_genre_rarity,
           en_general_rarity,
           en_verb_counts,
           # new section
           ddm_en,
           ddm_ar_en_DEEP,
           ddm_ar_en_DEEPL,
           ddm_ar_en_GOOGLE,
           ddm_ar_en_TRANSFORMERS,
           ddm_es_en_DEEP,
           ddm_es_en_DEEPL,
           ddm_es_en_GOOGLE,
           ddm_es_en_TRANSOFMERS,
           diff_ddm_ar_en_DEEP,
           diff_ddm_ar_en_DEEPL,
           diff_ddm_ar_en_GOOGLE,
           diff_ddm_ar_en_TRANSFORMERS,
           diff_ddm_es_en_DEEP,
           diff_ddm_es_en_DEEPL,
           diff_ddm_es_en_GOOGLE,
           diff_ddm_es_en_TRANSOFMERS))




## ES ---------------
data.es  <- data.pred %>%  
  select(c(en_es_DEEP_Classification,
           en_es_DEEP_lemma_counts,
           en_es_DEEP_difference_lemma_counts,
           en_es_DEEP_noun_counts,
           en_es_DEEP_difference_noun_counts,
           en_es_DEEP_Prediction_Score,
           en_es_DEEP_genre_rarity,
           en_es_DEEP_to_en_difference_genre_rarity,
           en_es_DEEP_general_rarity,
           en_es_DEEP_to_en_difference_general_rarity,
           en_es_DEEP_verb_counts,
           en_es_DEEP_difference_verb_counts,
           en_es_DEEPL_Classification,
           en_es_DEEPL_lemma_counts,
           en_es_DEEPL_difference_lemma_counts,
           en_es_DEEPL_noun_counts,
           en_es_DEEPL_difference_noun_counts,
           en_es_DEEPL_Prediction_Score,
           en_es_DEEPL_genre_rarity,
           en_es_DEEPL_to_en_difference_genre_rarity,
           en_es_DEEPL_general_rarity,
           en_es_DEEPL_to_en_difference_general_rarity,
           en_es_DEEPL_verb_counts,
           en_es_DEEPL_difference_verb_counts,
           en_es_GOOGLE_Classification,
           en_es_GOOGLE_lemma_counts,
           en_es_GOOGLE_difference_lemma_counts,
           en_es_GOOGLE_noun_counts,
           en_es_GOOGLE_difference_noun_counts,
           en_es_GOOGLE_Prediction_Score,
           en_es_GOOGLE_genre_rarity,
           en_es_GOOGLE_to_en_difference_genre_rarity,
           en_es_GOOGLE_general_rarity,
           en_es_GOOGLE_to_en_difference_general_rarity,
           en_es_GOOGLE_verb_counts,
           en_es_GOOGLE_difference_verb_counts,
           en_es_TRANSFORMERS_Classification,
           en_es_TRANSFORMERS_lemma_counts,
           en_es_TRANSFORMERS_difference_lemma_counts,
           en_es_TRANSFORMERS_noun_counts,
           en_es_TRANSFORMERS_difference_noun_counts,
           en_es_TRANSFORMERS_Prediction_Score,
           en_es_TRANSFORMERS_genre_rarity,
           en_es_TRANSFORMERS_to_en_difference_genre_rarity,
           en_es_TRANSFORMERS_general_rarity,
           en_es_TRANSFORMERS_to_en_difference_general_rarity,
           en_es_TRANSFORMERS_verb_counts,
           en_es_TRANSFORMERS_difference_verb_counts,
           es_Classification,
           es_lemma_counts,
           es_noun_counts,
           es_Prediction_Score,
           es_genre_rarity,
           es_general_rarity,
           es_verb_counts,
           # new section
           ddm_es,
           ddm_en_es_DEEP,
           ddm_en_es_DEEPL,
           ddm_en_es_GOOGLE,
           ddm_en_es_TRANSFORMERS,
           diff_ddm_en_es_DEEP,
           diff_ddm_en_es_DEEPL,
           diff_ddm_en_es_GOOGLE,
           diff_ddm_en_es_TRANSFORMERS))










## AR by mt tool  ---------------

data.ar.native  <- data.pred %>%  
  select(c(ar_lemma_counts,
           ar_noun_counts,
           ar_Prediction_Score,
           ar_genre_rarity,
           ar_general_rarity,
           ar_verb_counts,
           ar_Classification,
           ddm_ar))


data.ar.Deep.from.en <- data.pred %>%  
  select(c(en_ar_DEEP_Classification,
           en_ar_DEEP_lemma_counts,
           en_ar_DEEP_difference_lemma_counts,
           en_ar_DEEP_noun_counts,
           en_ar_DEEP_difference_noun_counts,
           en_ar_DEEP_Prediction_Score,
           en_ar_DEEP_genre_rarity,
           en_ar_DEEP_to_en_difference_genre_rarity,
           en_ar_DEEP_general_rarity,
           en_ar_DEEP_to_en_difference_general_rarity,
           en_ar_DEEP_verb_counts,
           en_ar_DEEP_difference_verb_counts,
           ddm_en_ar_DEEP,
           diff_ddm_en_ar_DEEP))


data.ar.DeepL.from.en <- data.pred %>%  
  select(c(en_ar_DEEPL_Classification,
           en_ar_DEEPL_lemma_counts,
           en_ar_DEEPL_difference_lemma_counts,
           en_ar_DEEPL_noun_counts,
           en_ar_DEEPL_difference_noun_counts,
           en_ar_DEEPL_Prediction_Score,
           en_ar_DEEPL_genre_rarity,
           en_ar_DEEPL_to_en_difference_genre_rarity,
           en_ar_DEEPL_general_rarity,
           en_ar_DEEPL_to_en_difference_general_rarity,
           en_ar_DEEPL_verb_counts,
           en_ar_DEEPL_difference_verb_counts,
           ddm_en_ar_DEEPL,
           diff_ddm_en_ar_DEEPL))


data.ar.GT.from.en <- data.pred %>%  
  select(c(en_ar_GOOGLE_Classification,
           en_ar_GOOGLE_lemma_counts,
           en_ar_GOOGLE_difference_lemma_counts,
           en_ar_GOOGLE_noun_counts,
           en_ar_GOOGLE_difference_noun_counts,
           en_ar_GOOGLE_Prediction_Score,
           en_ar_GOOGLE_genre_rarity,
           en_ar_GOOGLE_to_en_difference_genre_rarity,
           en_ar_GOOGLE_general_rarity,
           en_ar_GOOGLE_to_en_difference_general_rarity,
           en_ar_GOOGLE_verb_counts,
           en_ar_GOOGLE_difference_verb_counts,
           ddm_en_ar_GOOGLE,
           diff_ddm_en_ar_GOOGLE))


data.ar.OPUS.from.en <- data.pred %>%  
  select(c(en_ar_TRANSFORMERS_Classification,
           en_ar_TRANSFORMERS_lemma_counts,
           en_ar_TRANSFORMERS_difference_lemma_counts,
           en_ar_TRANSFORMERS_noun_counts,
           en_ar_TRANSFORMERS_difference_noun_counts,
           en_ar_TRANSFORMERS_Prediction_Score,
           en_ar_TRANSFORMERS_genre_rarity,
           en_ar_TRANSFORMERS_to_en_difference_genre_rarity,
           en_ar_TRANSFORMERS_general_rarity,
           en_ar_TRANSFORMERS_to_en_difference_general_rarity,
           en_ar_TRANSFORMERS_verb_counts,
           en_ar_TRANSFORMERS_difference_verb_counts,
           ddm_en_ar_TRANSFORMERS,
           diff_ddm_en_ar_TRANSFORMERS))







## EN by mt tool  ---------------

data.en.Deep.from.ar  <- data.pred %>%  
  select(c(ar_en_DEEP_Classification,
           ar_en_DEEP_lemma_counts,
           ar_en_DEEP_difference_lemma_counts,
           ar_en_DEEP_noun_counts,
           ar_en_DEEP_difference_noun_counts,
           ar_en_DEEP_Prediction_Score,
           ar_en_DEEP_genre_rarity,
           ar_en_DEEP_to_ar_difference_genre_rarity,
           ar_en_DEEP_general_rarity,
           ar_en_DEEP_to_ar_difference_general_rarity,
           ar_en_DEEP_verb_counts,
           ar_en_DEEP_difference_verb_counts,
           ddm_ar_en_DEEP,
           diff_ddm_ar_en_DEEP))




data.en.Deep.from.es  <- data.pred %>%  
  select(c(es_en_DEEP_Classification,
           es_en_DEEP_lemma_counts,
           es_en_DEEP_difference_lemma_counts,
           es_en_DEEP_noun_counts,
           es_en_DEEP_difference_noun_counts,
           es_en_DEEP_Prediction_Score,
           es_en_DEEP_genre_rarity,
           es_en_DEEP_to_es_difference_genre_rarity,
           es_en_DEEP_general_rarity,
           es_en_DEEP_to_es_difference_general_rarity,
           es_en_DEEP_verb_counts,
           es_en_DEEP_difference_verb_counts,
           ddm_es_en_DEEP,
           diff_ddm_es_en_DEEP))


data.en.DeepL.from.ar  <- data.pred %>%  
  select(c(ar_en_DEEPL_Classification,
           ar_en_DEEPL_lemma_counts,
           ar_en_DEEPL_difference_lemma_counts,
           ar_en_DEEPL_noun_counts,
           ar_en_DEEPL_difference_noun_counts,
           ar_en_DEEPL_Prediction_Score,
           ar_en_DEEPL_genre_rarity,
           ar_en_DEEPL_to_ar_difference_genre_rarity,
           ar_en_DEEPL_general_rarity,
           ar_en_DEEPL_to_ar_difference_general_rarity,
           ar_en_DEEPL_verb_counts,
           ar_en_DEEPL_difference_verb_counts,
           ddm_ar_en_DEEPL,
           diff_ddm_ar_en_DEEPL))




data.en.DeepL.from.es  <- data.pred %>%  
  select(c(es_en_DEEPL_Classification,
           es_en_DEEPL_lemma_counts,
           es_en_DEEPL_difference_lemma_counts,
           es_en_DEEPL_noun_counts,
           es_en_DEEPL_difference_noun_counts,
           es_en_DEEPL_Prediction_Score,
           es_en_DEEPL_genre_rarity,
           es_en_DEEPL_to_es_difference_genre_rarity,
           es_en_DEEPL_general_rarity,
           es_en_DEEPL_to_es_difference_general_rarity,
           es_en_DEEPL_verb_counts,
           es_en_DEEPL_difference_verb_counts,
           ddm_es_en_DEEPL,
           diff_ddm_es_en_DEEPL))


data.en.GT.from.ar  <- data.pred %>%  
  select(c(ar_en_GOOGLE_Classification,
           ar_en_GOOGLE_lemma_counts,
           ar_en_GOOGLE_difference_lemma_counts,
           ar_en_GOOGLE_noun_counts,
           ar_en_GOOGLE_difference_noun_counts,
           ar_en_GOOGLE_Prediction_Score,
           ar_en_GOOGLE_genre_rarity,
           ar_en_GOOGLE_to_ar_difference_genre_rarity,
           ar_en_GOOGLE_general_rarity,
           ar_en_GOOGLE_to_ar_difference_general_rarity,
           ar_en_GOOGLE_verb_counts,
           ar_en_GOOGLE_difference_verb_counts,
           ddm_ar_en_GOOGLE,
           diff_ddm_ar_en_GOOGLE))




data.en.GT.from.es  <- data.pred %>%  
  select(c(es_en_GOOGLE_lemma_counts,
           es_en_GOOGLE_difference_lemma_counts,
           es_en_GOOGLE_noun_counts,
           es_en_GOOGLE_difference_noun_counts,
           es_en_GOOGLE_Prediction_Score,
           es_en_GOOGLE_genre_rarity,
           es_en_GOOGLE_to_es_difference_genre_rarity,
           es_en_GOOGLE_general_rarity,
           es_en_GOOGLE_to_es_difference_general_rarity,
           es_en_GOOGLE_verb_counts,
           es_en_GOOGLE_difference_verb_counts,
           es_en_GOOGLE_Classification,
           ddm_es_en_GOOGLE,
           diff_ddm_es_en_GOOGLE))


data.en.Native <- data.pred %>%  
  select(c(en_Classification,
           en_lemma_counts,
           en_noun_counts,
           en_Prediction_Score,
           en_genre_rarity,
           en_general_rarity,
           en_verb_counts,
           ddm_en))


data.en.OPUS.from.ar <- data.pred %>%  
  select(c(ar_en_TRANSFORMERS_Classification,
           ar_en_TRANSFORMERS_lemma_counts,
           ar_en_TRANSFORMERS_difference_lemma_counts,
           ar_en_TRANSFORMERS_noun_counts,
           ar_en_TRANSFORMERS_difference_noun_counts,
           ar_en_TRANSFORMERS_Prediction_Score,
           ar_en_TRANSFORMERS_genre_rarity,
           ar_en_TRANSFORMERS_to_ar_difference_genre_rarity,
           ar_en_TRANSFORMERS_general_rarity,
           ar_en_TRANSFORMERS_to_ar_difference_general_rarity,
           ar_en_TRANSFORMERS_verb_counts,
           ar_en_TRANSFORMERS_difference_verb_counts,
           ddm_ar_en_TRANSFORMERS,
           diff_ddm_ar_en_TRANSFORMERS))




data.en.OPUS.from.es <- data.pred %>%  
  select(c(es_en_TRANSOFMERS_Classification,
           es_en_TRANSFORMERS_lemma_counts,
           es_en_TRANSFORMERS_difference_lemma_counts,
           es_en_TRANSFORMERS_noun_counts,
           es_en_TRANSFORMERS_difference_noun_counts,
           es_en_TRANSOFMERS_Prediction_Score,
           es_en_TRANSFORMERS_genre_rarity,
           es_en_TRANSFORMERS_to_es_difference_genre_rarity,
           es_en_TRANSFORMERS_general_rarity,
           es_en_TRANSFORMERS_to_es_difference_general_rarity,
           es_en_TRANSFORMERS_verb_counts,
           es_en_TRANSFORMERS_difference_verb_counts,
           ddm_es_en_TRANSOFMERS,
           diff_ddm_es_en_TRANSOFMERS))





## ES by mt tool  ---------------

data.es.Deep.from.en  <- data.pred %>%  
  select(c(en_es_DEEP_Classification,
           en_es_DEEP_lemma_counts,
           en_es_DEEP_difference_lemma_counts,
           en_es_DEEP_noun_counts,
           en_es_DEEP_difference_noun_counts,
           en_es_DEEP_Prediction_Score,
           en_es_DEEP_genre_rarity,
           en_es_DEEP_to_en_difference_genre_rarity,
           en_es_DEEP_general_rarity,
           en_es_DEEP_to_en_difference_general_rarity,
           en_es_DEEP_verb_counts,
           en_es_DEEP_difference_verb_counts,
           ddm_en_es_DEEP,
           diff_ddm_en_es_DEEP))


data.es.DeepL.from.en  <- data.pred %>%  
  select(c(en_es_DEEPL_Classification,
           en_es_DEEPL_lemma_counts,
           en_es_DEEPL_difference_lemma_counts,
           en_es_DEEPL_noun_counts,
           en_es_DEEPL_difference_noun_counts,
           en_es_DEEPL_Prediction_Score,
           en_es_DEEPL_genre_rarity,
           en_es_DEEPL_to_en_difference_genre_rarity,
           en_es_DEEPL_general_rarity,
           en_es_DEEPL_to_en_difference_general_rarity,
           en_es_DEEPL_verb_counts,
           en_es_DEEPL_difference_verb_counts,
           ddm_en_es_DEEPL,
           diff_ddm_en_es_DEEPL))


data.es.GT.from.en  <- data.pred %>%  
  select(c(en_es_GOOGLE_Classification,
           en_es_GOOGLE_lemma_counts,
           en_es_GOOGLE_difference_lemma_counts,
           en_es_GOOGLE_noun_counts,
           en_es_GOOGLE_difference_noun_counts,
           en_es_GOOGLE_Prediction_Score,
           en_es_GOOGLE_genre_rarity,
           en_es_GOOGLE_to_en_difference_genre_rarity,
           en_es_GOOGLE_general_rarity,
           en_es_GOOGLE_to_en_difference_general_rarity,
           en_es_GOOGLE_verb_counts,
           en_es_GOOGLE_difference_verb_counts,
           ddm_en_es_GOOGLE,
           diff_ddm_en_es_GOOGLE))


data.es.Native  <- data.pred %>%  
  select(c(es_Classification,
           es_lemma_counts,
           es_noun_counts,
           es_Prediction_Score,
           es_genre_rarity,
           es_general_rarity,
           es_verb_counts,
           ddm_es))


data.es.OPUS.from.en  <- data.pred %>%  
  select(c(en_es_TRANSFORMERS_Classification,
           en_es_TRANSFORMERS_lemma_counts,
           en_es_TRANSFORMERS_difference_lemma_counts,
           en_es_TRANSFORMERS_noun_counts,
           en_es_TRANSFORMERS_difference_noun_counts,
           en_es_TRANSFORMERS_Prediction_Score,
           en_es_TRANSFORMERS_genre_rarity,
           en_es_TRANSFORMERS_to_en_difference_genre_rarity,
           en_es_TRANSFORMERS_general_rarity,
           en_es_TRANSFORMERS_to_en_difference_general_rarity,
           en_es_TRANSFORMERS_verb_counts,
           en_es_TRANSFORMERS_difference_verb_counts,
           ddm_en_es_TRANSFORMERS,
           diff_ddm_en_es_TRANSFORMERS))


#




















# RENAME VARIABLES BY DATA FRAME   -------------------------------------



## AR   ---------------



data.ar.native  <- data.ar.native %>%  
  rename(classification = ar_Classification) %>%
  rename(lemma = ar_lemma_counts) %>%
  rename(noun = ar_noun_counts) %>%
  rename(pred = ar_Prediction_Score) %>%
  rename(rarity.dom = ar_genre_rarity) %>%
  rename(rarity.gral = ar_general_rarity) %>%
  rename(verb = ar_verb_counts)  %>%
  rename(ddm = ddm_ar) 

names(data.ar.native)



data.ar.Deep.from.en <- data.ar.Deep.from.en %>%  
  rename(classification = en_ar_DEEP_Classification) %>%
  rename(lemma = en_ar_DEEP_lemma_counts) %>%
  rename(lemma.diff = en_ar_DEEP_difference_lemma_counts) %>%
  rename(noun = en_ar_DEEP_noun_counts) %>%
  rename(noun.diff = en_ar_DEEP_difference_noun_counts) %>%
  rename(pred = en_ar_DEEP_Prediction_Score) %>%
  rename(rarity.dom = en_ar_DEEP_genre_rarity) %>%
  rename(rarity.dom.diff = en_ar_DEEP_to_en_difference_genre_rarity) %>%
  rename(rarity.gral = en_ar_DEEP_general_rarity) %>%
  rename(rarity.gral.diff = en_ar_DEEP_to_en_difference_general_rarity) %>%
  rename(verb = en_ar_DEEP_verb_counts) %>%
  rename(verb.diff = en_ar_DEEP_difference_verb_counts)  %>%
  rename(ddm = ddm_en_ar_DEEP) %>%
  rename(ddm.diff = diff_ddm_en_ar_DEEP) 

names(data.ar.Deep.from.en)



data.ar.DeepL.from.en <- data.ar.DeepL.from.en %>%  
  rename(classification = en_ar_DEEPL_Classification) %>%
  rename(lemma = en_ar_DEEPL_lemma_counts) %>%
  rename(lemma.diff = en_ar_DEEPL_difference_lemma_counts) %>%
  rename(noun = en_ar_DEEPL_noun_counts) %>%
  rename(noun.diff = en_ar_DEEPL_difference_noun_counts) %>%
  rename(pred = en_ar_DEEPL_Prediction_Score) %>%
  rename(rarity.dom = en_ar_DEEPL_genre_rarity) %>%
  rename(rarity.dom.diff = en_ar_DEEPL_to_en_difference_genre_rarity) %>%
  rename(rarity.gral = en_ar_DEEPL_general_rarity) %>%
  rename(rarity.gral.diff = en_ar_DEEPL_to_en_difference_general_rarity) %>%
  rename(verb = en_ar_DEEPL_verb_counts) %>%
  rename(verb.diff = en_ar_DEEPL_difference_verb_counts)  %>%
  rename(ddm = ddm_en_ar_DEEPL)  %>%
  rename(ddm.diff = diff_ddm_en_ar_DEEPL)  

names(data.ar.DeepL.from.en)



data.ar.GT.from.en <- data.ar.GT.from.en %>%  
  rename(classification = en_ar_GOOGLE_Classification) %>%
  rename(lemma = en_ar_GOOGLE_lemma_counts) %>%
  rename(lemma.diff = en_ar_GOOGLE_difference_lemma_counts) %>%
  rename(noun = en_ar_GOOGLE_noun_counts) %>%
  rename(noun.diff = en_ar_GOOGLE_difference_noun_counts) %>%
  rename(pred = en_ar_GOOGLE_Prediction_Score) %>%
  rename(rarity.dom = en_ar_GOOGLE_genre_rarity) %>%
  rename(rarity.dom.diff = en_ar_GOOGLE_to_en_difference_genre_rarity) %>%
  rename(rarity.gral = en_ar_GOOGLE_general_rarity) %>%
  rename(rarity.gral.diff = en_ar_GOOGLE_to_en_difference_general_rarity) %>%
  rename(verb = en_ar_GOOGLE_verb_counts) %>%
  rename(verb.diff = en_ar_GOOGLE_difference_verb_counts) %>%
  rename(ddm = ddm_en_ar_GOOGLE) %>%
  rename(ddm.diff = diff_ddm_en_ar_GOOGLE)

names(data.ar.GT.from.en)



data.ar.OPUS.from.en <- data.ar.OPUS.from.en %>%  
  rename(classification = en_ar_TRANSFORMERS_Classification) %>%
  rename(lemma = en_ar_TRANSFORMERS_lemma_counts) %>%
  rename(lemma.diff = en_ar_TRANSFORMERS_difference_lemma_counts) %>%
  rename(noun = en_ar_TRANSFORMERS_noun_counts) %>%
  rename(noun.diff = en_ar_TRANSFORMERS_difference_noun_counts) %>%
  rename(pred = en_ar_TRANSFORMERS_Prediction_Score) %>%
  rename(rarity.dom = en_ar_TRANSFORMERS_genre_rarity) %>%
  rename(rarity.dom.diff = en_ar_TRANSFORMERS_to_en_difference_genre_rarity) %>%
  rename(rarity.gral = en_ar_TRANSFORMERS_general_rarity) %>%
  rename(rarity.gral.diff = en_ar_TRANSFORMERS_to_en_difference_general_rarity) %>%
  rename(verb = en_ar_TRANSFORMERS_verb_counts) %>%
  rename(verb.diff = en_ar_TRANSFORMERS_difference_verb_counts)  %>%
  rename(ddm = ddm_en_ar_TRANSFORMERS)  %>%
  rename(ddm.diff = diff_ddm_en_ar_TRANSFORMERS)  

names(data.ar.OPUS.from.en)



# Check names
names(data.ar.native)
names(data.ar.Deep.from.en)
names(data.ar.DeepL.from.en)
names(data.ar.GT.from.en)
names(data.ar.OPUS.from.en)







## EN    ---------------

data.en.Deep.from.ar  <- data.en.Deep.from.ar %>%  
  rename(classification = ar_en_DEEP_Classification) %>%
  rename(lemma = ar_en_DEEP_lemma_counts) %>%
  rename(lemma.diff = ar_en_DEEP_difference_lemma_counts) %>%
  rename(noun = ar_en_DEEP_noun_counts) %>%
  rename(noun.diff = ar_en_DEEP_difference_noun_counts) %>%
  rename(pred = ar_en_DEEP_Prediction_Score) %>%
  rename(rarity.dom = ar_en_DEEP_genre_rarity) %>%
  rename(rarity.dom.diff = ar_en_DEEP_to_ar_difference_genre_rarity) %>%
  rename(rarity.gral = ar_en_DEEP_general_rarity) %>%
  rename(rarity.gral.diff = ar_en_DEEP_to_ar_difference_general_rarity) %>%
  rename(verb = ar_en_DEEP_verb_counts) %>%
  rename(verb.diff = ar_en_DEEP_difference_verb_counts) %>%
  rename(ddm = ddm_ar_en_DEEP) %>%
  rename(ddm.diff = diff_ddm_ar_en_DEEP)  

names(data.en.Deep.from.ar)



data.en.Deep.from.es  <- data.en.Deep.from.es %>%  
  rename(classification = es_en_DEEP_Classification) %>%
  rename(lemma = es_en_DEEP_lemma_counts) %>%
  rename(lemma.diff = es_en_DEEP_difference_lemma_counts) %>%
  rename(noun = es_en_DEEP_noun_counts) %>%
  rename(noun.diff = es_en_DEEP_difference_noun_counts) %>%
  rename(pred = es_en_DEEP_Prediction_Score) %>%
  rename(rarity.dom = es_en_DEEP_genre_rarity) %>%
  rename(rarity.dom.diff = es_en_DEEP_to_es_difference_genre_rarity) %>%
  rename(rarity.gral = es_en_DEEP_general_rarity) %>%
  rename(rarity.gral.diff = es_en_DEEP_to_es_difference_general_rarity) %>%
  rename(verb = es_en_DEEP_verb_counts) %>%
  rename(verb.diff = es_en_DEEP_difference_verb_counts) %>%
  rename(ddm = ddm_es_en_DEEP) %>%
  rename(ddm.diff = diff_ddm_es_en_DEEP)  

names(data.en.Deep.from.es)




data.en.DeepL.from.ar  <- data.en.DeepL.from.ar %>%  
  rename(classification = ar_en_DEEPL_Classification) %>%
  rename(lemma = ar_en_DEEPL_lemma_counts) %>%
  rename(lemma.diff = ar_en_DEEPL_difference_lemma_counts) %>%
  rename(noun = ar_en_DEEPL_noun_counts) %>%
  rename(noun.diff = ar_en_DEEPL_difference_noun_counts) %>%
  rename(pred = ar_en_DEEPL_Prediction_Score) %>%
  rename(rarity.dom = ar_en_DEEPL_genre_rarity) %>%
  rename(rarity.dom.diff = ar_en_DEEPL_to_ar_difference_genre_rarity) %>%
  rename(rarity.gral = ar_en_DEEPL_general_rarity) %>%
  rename(rarity.gral.diff = ar_en_DEEPL_to_ar_difference_general_rarity) %>%
  rename(verb = ar_en_DEEPL_verb_counts) %>%
  rename(verb.diff = ar_en_DEEPL_difference_verb_counts)  %>%
  rename(ddm = ddm_ar_en_DEEPL)  %>%
  rename(ddm.diff = diff_ddm_ar_en_DEEPL)  

names(data.en.DeepL.from.ar)




data.en.DeepL.from.es  <- data.en.DeepL.from.es %>%  
  rename(classification = es_en_DEEPL_Classification) %>%
  rename(lemma = es_en_DEEPL_lemma_counts) %>%
  rename(lemma.diff = es_en_DEEPL_difference_lemma_counts) %>%
  rename(noun = es_en_DEEPL_noun_counts) %>%
  rename(noun.diff = es_en_DEEPL_difference_noun_counts) %>%
  rename(pred = es_en_DEEPL_Prediction_Score) %>%
  rename(rarity.dom = es_en_DEEPL_genre_rarity) %>%
  rename(rarity.dom.diff = es_en_DEEPL_to_es_difference_genre_rarity) %>%
  rename(rarity.gral = es_en_DEEPL_general_rarity) %>%
  rename(rarity.gral.diff = es_en_DEEPL_to_es_difference_general_rarity) %>%
  rename(verb = es_en_DEEPL_verb_counts) %>%
  rename(verb.diff = es_en_DEEPL_difference_verb_counts) %>%
  rename(ddm = ddm_es_en_DEEPL)   %>%
  rename(ddm.diff = diff_ddm_es_en_DEEPL)    

names(data.en.DeepL.from.es)




data.en.GT.from.ar  <- data.en.GT.from.ar %>%  
  rename(classification = ar_en_GOOGLE_Classification) %>%
  rename(lemma = ar_en_GOOGLE_lemma_counts) %>%
  rename(lemma.diff = ar_en_GOOGLE_difference_lemma_counts) %>%
  rename(noun = ar_en_GOOGLE_noun_counts) %>%
  rename(noun.diff = ar_en_GOOGLE_difference_noun_counts) %>%
  rename(pred = ar_en_GOOGLE_Prediction_Score) %>%
  rename(rarity.dom = ar_en_GOOGLE_genre_rarity) %>%
  rename(rarity.dom.diff = ar_en_GOOGLE_to_ar_difference_genre_rarity) %>%
  rename(rarity.gral = ar_en_GOOGLE_general_rarity) %>%
  rename(rarity.gral.diff = ar_en_GOOGLE_to_ar_difference_general_rarity) %>%
  rename(verb = ar_en_GOOGLE_verb_counts) %>%
  rename(verb.diff = ar_en_GOOGLE_difference_verb_counts)  %>%
  rename(ddm = ddm_ar_en_GOOGLE) %>%
  rename(ddm.diff = diff_ddm_ar_en_GOOGLE) 

names(data.en.GT.from.ar)




data.en.GT.from.es  <- data.en.GT.from.es%>%  
  rename(classification = es_en_GOOGLE_Classification) %>%
  rename(lemma = es_en_GOOGLE_lemma_counts) %>%
  rename(lemma.diff = es_en_GOOGLE_difference_lemma_counts) %>%
  rename(noun = es_en_GOOGLE_noun_counts) %>%
  rename(noun.diff = es_en_GOOGLE_difference_noun_counts) %>%
  rename(pred = es_en_GOOGLE_Prediction_Score) %>%
  rename(rarity.dom = es_en_GOOGLE_genre_rarity) %>%
  rename(rarity.dom.diff = es_en_GOOGLE_to_es_difference_genre_rarity) %>%
  rename(rarity.gral = es_en_GOOGLE_general_rarity) %>%
  rename(rarity.gral.diff = es_en_GOOGLE_to_es_difference_general_rarity) %>%
  rename(verb = es_en_GOOGLE_verb_counts) %>%
  rename(verb.diff = es_en_GOOGLE_difference_verb_counts)  %>%
  rename(ddm = ddm_es_en_GOOGLE)  %>%
  rename(ddm.diff = diff_ddm_es_en_GOOGLE)  

names(data.en.GT.from.es)




data.en.Native <- data.en.Native %>%  
  rename(classification = en_Classification) %>%
  rename(lemma = en_lemma_counts) %>%
  rename(noun = en_noun_counts) %>%
  rename(pred = en_Prediction_Score) %>%
  rename(rarity.dom = en_genre_rarity) %>%
  rename(rarity.gral = en_general_rarity) %>%
  rename(verb = en_verb_counts)  %>%
  rename(ddm = ddm_en)  

names(data.en.Native)




data.en.OPUS.from.ar <- data.en.OPUS.from.ar %>%  
  rename(classification = ar_en_TRANSFORMERS_Classification) %>%
  rename(lemma = ar_en_TRANSFORMERS_lemma_counts) %>%
  rename(lemma.diff = ar_en_TRANSFORMERS_difference_lemma_counts) %>%
  rename(noun = ar_en_TRANSFORMERS_noun_counts) %>%
  rename(noun.diff = ar_en_TRANSFORMERS_difference_noun_counts) %>%
  rename(pred = ar_en_TRANSFORMERS_Prediction_Score) %>%
  rename(rarity.dom = ar_en_TRANSFORMERS_genre_rarity) %>%
  rename(rarity.dom.diff = ar_en_TRANSFORMERS_to_ar_difference_genre_rarity) %>%
  rename(rarity.gral = ar_en_TRANSFORMERS_general_rarity) %>%
  rename(rarity.gral.diff = ar_en_TRANSFORMERS_to_ar_difference_general_rarity) %>%
  rename(verb = ar_en_TRANSFORMERS_verb_counts) %>%
  rename(verb.diff = ar_en_TRANSFORMERS_difference_verb_counts) %>%
  rename(ddm = ddm_ar_en_TRANSFORMERS)  %>%
  rename(ddm.diff = diff_ddm_ar_en_TRANSFORMERS)   

names(data.en.OPUS.from.ar)




data.en.OPUS.from.es <- data.en.OPUS.from.es %>%  
  rename(classification = es_en_TRANSOFMERS_Classification) %>%
  rename(lemma = es_en_TRANSFORMERS_lemma_counts) %>%
  rename(lemma.diff = es_en_TRANSFORMERS_difference_lemma_counts) %>%
  rename(noun = es_en_TRANSFORMERS_noun_counts) %>%
  rename(noun.diff = es_en_TRANSFORMERS_difference_noun_counts) %>%
  rename(pred = es_en_TRANSOFMERS_Prediction_Score) %>%
  rename(rarity.dom = es_en_TRANSFORMERS_genre_rarity) %>%
  rename(rarity.dom.diff = es_en_TRANSFORMERS_to_es_difference_genre_rarity) %>%
  rename(rarity.gral = es_en_TRANSFORMERS_general_rarity) %>%
  rename(rarity.gral.diff = es_en_TRANSFORMERS_to_es_difference_general_rarity) %>%
  rename(verb = es_en_TRANSFORMERS_verb_counts) %>%
  rename(verb.diff = es_en_TRANSFORMERS_difference_verb_counts)  %>%
  rename(ddm = ddm_es_en_TRANSOFMERS) %>%
  rename(ddm.diff = diff_ddm_es_en_TRANSOFMERS) 

names(data.en.OPUS.from.es)




## ES    ---------------

data.es.Deep.from.en  <- data.es.Deep.from.en %>%  
  rename(classification = en_es_DEEP_Classification) %>%
  rename(lemma = en_es_DEEP_lemma_counts) %>%
  rename(lemma.diff = en_es_DEEP_difference_lemma_counts) %>%
  rename(noun = en_es_DEEP_noun_counts) %>%
  rename(noun.diff = en_es_DEEP_difference_noun_counts) %>%
  rename(pred = en_es_DEEP_Prediction_Score) %>%
  rename(rarity.dom = en_es_DEEP_genre_rarity) %>%
  rename(rarity.dom.diff = en_es_DEEP_to_en_difference_genre_rarity) %>%
  rename(rarity.gral = en_es_DEEP_general_rarity) %>%
  rename(rarity.gral.diff = en_es_DEEP_to_en_difference_general_rarity) %>%
  rename(verb = en_es_DEEP_verb_counts) %>%
  rename(verb.diff = en_es_DEEP_difference_verb_counts)  %>%
  rename(ddm = ddm_en_es_DEEP) %>%
  rename(ddm.diff = diff_ddm_en_es_DEEP) 

names(data.es.Deep.from.en)




data.es.DeepL.from.en  <- data.es.DeepL.from.en %>%  
  rename(classification = en_es_DEEPL_Classification) %>%
  rename(lemma = en_es_DEEPL_lemma_counts) %>%
  rename(lemma.diff = en_es_DEEPL_difference_lemma_counts) %>%
  rename(noun = en_es_DEEPL_noun_counts) %>%
  rename(noun.diff = en_es_DEEPL_difference_noun_counts) %>%
  rename(pred = en_es_DEEPL_Prediction_Score) %>%
  rename(rarity.dom = en_es_DEEPL_genre_rarity) %>%
  rename(rarity.dom.diff = en_es_DEEPL_to_en_difference_genre_rarity) %>%
  rename(rarity.gral = en_es_DEEPL_general_rarity) %>%
  rename(rarity.gral.diff = en_es_DEEPL_to_en_difference_general_rarity) %>%
  rename(verb = en_es_DEEPL_verb_counts) %>%
  rename(verb.diff = en_es_DEEPL_difference_verb_counts)   %>%
  rename(ddm = ddm_en_es_DEEPL)  %>%
  rename(ddm.diff = diff_ddm_en_es_DEEPL) 

names(data.es.DeepL.from.en)




data.es.GT.from.en  <- data.es.GT.from.en %>%  
  rename(classification = en_es_GOOGLE_Classification) %>%
  rename(lemma = en_es_GOOGLE_lemma_counts) %>%
  rename(lemma.diff = en_es_GOOGLE_difference_lemma_counts) %>%
  rename(noun = en_es_GOOGLE_noun_counts) %>%
  rename(noun.diff = en_es_GOOGLE_difference_noun_counts) %>%
  rename(pred = en_es_GOOGLE_Prediction_Score) %>%
  rename(rarity.dom = en_es_GOOGLE_genre_rarity) %>%
  rename(rarity.dom.diff = en_es_GOOGLE_to_en_difference_genre_rarity) %>%
  rename(rarity.gral = en_es_GOOGLE_general_rarity) %>%
  rename(rarity.gral.diff = en_es_GOOGLE_to_en_difference_general_rarity) %>%
  rename(verb = en_es_GOOGLE_verb_counts) %>%
  rename(verb.diff = en_es_GOOGLE_difference_verb_counts)  %>%
  rename(ddm = ddm_en_es_GOOGLE) %>%
  rename(ddm.diff = diff_ddm_en_es_GOOGLE) 

names(data.es.GT.from.en)




data.es.Native  <- data.es.Native %>%  
  rename(classification = es_Classification) %>%
  rename(lemma = es_lemma_counts) %>%
  rename(noun = es_noun_counts) %>%
  rename(pred = es_Prediction_Score) %>%
  rename(rarity.dom = es_genre_rarity) %>%
  rename(rarity.gral = es_general_rarity) %>%
  rename(verb = es_verb_counts)   %>%
  rename(ddm = ddm_es)  

names(data.es.Native)




data.es.OPUS.from.en  <- data.es.OPUS.from.en %>%  
  rename(classification = en_es_TRANSFORMERS_Classification) %>%
  rename(lemma = en_es_TRANSFORMERS_lemma_counts) %>%
  rename(lemma.diff = en_es_TRANSFORMERS_difference_lemma_counts) %>%
  rename(noun = en_es_TRANSFORMERS_noun_counts) %>%
  rename(noun.diff = en_es_TRANSFORMERS_difference_noun_counts) %>%
  rename(pred = en_es_TRANSFORMERS_Prediction_Score) %>%
  rename(rarity.dom = en_es_TRANSFORMERS_genre_rarity) %>%
  rename(rarity.dom.diff = en_es_TRANSFORMERS_to_en_difference_genre_rarity) %>%
  rename(rarity.gral = en_es_TRANSFORMERS_general_rarity) %>%
  rename(rarity.gral.diff = en_es_TRANSFORMERS_to_en_difference_general_rarity) %>%
  rename(verb = en_es_TRANSFORMERS_verb_counts) %>%
  rename(verb.diff = en_es_TRANSFORMERS_difference_verb_counts)  %>%
  rename(ddm = ddm_en_es_TRANSFORMERS) %>%
  rename(ddm.diff = diff_ddm_en_es_TRANSFORMERS) 

names(data.es.OPUS.from.en)





# ASSIGN DF VARIABLES  -------------------------------------



data.ar.Deep.from.en <- data.ar.Deep.from.en %>%
  mutate(lang = "AR") %>%
  mutate(trans.lang = "EN to AR") %>%
  mutate(mt.tool = "Deep")

data.ar.DeepL.from.en <- data.ar.DeepL.from.en %>%
  mutate(lang = "AR") %>%
  mutate(trans.lang = "EN to AR") %>%
  mutate(mt.tool = "DeepL")

data.ar.GT.from.en <- data.ar.GT.from.en %>%
  mutate(lang = "AR") %>%
  mutate(trans.lang = "EN to AR") %>%
  mutate(mt.tool = "GT")

data.ar.OPUS.from.en <- data.ar.OPUS.from.en %>%
  mutate(lang = "AR") %>%
  mutate(trans.lang = "EN to AR") %>%
  mutate(mt.tool = "OPUS")

data.ar.native <- data.ar.native %>%
  mutate(lang = "AR") %>%
  mutate(trans.lang = "Native") %>%
  mutate(mt.tool = "Native")

data.en.Deep.from.ar <- data.en.Deep.from.ar %>%
  mutate(lang = "EN") %>%
  mutate(trans.lang = "AR to EN") %>%
  mutate(mt.tool = "Deep")

data.en.Deep.from.es <- data.en.Deep.from.es %>%
  mutate(lang = "EN") %>%
  mutate(trans.lang = "ES to EN") %>%
  mutate(mt.tool = "Deep")

data.en.DeepL.from.ar <- data.en.DeepL.from.ar %>%
  mutate(lang = "EN") %>%
  mutate(trans.lang = "AR to EN") %>%
  mutate(mt.tool = "DeepL")

data.en.DeepL.from.es <- data.en.DeepL.from.es %>%
  mutate(lang = "EN") %>%
  mutate(trans.lang = "ES to EN") %>%
  mutate(mt.tool = "DeepL")

data.en.GT.from.ar <- data.en.GT.from.ar %>%
  mutate(lang = "EN") %>%
  mutate(trans.lang = "AR to EN") %>%
  mutate(mt.tool = "GT")

data.en.GT.from.es <- data.en.GT.from.es %>%
  mutate(lang = "EN") %>%
  mutate(trans.lang = "ES to EN") %>%
  mutate(mt.tool = "GT")

data.en.Native <- data.en.Native %>%
  mutate(lang = "EN") %>%
  mutate(trans.lang = "Native") %>%
  mutate(mt.tool = "Native")

data.en.OPUS.from.ar <- data.en.OPUS.from.ar %>%
  mutate(lang = "EN") %>%
  mutate(trans.lang = "AR to EN") %>%
  mutate(mt.tool = "OPUS")

data.en.OPUS.from.es <- data.en.OPUS.from.es %>%
  mutate(lang = "EN") %>%
  mutate(trans.lang = "ES to EN") %>%
  mutate(mt.tool = "OPUS")

data.es.Deep.from.en <- data.es.Deep.from.en %>%
  mutate(lang = "ES") %>%
  mutate(trans.lang = "EN to ES") %>%
  mutate(mt.tool = "Deep")

data.es.DeepL.from.en <- data.es.DeepL.from.en %>%
  mutate(lang = "ES") %>%
  mutate(trans.lang = "EN to ES") %>%
  mutate(mt.tool = "DeepL")

data.es.GT.from.en <- data.es.GT.from.en %>%
  mutate(lang = "ES") %>%
  mutate(trans.lang = "EN to ES") %>%
  mutate(mt.tool = "GT")

data.es.Native <- data.es.Native %>%
  mutate(lang = "ES") %>%
  mutate(trans.lang = "Native") %>%
  mutate(mt.tool = "Native")

data.es.OPUS.from.en <- data.es.OPUS.from.en %>%
  mutate(lang = "ES") %>%
  mutate(trans.lang = "EN to ES") %>%
  mutate(mt.tool = "OPUS")






# MERGE THE DATA  -------------------------------------


## Index dist and all other data frames  

#data.ar.Deep.from.en <- data.ar.Deep.from.en %>% mutate(id = row_number())
#data.ar.DeepL.from.en <- data.ar.DeepL.from.en %>% mutate(id = row_number())
#data.ar.GT.from.en <- data.ar.GT.from.en %>% mutate(id = row_number())
#data.ar.OPUS.from.en <- data.ar.OPUS.from.en %>% mutate(id = row_number())
#data.en.Deep.from.ar <- data.en.Deep.from.ar %>% mutate(id = row_number())
#data.en.Deep.from.es <- data.en.Deep.from.es %>% mutate(id = row_number())
#data.en.DeepL.from.ar <- data.en.DeepL.from.ar %>% mutate(id = row_number())
#data.en.DeepL.from.es <- data.en.DeepL.from.es %>% mutate(id = row_number())
#data.en.GT.from.ar <- data.en.GT.from.ar %>% mutate(id = row_number())
#data.en.GT.from.es <- data.en.GT.from.es %>% mutate(id = row_number())
#data.en.OPUS.from.ar <- data.en.OPUS.from.ar %>% mutate(id = row_number())
#data.en.OPUS.from.es <- data.en.OPUS.from.es %>% mutate(id = row_number())
#data.es.Deep.from.en <- data.es.Deep.from.en %>% mutate(id = row_number())
#data.es.DeepL.from.en <- data.es.DeepL.from.en %>% mutate(id = row_number())
#data.es.GT.from.en <- data.es.GT.from.en %>% mutate(id = row_number())
#data.es.OPUS.from.en <- data.es.OPUS.from.en %>% mutate(id = row_number())

#dist.ar.Deep.from.en   <- dist.ar.Deep.from.en   %>% mutate(id = row_number()) %>% select(c(dist,id))
#dist.ar.DeepL.from.en   <- dist.ar.DeepL.from.en   %>% mutate(id = row_number()) %>% select(c(dist,id))
#dist.ar.GT.from.en  <- dist.ar.GT.from.en  %>% mutate(id = row_number()) %>% select(c(dist,id))
#dist.ar.OPUS.from.en  <- dist.ar.OPUS.from.en  %>% mutate(id = row_number()) %>% select(c(dist,id))
#dist.en.Deep.from.ar  <- dist.en.Deep.from.ar  %>% mutate(id = row_number()) %>% select(c(dist,id))
#dist.en.Deep.from.es  <- dist.en.Deep.from.es  %>% mutate(id = row_number()) %>% select(c(dist,id))
#dist.en.DeepL.from.ar  <- dist.en.DeepL.from.ar  %>% mutate(id = row_number()) %>% select(c(dist,id))
#dist.en.DeepL.from.es  <- dist.en.DeepL.from.es  %>% mutate(id = row_number()) %>% select(c(dist,id))
#dist.en.GT.from.ar  <- dist.en.GT.from.ar  %>% mutate(id = row_number()) %>% select(c(dist,id))
#dist.en.GT.from.es  <- dist.en.GT.from.es  %>% mutate(id = row_number()) %>% select(c(dist,id))
#dist.en.OPUS.from.ar  <- dist.en.OPUS.from.ar  %>% mutate(id = row_number()) %>% select(c(dist,id))
#dist.en.OPUS.from.es  <- dist.en.OPUS.from.es  %>% mutate(id = row_number()) %>% select(c(dist,id))
#dist.es.Deep.from.en  <- dist.es.Deep.from.en  %>% mutate(id = row_number()) %>% select(c(dist,id))
#dist.es.DeepL.from.en  <- dist.es.DeepL.from.en  %>% mutate(id = row_number()) %>% select(c(dist,id))
#dist.es.GT.from.en  <- dist.es.GT.from.en  %>% mutate(id = row_number()) %>% select(c(dist,id))
#dist.es.OPUS.from.en  <- dist.es.OPUS.from.en  %>% mutate(id = row_number()) %>% select(c(dist,id))



## Merge dist and all other data frames  

#data.dist.ar.Deep.from.en <- left_join(data.ar.Deep.from.en, dist.ar.Deep.from.en , by="id") %>% select(-id) 
#data.dist.ar.DeepL.from.en <- left_join(data.ar.DeepL.from.en, dist.ar.DeepL.from.en , by="id") %>% select(-id) 
#data.dist.ar.GT.from.en <- left_join(data.ar.GT.from.en, dist.ar.GT.from.en , by="id") %>% select(-id)
#data.dist.ar.OPUS.from.en <- left_join(data.ar.OPUS.from.en, dist.ar.OPUS.from.en , by="id") %>% select(-id)
#data.dist.en.Deep.from.ar <- left_join(data.en.Deep.from.ar, dist.en.Deep.from.ar , by="id") %>% select(-id)
#data.dist.en.Deep.from.es <- left_join(data.en.Deep.from.es, dist.en.Deep.from.es , by="id") %>% select(-id)
#data.dist.en.DeepL.from.ar <- left_join(data.en.DeepL.from.ar, dist.en.DeepL.from.ar , by="id") %>% select(-id)
#data.dist.en.DeepL.from.es <- left_join(data.en.DeepL.from.es, dist.en.DeepL.from.es , by="id") %>% select(-id)
#data.dist.en.GT.from.ar <- left_join(data.en.GT.from.ar, dist.en.GT.from.ar , by="id") %>% select(-id)
#data.dist.en.GT.from.es <- left_join(data.en.GT.from.es, dist.en.GT.from.es , by="id") %>% select(-id)
#data.dist.en.OPUS.from.ar <- left_join(data.en.OPUS.from.ar, dist.en.OPUS.from.ar , by="id") %>% select(-id)
#data.dist.en.OPUS.from.es <- left_join(data.en.OPUS.from.es, dist.en.OPUS.from.es , by="id") %>% select(-id)
#data.dist.es.Deep.from.en <- left_join(data.es.Deep.from.en, dist.es.Deep.from.en , by="id") %>% select(-id)
#data.dist.es.DeepL.from.en <- left_join(data.es.DeepL.from.en, dist.es.DeepL.from.en , by="id") %>% select(-id)
#data.dist.es.GT.from.en <- left_join(data.es.GT.from.en, dist.es.GT.from.en , by="id") %>% select(-id)
#data.dist.es.OPUS.from.en <- left_join(data.es.OPUS.from.en, dist.es.OPUS.from.en , by="id") %>% select(-id)





## Merge data frames  

data.pred.merged <- bind_rows(data.ar.Deep.from.en ,
                              data.ar.DeepL.from.en ,
                              data.ar.GT.from.en ,
                              data.ar.OPUS.from.en , 
                              data.en.Deep.from.ar ,
                              data.en.Deep.from.es ,
                              data.en.DeepL.from.ar ,
                              data.en.DeepL.from.es ,
                              data.en.GT.from.ar ,
                              data.en.GT.from.es ,
                              data.en.OPUS.from.ar , 
                              data.en.OPUS.from.es ,
                              data.es.Deep.from.en ,
                              data.es.DeepL.from.en ,
                              data.es.GT.from.en ,
                              data.es.OPUS.from.en ,
                              data.ar.native ,  
                              data.en.Native ,  
                              data.es.Native) 

# Merge dist data
names(data.pred.merged)







# STANDARDIZE VARIABLES FOR REG  -------------------------------------

# Get hames
names(data.pred.merged)

# Standardize measures from 0-1
summary(data.pred.merged)
summary(data.pred.merged$lemma)
summary(data.pred.merged$verb)
summary(data.pred.merged$noun)
summary(data.pred.merged$ddm)
summary(data.pred.merged$ddm.diff)

# Check ddm.diff NAs by group
check.na <- data.pred.merged %>%
  group_by(lang) %>% 
  summarise(sum_na = sum(is.na(ddm.diff)))
check.na     # OK, NAs correspond to native langs


# Get maximum values
data.pred.merged <- data.pred.merged %>%
  mutate(lemma.max=max(lemma, na.rm=TRUE)) %>%
  mutate(verb.max=max(verb, na.rm=TRUE)) %>%
  mutate(noun.max=max(noun, na.rm=TRUE)) %>%
  mutate(ddm.max=max(ddm, na.rm=TRUE)) 

# Standardize values
data.pred.merged <- data.pred.merged %>%
  mutate(lemma.std=(lemma/lemma.max)) %>%
  mutate(verb.std=(verb/verb.max)) %>%
  mutate(noun.std=(noun/noun.max)) %>%
  mutate(ddm.std=(ddm/ddm.max)) 



# We have dd.diff differently due to its positive and netagie vavlues

# explore the data
max(data.pred.merged$ddm.diff, na.rm=TRUE)
min(data.pred.merged$ddm.diff, na.rm=TRUE)
hist(data.pred.merged$ddm.diff)

# Get max and min for dd.diff
data.pred.merged <- data.pred.merged %>%
  mutate(ddm.diff.max=max(ddm.diff, na.rm=TRUE)) %>%
  mutate(ddm.diff.min=min(ddm.diff, na.rm=TRUE))

# Standardize dd.diff values from -1 to 1
data.pred.merged <- data.pred.merged %>%
  mutate(ddm.diff.std = case_when(
    ddm.diff>0 ~ (ddm.diff/ddm.diff.max), 
    ddm.diff<=0 ~ (ddm.diff/ddm.diff.min),
    TRUE ~ NA ))




# Check distribution
summary(data.pred.merged$lemma.std)
summary(data.pred.merged$verb.std)
summary(data.pred.merged$noun.std)
summary(data.pred.merged$ddm.std)
summary(data.pred.merged$ddm.diff.std)



# SAVE THE DATA --------------------------------------------------

write.csv(data.pred.merged, 'results/prediction_merged_Data_Master_v2.csv')








# End of script