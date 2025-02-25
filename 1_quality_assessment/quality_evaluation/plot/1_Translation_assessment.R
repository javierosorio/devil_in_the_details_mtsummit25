#####################################################################
# UN Multilingual Corpus
# Translation Assessment
# Javier Osorio 
# 8-19-2024
#####################################################################



# SETUP --------------------------------------------------

# Load all packages here

if (!require("pacman")) install.packages("pacman")
pacman::p_load(here, glue, openxlsx, tidyverse, readxl, dplyr, stringr, stringi, 
               ggplot2, grid)


# GET THE DATA #####################################################################


## Get the master data to identify excluded NA rows #############################

# Get the data
data.na <- read_excel("data/raw_data/data_master_text.xlsx")  


# Get rows with NA
data.na <- data.na[!complete.cases(data.na),]
data.na <- data.na %>% select(id)


## Get quality data #############################

# Get the data
data.qual <- read_excel('master_quality_data_ID_COMIT_Updated.xlsx')

# Generate id variable
data.qual <- data.qual %>%
  mutate(id = Sentence_ID )

# Exclude rows with NAs
data.qual <- anti_join(data.qual, data.na, by = "id")


# Explore the data
names(data.qual)

table(data.qual$Translation_Tool)
table(data.qual$Translation_Language)


# rename values
data.qual <- data.qual %>%
  mutate(Translation_Language=case_when(
    Translation_Language=="ar_en_DEEP" ~ "AR to EN",
    Translation_Language=="ar_en_DEEPL" ~ "AR to EN",
    Translation_Language=="ar_en_GOOGLE" ~ "AR to EN",
    Translation_Language=="ar_en_TRANSFORMERS" ~ "AR to EN",
    Translation_Language=="en_ar_DEEP" ~ "EN to AR",
    Translation_Language=="en_ar_DEEPL" ~ "EN to AR",
    Translation_Language=="en_ar_GOOGLE" ~ "EN to AR",
    Translation_Language=="en_ar_TRANSFORMERS" ~ "EN to AR",
    Translation_Language=="en_es_DEEP" ~ "EN to ES",
    Translation_Language=="en_es_DEEPL" ~ "EN to ES",
    Translation_Language=="en_es_GOOGLE" ~ "EN to ES",
    Translation_Language=="en_es_TRANSFORMERS" ~ "EN to ES",
    Translation_Language=="es_en_DEEP" ~ "ES to EN",
    Translation_Language=="es_en_DEEPL" ~ "ES to EN",
    Translation_Language=="es_en_GOOGLE" ~ "ES to EN",
    Translation_Language=="es_en_TRANSOFMERS" ~ "ES to EN"
  ))


data.qual <- data.qual %>%
  mutate(Translation_Tool=case_when(
    Translation_Tool=="Google_API" ~ "GT",
    Translation_Tool=="Transformers" ~ "OPUS",
    TRUE ~ Translation_Tool
  ))


table(data.qual$Translation_Tool)
table(data.qual$Translation_Language)



# CALCULATE MEANS #####################################################################

##  Calculate means ###########################
data.qual.mean <- data.qual %>%
  group_by(Translation_Tool,Translation_Language) %>%
  summarise(BLEU = mean(BLEU),
            SacreBLEU = mean(SacreBLEU),
            METEOR = mean(METEOR),
            BERTScore = mean(BERTScore),
            COMIT = mean(COMIT)) 
  




# Pivot form wide to long
data.qual.mean <- data.qual.mean %>%
  pivot_longer(cols=c("BLEU","SacreBLEU","METEOR","COMIT","BERTScore"),
               names_to="metric",
               values_to="values")


# Rename metrics to order them
data.qual.mean <- data.qual.mean %>%
  mutate(metric=case_when(
    metric=="BLEU" ~ "(a) BLEU",
    metric=="SacreBLEU" ~ "(b) SacreBLEU",
    metric=="METEOR" ~  "(c) METEOR",
    metric=="COMIT" ~ "(d) COMET",
    metric=="BERTScore" ~ "(e) BERTScore"  
  ))



##  Plot heat maps ###########################

ggplot(data.qual.mean, aes(x=Translation_Tool, 
                           y = reorder(metric, desc(metric)), 
                           fill= values)) + 
  geom_tile() + 
  #facet_wrap(~Translation_Language) +
  facet_wrap(~factor(Translation_Language, levels=c('AR to EN','EN to AR','ES to EN','EN to ES'))) +
  geom_text(aes(label=round(values, 2)),color="white")  +
  theme(axis.text.x = element_text(angle=90, vjust=.5, hjust=1)) + 
  xlab("Translation Tools") + ylab("Quality Metric")  +
  theme(legend.position="bottom") 

ggsave("graphs/quality_scores.pdf", width = 5, height = 5.5, units = "in")



# END OF SCRIPT