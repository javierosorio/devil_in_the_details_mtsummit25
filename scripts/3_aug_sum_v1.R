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



# GET THE DATA --------------------------------------------------

# Get the data (clean version)

data.diff <- read_excel("data/raw_data/data_master_text_clean_2.xlsx")  


names(data.diff)


# Eliminate classification variables
data.diff <- data.diff %>%
  select(-c(MatConf, MatCoop, VerConf, VerCoop, Relevant,QuadClass))
  
names(data.diff)




# CALCULATE LENGTH --------------------------------------------------


##  Count words  -------------------

data.diff <- data.diff %>%
  # Native text
  mutate(en.words = str_count(en, '\\w+')) %>%
  mutate(es.words = str_count(es, '\\w+')) %>%
  mutate(ar.words = str_count(ar, '\\w+')) %>%
  mutate(es.en.DEEP.words = str_count(es_en_DEEP, '\\w+')) %>%
  mutate(es.en.DEEPL.words = str_count(es_en_DEEPL, '\\w+')) %>%
  mutate(es.en.GOOGLE.words = str_count(es_en_GOOGLE, '\\w+')) %>%
  mutate(es.en.TRANSOFMERS.words = str_count(es_en_TRANSOFMERS, '\\w+')) %>%
  mutate(ar.en.DEEP.words = str_count(ar_en_DEEP, '\\w+')) %>%
  mutate(ar.en.DEEPL.words = str_count(ar_en_DEEPL, '\\w+')) %>%
  mutate(ar.en.GOOGLE.words = str_count(ar_en_GOOGLE, '\\w+')) %>%
  mutate(ar.en.TRANSFORMERS.words = str_count(ar_en_TRANSFORMERS, '\\w+')) %>%
  mutate(en.es.DEEP.words = str_count(en_es_DEEP, '\\w+')) %>%
  mutate(en.es.DEEPL.words = str_count(en_es_DEEPL, '\\w+')) %>%
  mutate(en.es.GOOGLE.words = str_count(en_es_GOOGLE, '\\w+')) %>%
  mutate(en.es.TRANSFORMERS.words = str_count(en_es_TRANSFORMERS, '\\w+')) %>%
  mutate(en.ar.DEEP.words = str_count(en_ar_DEEP, '\\w+')) %>%
  mutate(en.ar.DEEPL.words = str_count(en_ar_DEEPL, '\\w+')) %>%
  mutate(en.ar.GOOGLE.words = str_count(en_ar_GOOGLE, '\\w+')) %>%
  mutate(en.ar.TRANSFORMERS.words = str_count(en_ar_TRANSFORMERS, '\\w+')) 






## Difference words  -------------------

data.diff <-  data.diff %>%
  # Diff ES to ES
  mutate(diff.es.en.DEEP=es.en.DEEP.words-es.words) %>%
  mutate(diff.es.en.DEEPL=es.en.DEEPL.words-es.words) %>%
  mutate(diff.es.en.GOOGLE=es.en.GOOGLE.words-es.words) %>%
  mutate(diff.es.en.TRANSOFMERS=es.en.TRANSOFMERS.words-es.words) %>%
  # Diff AR to EN
  mutate(diff.ar.en.DEEP=ar.en.DEEP.words-ar.words) %>%
  mutate(diff.ar.en.DEEPL=ar.en.DEEPL.words-ar.words) %>%
  mutate(diff.ar.en.GOOGLE=ar.en.GOOGLE.words-ar.words) %>%
  mutate(diff.ar.en.TRANSFORMERS=ar.en.TRANSFORMERS.words-ar.words) %>%
  # Diff EN to ES
  mutate(diff.en.es.DEEP=en.es.DEEP.words-en.words) %>%
  mutate(diff.en.es.DEEPL=en.es.DEEPL.words-en.words) %>%
  mutate(diff.en.es.GOOGLE=en.es.GOOGLE.words-en.words) %>%
  mutate(diff.en.es.TRANSFORMERS=en.es.TRANSFORMERS.words-en.words) %>%
  # Diff EN to AR
  mutate(diff.en.ar.DEEP=en.ar.DEEP.words-en.words) %>%
  mutate(diff.en.ar.DEEPL=en.ar.DEEPL.words-en.words) %>%
  mutate(diff.en.ar.GOOGLE=en.ar.GOOGLE.words-en.words) %>%
  mutate(diff.en.ar.TRANSFORMERS=en.ar.TRANSFORMERS.words-en.words) 
  


  


## Calculate total words by language  -------------------

total.es.en.DEEP<- sum(data.diff[, 'diff.es.en.DEEP'], na.rm = TRUE)
total.es.en.DEEPL<- sum(data.diff[, 'diff.es.en.DEEPL'], na.rm = TRUE)
total.es.en.GOOGLE<- sum(data.diff[, 'diff.es.en.GOOGLE'], na.rm = TRUE)
total.es.en.TRANSOFMERS<- sum(data.diff[, 'diff.es.en.TRANSOFMERS'], na.rm = TRUE)
total.ar.en.DEEP<- sum(data.diff[, 'diff.ar.en.DEEP'], na.rm = TRUE)
total.ar.en.DEEPL<- sum(data.diff[, 'diff.ar.en.DEEPL'], na.rm = TRUE)
total.ar.en.GOOGLE<- sum(data.diff[, 'diff.ar.en.GOOGLE'], na.rm = TRUE)
total.ar.en.TRANSFORMERS<- sum(data.diff[, 'diff.ar.en.TRANSFORMERS'], na.rm = TRUE)
total.en.es.DEEP<- sum(data.diff[, 'diff.en.es.DEEP'], na.rm = TRUE)
total.en.es.DEEPL<- sum(data.diff[, 'diff.en.es.DEEPL'], na.rm = TRUE)
total.en.es.GOOGLE<- sum(data.diff[, 'diff.en.es.GOOGLE'], na.rm = TRUE)
total.en.es.TRANSFORMERS<- sum(data.diff[, 'diff.en.es.TRANSFORMERS'], na.rm = TRUE)
total.en.ar.DEEP<- sum(data.diff[, 'diff.en.ar.DEEP'], na.rm = TRUE)
total.en.ar.DEEPL<- sum(data.diff[, 'diff.en.ar.DEEPL'], na.rm = TRUE)
total.en.ar.GOOGLE<- sum(data.diff[, 'diff.en.ar.GOOGLE'], na.rm = TRUE)
total.en.ar.TRANSFORMERS<- sum(data.diff[, 'diff.en.ar.TRANSFORMERS'], na.rm = TRUE)


# # Format totals with commas
# total.es.en.DEEP <- formatC(total.es.en.DEEP, format='f', big.mark=',', digits=0)
# total.es.en.DEEPL <- formatC(total.es.en.DEEPL, format='f', big.mark=',', digits=0)
# total.es.en.GOOGLE <- formatC(total.es.en.GOOGLE, format='f', big.mark=',', digits=0)
# total.es.en.TRANSOFMERS <- formatC(total.es.en.TRANSOFMERS, format='f', big.mark=',', digits=0)
# total.ar.en.DEEP <- formatC(total.ar.en.DEEP, format='f', big.mark=',', digits=0)
# total.ar.en.DEEPL <- formatC(total.ar.en.DEEPL, format='f', big.mark=',', digits=0)
# total.ar.en.GOOGLE <- formatC(total.ar.en.GOOGLE, format='f', big.mark=',', digits=0)
# total.ar.en.TRANSFORMERS <- formatC(total.ar.en.TRANSFORMERS, format='f', big.mark=',', digits=0)
# total.en.es.DEEP <- formatC(total.en.es.DEEP, format='f', big.mark=',', digits=0)
# total.en.es.DEEPL <- formatC(total.en.es.DEEPL, format='f', big.mark=',', digits=0)
# total.en.es.GOOGLE <- formatC(total.en.es.GOOGLE, format='f', big.mark=',', digits=0)
# total.en.es.TRANSFORMERS <- formatC(total.en.es.TRANSFORMERS, format='f', big.mark=',', digits=0)
# total.en.ar.DEEP <- formatC(total.en.ar.DEEP, format='f', big.mark=',', digits=0)
# total.en.ar.DEEPL <- formatC(total.en.ar.DEEPL, format='f', big.mark=',', digits=0)
# total.en.ar.GOOGLE <- formatC(total.en.ar.GOOGLE, format='f', big.mark=',', digits=0)
# total.en.ar.TRANSFORMERS <- formatC(total.en.ar.TRANSFORMERS, format='f', big.mark=',', digits=0)



# Transform to K
total.K.es.en.DEEP <- glue(round((as.numeric(total.es.en.DEEP)/1000), digits=0),'K')
total.K.es.en.DEEPL <- glue(round((as.numeric(total.es.en.DEEPL)/1000), digits=0),'K')
total.K.es.en.GOOGLE <- glue(round((as.numeric(total.es.en.GOOGLE)/1000), digits=0),'K')
total.K.es.en.TRANSOFMERS <- glue(round((as.numeric(total.es.en.TRANSOFMERS)/1000), digits=0),'K')
total.K.ar.en.DEEP <- glue(round((as.numeric(total.ar.en.DEEP)/1000), digits=0),'K')
total.K.ar.en.DEEPL <- glue(round((as.numeric(total.ar.en.DEEPL)/1000), digits=0),'K')
total.K.ar.en.GOOGLE <- glue(round((as.numeric(total.ar.en.GOOGLE)/1000), digits=0),'K')
total.K.ar.en.TRANSFORMERS <- glue(round((as.numeric(total.ar.en.TRANSFORMERS)/1000), digits=0),'K')
total.K.en.es.DEEP <- glue(round((as.numeric(total.en.es.DEEP)/1000), digits=0),'K')
total.K.en.es.DEEPL <- glue(round((as.numeric(total.en.es.DEEPL)/1000), digits=0),'K')
total.K.en.es.GOOGLE <- glue(round((as.numeric(total.en.es.GOOGLE)/1000), digits=0),'K')
total.K.en.es.TRANSFORMERS <- glue(round((as.numeric(total.en.es.TRANSFORMERS)/1000), digits=0),'K')
total.K.en.ar.DEEP <- glue(round((as.numeric(total.en.ar.DEEP)/1000), digits=0),'K')
total.K.en.ar.DEEPL <- glue(round((as.numeric(total.en.ar.DEEPL)/1000), digits=0),'K')
total.K.en.ar.GOOGLE <- glue(round((as.numeric(total.en.ar.GOOGLE)/1000), digits=0),'K')
total.K.en.ar.TRANSFORMERS <- glue(round((as.numeric(total.en.ar.TRANSFORMERS)/1000), digits=0),'K')

                            




#  WORLD CHANGE  ###########################


##  Prepare data  ###########################

data.diff <- data.diff %>%
  select(diff.es.en.DEEP , diff.es.en.DEEPL , diff.es.en.GOOGLE , diff.es.en.TRANSOFMERS ,
         diff.ar.en.DEEP , diff.ar.en.DEEPL , diff.ar.en.GOOGLE , diff.ar.en.TRANSFORMERS ,  
         diff.en.es.DEEP , diff.en.es.DEEPL , diff.en.es.GOOGLE , diff.en.es.TRANSFORMERS ,  
         diff.en.ar.DEEP , diff.en.ar.DEEPL , diff.en.ar.GOOGLE , diff.en.ar.TRANSFORMERS ) %>%
  pivot_longer(cols=c("diff.es.en.DEEP","diff.es.en.DEEPL","diff.es.en.GOOGLE","diff.es.en.TRANSOFMERS",
                      "diff.ar.en.DEEP","diff.ar.en.DEEPL","diff.ar.en.GOOGLE","diff.ar.en.TRANSFORMERS",  
                      "diff.en.es.DEEP","diff.en.es.DEEPL","diff.en.es.GOOGLE","diff.en.es.TRANSFORMERS",  
                      "diff.en.ar.DEEP","diff.en.ar.DEEPL","diff.en.ar.GOOGLE","diff.en.ar.TRANSFORMERS"),
               names_to="language",
               values_to="difference")


# Split language 
data.diff[c('diff', 'source', 'target','tool')] <- str_split_fixed(data.diff$language, "\\.", 4)



# Concatenate language
data.diff <- data.diff %>%
  mutate(source = toupper(source)) %>%
  mutate(target = toupper(target)) %>%
  mutate(lang=paste0(source, " to ", target)) %>%
  select(lang, tool, difference)



# Clean up tool names
data.diff <- data.diff %>% 
  mutate(tool=case_when(
    tool=="DEEP" ~ "Deep",
    tool=="DEEPL" ~ "DeepL",
    tool=="GOOGLE" ~ "GT",
    tool=="TRANSFORMERS" ~ "OPUS",
    tool=="TRANSOFMERS" ~ "OPUS"
  ))





#  PLOT WORLD CHANGE  ###########################


## Plot violin differences 
ggplot(data.diff, aes(x=tool, y=difference)) + 
  geom_violin() + 
  #facet_wrap(vars(lang)) +
  facet_wrap(~factor(lang, levels=c('AR to EN','EN to AR','ES to EN','EN to ES'))) +
  theme(axis.text.x = element_text(angle=90, vjust=.5, hjust=1)) + 
  xlab("Translation Tools") + ylab("Word difference") +
  theme_bw()

ggsave("graphs/diff_viol.pdf", width = 5, height = 5, units = "in")




## Plot box differences 
ggplot(data.diff, aes(x=tool, y=difference)) + 
  geom_boxplot() +
  #facet_wrap(vars(lang)) +
  facet_wrap(~factor(lang, levels=c('AR to EN','EN to AR','ES to EN','EN to ES'))) +
  theme(axis.text.x = element_text(angle=90, vjust=.5, hjust=1)) + 
  xlab("Translation Tools") + ylab("Word difference") +
  theme_bw()

ggsave("graphs/diff_box.pdf", width = 5, height = 5, units = "in")


 

##  Plot with net change ###########################


## Plot box differences with net change

b <- ggplot(data.diff, aes(x=tool, y=difference)) + 
  geom_boxplot() +
  #facet_wrap(vars(lang)) +
  facet_wrap(~factor(lang, levels=c('AR to EN','EN to AR','ES to EN','EN to ES'))) +
  theme(axis.text.x = element_text(angle=90, vjust=.5, hjust=1)) + 
  xlab("Translation Tools") + ylab("Word difference") + 
  ylim(-800,700) +
  theme_bw()

print(b)


# Define labels per panel
df_labels <- data.frame(
  lang = c("AR to EN","AR to EN","AR to EN","AR to EN",
           "EN to AR","EN to AR","EN to AR","EN to AR",
           "EN to ES","EN to ES","EN to ES","EN to ES",
           "ES to EN","ES to EN","ES to EN","ES to EN"),
  label = c(total.K.ar.en.DEEP, total.K.ar.en.DEEPL, total.K.ar.en.GOOGLE, total.K.ar.en.TRANSFORMERS,
            total.K.en.ar.DEEP, total.K.en.ar.DEEPL, total.K.en.ar.GOOGLE, total.K.en.ar.TRANSFORMERS,
            total.K.en.es.DEEP, total.K.en.es.DEEPL, total.K.en.es.GOOGLE, total.K.en.es.TRANSFORMERS,
            total.K.es.en.DEEP, total.K.es.en.DEEPL, total.K.es.en.GOOGLE, total.K.es.en.TRANSOFMERS),   tool = c(1,2,3,4),
  difference = c(600, 600, 600, 600,
                 -700,-700,-700,-700,
                 600, 600, 600, 600,
                 -700,-700,-700,-700))


# Add labels to graph
b + geom_text(
  data    = df_labels,
  mapping = aes(x = tool, y = difference,  label = label),
  colour="black"
  ) 

ggsave("graphs/diff_box_net.pdf", width = 5, height = 5, units = "in")






## Plot violin differences with net change

v <- ggplot(data.diff, aes(x=tool, y=difference)) + 
  geom_violin() + 
  #facet_wrap(vars(lang)) +
  facet_wrap(~factor(lang, levels=c('AR to EN','EN to AR','ES to EN','EN to ES'))) +
  theme(axis.text.x = element_text(angle=90, vjust=.5, hjust=1)) + 
  xlab("Translation Tools") + ylab("Word difference") + 
  ylim(-800,700) +
  theme_bw()

print(v)


# Define labels per panel
df_labels <- data.frame(
  lang = c("AR to EN","AR to EN","AR to EN","AR to EN",
           "EN to AR","EN to AR","EN to AR","EN to AR",
           "EN to ES","EN to ES","EN to ES","EN to ES",
           "ES to EN","ES to EN","ES to EN","ES to EN"),
  label = c(total.K.ar.en.DEEP, total.K.ar.en.DEEPL, total.K.ar.en.GOOGLE, total.K.ar.en.TRANSFORMERS,
            total.K.en.ar.DEEP, total.K.en.ar.DEEPL, total.K.en.ar.GOOGLE, total.K.en.ar.TRANSFORMERS,
            total.K.en.es.DEEP, total.K.en.es.DEEPL, total.K.en.es.GOOGLE, total.K.en.es.TRANSFORMERS,
            total.K.es.en.DEEP, total.K.es.en.DEEPL, total.K.es.en.GOOGLE, total.K.es.en.TRANSOFMERS),   tool = c(1,2,3,4),
  tool = c(1,2,3,4),
  difference = c(600, 600, 600, 600,
                 -700,-700,-700,-700,
                 600, 600, 600, 600,
                 -700,-700,-700,-700))


# Add labels to graph
v + geom_text(
  data    = df_labels,
  mapping = aes(x = tool, y = difference,  label = label),
  colour="black"
) 

ggsave("graphs/diff_viol_net.pdf", width = 5, height = 5, units = "in")






#  PLOT LOG WORLD CHANGE  ###########################


##  Log transform Word Change  ###########################

# Generate log transformation
data.diff <- data.diff %>% 
  mutate(diff.log = case_when(
    difference==0 ~ 0,
    difference>0 ~ log(difference),
    difference<0 ~(log(abs(difference))*(-1))
  ))



v.log <- ggplot(data.diff, aes(x=tool, y=diff.log, fill=tool)) + 
  geom_violin() + 
  #facet_wrap(vars(lang)) +
  facet_wrap(~factor(lang, levels=c('AR to EN','EN to AR','ES to EN','EN to ES'))) +
  theme(axis.text.x = element_text(angle=90, vjust=.5, hjust=1)) + 
  xlab("Translation Tools") + ylab("Word difference (log)") + 
  ylim(-8.5,8.5) + 
  #geom_hline(yintercept=0,  color = "black") +
  theme_bw() + 
  theme(legend.position="none")

print(v.log)


# Define labels per panel
df_labels <- data.frame(
  lang = c("AR to EN","AR to EN","AR to EN","AR to EN",
           "EN to AR","EN to AR","EN to AR","EN to AR",
           "EN to ES","EN to ES","EN to ES","EN to ES",
           "ES to EN","ES to EN","ES to EN","ES to EN"),
  label = c(total.K.ar.en.DEEP, total.K.ar.en.DEEPL, total.K.ar.en.GOOGLE, total.K.ar.en.TRANSFORMERS,
            total.K.en.ar.DEEP, total.K.en.ar.DEEPL, total.K.en.ar.GOOGLE, total.K.en.ar.TRANSFORMERS,
            total.K.en.es.DEEP, total.K.en.es.DEEPL, total.K.en.es.GOOGLE, total.K.en.es.TRANSFORMERS,
            total.K.es.en.DEEP, total.K.es.en.DEEPL, total.K.es.en.GOOGLE, total.K.es.en.TRANSOFMERS),   tool = c(1,2,3,4),
  tool = c(1,2,3,4),
  fill=c("Deep","DeepL","GT","OPUS"),
  difference = c(7, 7, 7, 7,
                 -7,-7,-7,-7,
                 7, 7, 7, 7,
                 -7,-7,-7,-7))


# Add labels to graph
v.log + geom_text(
  data    = df_labels,
  mapping = aes(x = tool, y = difference,  label = label, fill=fill),
  colour="black"
) 

ggsave("graphs/diff_viol_net_log.pdf", width = 5, height = 5, units = "in")







# End of script