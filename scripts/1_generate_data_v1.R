#####################################################################
# Translate COLING 2024
# Generate data
# Javier Osorio 
# 8-19-2024
#####################################################################



# SETUP --------------------------------------------------

# Load all packages here

if (!require("pacman")) install.packages("pacman")
pacman::p_load(here, glue, openxlsx, xtable, tidyverse, readxl, dplyr, stringr, stringi, 
               ggplot2, grid)




# GET THE DATA --------------------------------------------------

# Get the data

data.master <- read_excel("data/raw_data/data_master.xlsx")  




# CREATE FUNCTIONS --------------------------------------------------



# Generate function Binary
gen_data_bin <- function(lang){
  data.working.binary <- data.master %>% 
    select(lang, Relevant) 
  write.table(data.working.binary, file=glue('data/output_data/bin/{lang}_binary.tsv'), quote=FALSE, sep='\t', row.names = FALSE)
  write.xlsx(data.working.binary, file=glue('data/output_data/bin/{lang}_binary.xlsx'))
  rm(data.working.binary)
}


# Generate function QuadClass
gen_data_quad <- function(lang){
  data.working.quad <- data.master %>% 
    select(lang, QuadClass)  %>%
    filter(QuadClass>=1)
  write.table(data.working.quad, file=glue('data/output_data/quad/{lang}_quad.tsv'), quote=FALSE, sep='\t', row.names = FALSE)
  write.xlsx(data.working.quad, file=glue('data/output_data/quad/{lang}_quad.xlsx'))
  rm(data.working.quad)
}



# Generate function gen_data_quadclass_bin
gen_data_quadclass_bin <- function(lang){
  # MatConf
  data.working.MatConf <- data.master %>% select(lang, MatConf, Relevant, QuadClass) %>% filter(Relevant==1) %>% filter(QuadClass>0) %>% select(-c(Relevant, QuadClass))
  data.working.1.MatConf <- data.working.MatConf %>% filter(MatConf==1)
  get.n <- nrow(data.working.1.MatConf)   
  data.working.0.MatConf <- data.working.MatConf %>% filter(MatConf==0) %>% sample_n(., get.n)
  data.working.merge.MatConf <- rbind(data.working.1.MatConf,data.working.0.MatConf) %>% sample_frac()
  write.table(data.working.merge.MatConf, file=glue('data/output_data/bin_quad/{lang}_binary_matconf.tsv'), quote=FALSE, sep='\t', row.names = FALSE)
  write.xlsx(data.working.merge.MatConf, file=glue('data/output_data/bin_quad/{lang}_binary_matconf.xlsx'))
  rm(data.working.MatConf,data.working.1.MatConf,data.working.0.MatConf,data.working.merge.MatConf,get.n)
  # MatCoop
  data.working.MatCoop <- data.master %>% select(lang, MatCoop, Relevant, QuadClass) %>% filter(Relevant==1) %>% filter(QuadClass>0) %>% select(-c(Relevant, QuadClass))
  data.working.1.MatCoop <- data.working.MatCoop %>% filter(MatCoop==1)
  get.n <- nrow(data.working.1.MatCoop)   
  data.working.0.MatCoop <- data.working.MatCoop %>% filter(MatCoop==0) %>% sample_n(., get.n)
  data.working.merge.MatCoop <- rbind(data.working.1.MatCoop,data.working.0.MatCoop) %>% sample_frac()
  write.table(data.working.merge.MatCoop, file=glue('data/output_data/bin_quad/{lang}_binary_matcoop.tsv'), quote=FALSE, sep='\t', row.names = FALSE)
  write.xlsx(data.working.merge.MatCoop, file=glue('data/output_data/bin_quad/{lang}_binary_matcoop.xlsx'))
  rm(data.working.MatCoop,data.working.1.MatCoop,data.working.0.MatCoop,data.working.merge.MatCoop,get.n)
  # VerConf
  data.working.VerConf <- data.master %>% select(lang, VerConf, Relevant, QuadClass) %>% filter(Relevant==1) %>% filter(QuadClass>0) %>% select(-c(Relevant, QuadClass))
  data.working.1.VerConf <- data.working.VerConf %>% filter(VerConf==1)
  get.n <- nrow(data.working.1.VerConf)   
  data.working.0.VerConf <- data.working.VerConf %>% filter(VerConf==0) %>% sample_n(., get.n)
  data.working.merge.VerConf <- rbind(data.working.1.VerConf,data.working.0.VerConf) %>% sample_frac()
  write.table(data.working.merge.VerConf, file=glue('data/output_data/bin_quad/{lang}_binary_verconf.tsv'), quote=FALSE, sep='\t', row.names = FALSE)
  write.xlsx(data.working.merge.VerConf, file=glue('data/output_data/bin_quad/{lang}_binary_verconf.xlsx'))
  rm(data.working.VerConf,data.working.1.VerConf,data.working.0.VerConf,data.working.merge.VerConf,get.n)
  # VerCoop
  data.working.VerCoop <- data.master %>% select(lang, VerCoop, Relevant, QuadClass) %>% filter(Relevant==1) %>% filter(QuadClass>0) %>% select(-c(Relevant, QuadClass))
  data.working.1.VerCoop <- data.working.VerCoop %>% filter(VerCoop==1)
  get.n <- nrow(data.working.1.VerCoop)   
  data.working.0.VerCoop <- data.working.VerCoop %>% filter(VerCoop==0) %>% sample_n(., get.n)
  data.working.merge.VerCoop <- rbind(data.working.1.VerCoop,data.working.0.VerCoop) %>% sample_frac()
  write.table(data.working.merge.VerCoop, file=glue('data/output_data/bin_quad/{lang}_binary_vercoop.tsv'), quote=FALSE, sep='\t', row.names = FALSE)
  write.xlsx(data.working.merge.VerCoop, file=glue('data/output_data/bin_quad/{lang}_binary_vercoop.xlsx'))
  rm(data.working.VerCoop,data.working.1.VerCoop,data.working.0.VerCoop,data.working.merge.VerCoop,get.n)
}





# SET SEED --------------------------------------------------

# Set seed
set.seed(070924)



# GENERATE BINARY DATABASES --------------------------------------------------


# en
gen_data_bin(lang="en")

# es
gen_data_bin(lang="es")

# ar
gen_data_bin(lang="ar")

# es_en_DEEP
gen_data_bin(lang="es_en_DEEP")

# es_en_DEEPL
gen_data_bin(lang="es_en_DEEPL")

# es_en_GOOGLE
gen_data_bin(lang="es_en_GOOGLE")

# es_en_TRANSOFMERS
gen_data_bin(lang="es_en_TRANSOFMERS")

# ar_en_DEEP
gen_data_bin(lang="ar_en_DEEP")

# ar_en_DEEPL
gen_data_bin(lang="ar_en_DEEPL")

# ar_en_GOOGLE
gen_data_bin(lang="ar_en_GOOGLE")

# ar_en_TRANSFORMERS
gen_data_bin(lang="ar_en_TRANSFORMERS")

# en_es_DEEP
gen_data_bin(lang="en_es_DEEP")

# en_es_DEEPL
gen_data_bin(lang="en_es_DEEPL")

# en_es_GOOGLE
gen_data_bin(lang="en_es_GOOGLE")

# en_es_TRANSFORMERS
gen_data_bin(lang="en_es_TRANSFORMERS")

# en_ar_DEEP
gen_data_bin(lang="en_ar_DEEP")

# en_ar_DEEPL
gen_data_bin(lang="en_ar_DEEPL")

# en_ar_GOOGLE
gen_data_bin(lang="en_ar_GOOGLE")

# en_ar_TRANSFORMERS
gen_data_bin(lang="en_ar_TRANSFORMERS")




# GENERATE QUADCLASS DATABASES --------------------------------------------------


# en
gen_data_quad(lang="en")

# es
gen_data_quad(lang="es")

# ar
gen_data_quad(lang="ar")

# es_en_DEEP
gen_data_quad(lang="es_en_DEEP")

# es_en_DEEPL
gen_data_quad(lang="es_en_DEEPL")

# es_en_GOOGLE
gen_data_quad(lang="es_en_GOOGLE")

# es_en_TRANSOFMERS
gen_data_quad(lang="es_en_TRANSOFMERS")

# ar_en_DEEP
gen_data_quad(lang="ar_en_DEEP")

# ar_en_DEEPL
gen_data_quad(lang="ar_en_DEEPL")

# ar_en_GOOGLE
gen_data_quad(lang="ar_en_GOOGLE")

# ar_en_TRANSFORMERS
gen_data_quad(lang="ar_en_TRANSFORMERS")

# en_es_DEEP
gen_data_quad(lang="en_es_DEEP")

# en_es_DEEPL
gen_data_quad(lang="en_es_DEEPL")

# en_es_GOOGLE
gen_data_quad(lang="en_es_GOOGLE")

# en_es_TRANSFORMERS
gen_data_quad(lang="en_es_TRANSFORMERS")

# en_ar_DEEP
gen_data_quad(lang="en_ar_DEEP")

# en_ar_DEEPL
gen_data_quad(lang="en_ar_DEEPL")

# en_ar_GOOGLE
gen_data_quad(lang="en_ar_GOOGLE")

# en_ar_TRANSFORMERS
gen_data_quad(lang="en_ar_TRANSFORMERS")






# GENERATE QUADCLASS BINARY DATABASES --------------------------------------------------


# en
gen_data_quadclass_bin(lang="en")

# es
gen_data_quadclass_bin(lang="es")

# ar
gen_data_quadclass_bin(lang="ar")

# es_en_DEEP
gen_data_quadclass_bin(lang="es_en_DEEP")

# es_en_DEEPL
gen_data_quadclass_bin(lang="es_en_DEEPL")

# es_en_GOOGLE
gen_data_quadclass_bin(lang="es_en_GOOGLE")

# es_en_TRANSOFMERS
gen_data_quadclass_bin(lang="es_en_TRANSOFMERS")

# ar_en_DEEP
gen_data_quadclass_bin(lang="ar_en_DEEP")

# ar_en_DEEPL
gen_data_quadclass_bin(lang="ar_en_DEEPL")

# ar_en_GOOGLE
gen_data_quadclass_bin(lang="ar_en_GOOGLE")

# ar_en_TRANSFORMERS
gen_data_quadclass_bin(lang="ar_en_TRANSFORMERS")

# en_es_DEEP
gen_data_quadclass_bin(lang="en_es_DEEP")

# en_es_DEEPL
gen_data_quadclass_bin(lang="en_es_DEEPL")

# en_es_GOOGLE
gen_data_quadclass_bin(lang="en_es_GOOGLE")

# en_es_TRANSFORMERS
gen_data_quadclass_bin(lang="en_es_TRANSFORMERS")

# en_ar_DEEP
gen_data_quadclass_bin(lang="en_ar_DEEP")

# en_ar_DEEPL
gen_data_quadclass_bin(lang="en_ar_DEEPL")

# en_ar_GOOGLE
gen_data_quadclass_bin(lang="en_ar_GOOGLE")

# en_ar_TRANSFORMERS
gen_data_quadclass_bin(lang="en_ar_TRANSFORMERS")







# End of script