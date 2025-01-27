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

data.master <- read_excel("data/raw_data/data_master_text.xlsx")  

# Get n1
n1 <- dim(data.master)[1]
n1

# CLEAN UP THE DATA --------------------------------------------------


# Get rows with NA
data.na <- data.master[!complete.cases(data.master),]
data.na <- data.na %>% select(id)
data.na

write.xlsx(data.na,  file=glue('data/raw_data/list_nas.xlsx'))



# eliminate any row with NAs
data.master <- data.master %>% drop_na()
n2 <- dim(data.master)[1]
n2
n1-n2
# This eliminates 167 rows with NAs


# eliminate any row with "NA" as text
check.nas <- data.master %>% filter(en=="NA")
check.nas <- data.master %>% filter(is.na(en))

# Check there are no NA cases
#check.nas <- data.master %>% filter(id=="2005/s/2005/679/21:1")



# Export clean data frame
write.table(data.master, file=glue('data/raw_data/data_master_text_clean_2.tsv'), sep='\t', row.names = FALSE)
write.xlsx(data.master,  file=glue('data/raw_data/data_master_text_clean_2.xlsx'))
write.csv(data.master,   file=glue('data/raw_data/data_master_text_clean_2.csv', row.names=FALSE))





# CREATE FUNCTIONS --------------------------------------------------



# Generate function Binary
gen_data_bin <- function(lang){
  data.working.binary <- data.master %>% 
    select(lang, Relevant) 
  write.table(data.working.binary, file=glue('data/output_data/bin/{lang}_binary.tsv'), sep='\t', row.names = FALSE)
  write.xlsx(data.working.binary,  file=glue('data/output_data/bin/{lang}_binary.xlsx'))
  write.csv(data.working.binary,   file=glue('data/output_data/bin/{lang}_binary.csv', row.names=FALSE))
  rm(data.working.binary)
}


# Generate function QuadClass
gen_data_quad <- function(lang){
  data.working.quad <- data.master %>% 
    select(lang, QuadClass) %>%
    filter(QuadClass>=0)
  write.table(data.working.quad, file=glue('data/output_data/quad/{lang}_quad.tsv'), sep='\t', row.names = FALSE)
  write.xlsx(data.working.quad,  file=glue('data/output_data/quad/{lang}_quad.xlsx'))
  write.csv(data.working.quad,   file=glue('data/output_data/quad/{lang}_quad.csv', row.names=FALSE))
  rm(data.working.quad)
}



# Generate function gen_data_quadclass_bin
gen_data_quadclass_bin <- function(lang){
  # MatConf
  data.working.MatConf <- data.master %>% select(lang, MatConf, QuadClass) %>% filter(QuadClass>=0) %>% select(-c(QuadClass))
  data.working.1.MatConf <- data.working.MatConf %>% filter(MatConf==1)
  get.n <- nrow(data.working.1.MatConf)   
  data.working.0.MatConf <- data.working.MatConf %>% filter(MatConf==0) %>% sample_n(., get.n)
  data.working.merge.MatConf <- rbind(data.working.1.MatConf,data.working.0.MatConf) %>% sample_frac()
  write.table(data.working.merge.MatConf, file=glue('data/output_data/bin_quad/{lang}_binary_matconf.tsv'), sep='\t', row.names = FALSE)
  write.xlsx(data.working.merge.MatConf,  file=glue('data/output_data/bin_quad/{lang}_binary_matconf.xlsx'))
  write.csv(data.working.merge.MatConf,   file=glue('data/output_data/bin_quad/{lang}_binary_matconf.csv', row.names=FALSE))
  rm(data.working.MatConf,data.working.1.MatConf,data.working.0.MatConf,data.working.merge.MatConf,get.n)
  # MatCoop
  data.working.MatCoop <- data.master %>% select(lang, MatCoop, QuadClass) %>% filter(QuadClass>=0) %>% select(-c(QuadClass))
  data.working.1.MatCoop <- data.working.MatCoop %>% filter(MatCoop==1)
  get.n <- nrow(data.working.1.MatCoop)   
  data.working.0.MatCoop <- data.working.MatCoop %>% filter(MatCoop==0) %>% sample_n(., get.n)
  data.working.merge.MatCoop <- rbind(data.working.1.MatCoop,data.working.0.MatCoop) %>% sample_frac()
  write.table(data.working.merge.MatCoop, file=glue('data/output_data/bin_quad/{lang}_binary_MatCoop.tsv'), sep='\t', row.names = FALSE)
  write.xlsx(data.working.merge.MatCoop,  file=glue('data/output_data/bin_quad/{lang}_binary_MatCoop.xlsx'))
  write.csv(data.working.merge.MatCoop,   file=glue('data/output_data/bin_quad/{lang}_binary_MatCoop.csv', row.names=FALSE))
  rm(data.working.MatCoop,data.working.1.MatCoop,data.working.0.MatCoop,data.working.merge.MatCoop,get.n)
  # VerConf
  data.working.VerConf <- data.master %>% select(lang, VerConf, QuadClass) %>% filter(QuadClass>=0) %>% select(-c(QuadClass))
  data.working.1.VerConf <- data.working.VerConf %>% filter(VerConf==1)
  get.n <- nrow(data.working.1.VerConf)   
  data.working.0.VerConf <- data.working.VerConf %>% filter(VerConf==0) %>% sample_n(., get.n)
  data.working.merge.VerConf <- rbind(data.working.1.VerConf,data.working.0.VerConf) %>% sample_frac()
  write.table(data.working.merge.VerConf, file=glue('data/output_data/bin_quad/{lang}_binary_VerConf.tsv'), sep='\t', row.names = FALSE)
  write.xlsx(data.working.merge.VerConf,  file=glue('data/output_data/bin_quad/{lang}_binary_VerConf.xlsx'))
  write.csv(data.working.merge.VerConf,   file=glue('data/output_data/bin_quad/{lang}_binary_VerConf.csv', row.names=FALSE))
  rm(data.working.VerConf,data.working.1.VerConf,data.working.0.VerConf,data.working.merge.VerConf,get.n)
  # VerCoop
  data.working.VerCoop <- data.master %>% select(lang, VerCoop, QuadClass) %>% filter(QuadClass>=0) %>% select(-c(QuadClass))
  data.working.1.VerCoop <- data.working.VerCoop %>% filter(VerCoop==1)
  get.n <- nrow(data.working.1.VerCoop)   
  data.working.0.VerCoop <- data.working.VerCoop %>% filter(VerCoop==0) %>% sample_n(., get.n)
  data.working.merge.VerCoop <- rbind(data.working.1.VerCoop,data.working.0.VerCoop) %>% sample_frac()
  write.table(data.working.merge.VerCoop, file=glue('data/output_data/bin_quad/{lang}_binary_VerCoop.tsv'), sep='\t', row.names = FALSE)
  write.xlsx(data.working.merge.VerCoop,  file=glue('data/output_data/bin_quad/{lang}_binary_VerCoop.xlsx'))
  write.csv(data.working.merge.VerCoop,   file=glue('data/output_data/bin_quad/{lang}_binary_VerCoop.csv', row.names=FALSE))
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