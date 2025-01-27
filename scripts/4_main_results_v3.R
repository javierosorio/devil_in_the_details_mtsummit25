#####################################################################
# UN Multilingual Corpus
# Translation Assessment
# Javier Osorio 
# 9-8-2024
#####################################################################



# SETUP --------------------------------------------------

# Load all packages here

if (!require("pacman")) install.packages("pacman")

pacman::p_load(here, glue, openxlsx, tidyverse, readxl, dplyr, stringr, stringi, 
               ggplot2, grid,  ggpubr, BSDA)



# GET THE DATA --------------------------------------------------

## Bin ----------------------

#data.x <- read.csv(file = '',  header = TRUE)


# EN
data.bin.EN <- read.csv(file = 'https://raw.githubusercontent.com/javierosorio/translation_coling24/main/Results/Binary/en_binary_full_report.csv',  header = TRUE)
data.bin.AR.EN.DEEP <- read.csv(file = 'https://raw.githubusercontent.com/javierosorio/translation_coling24/main/Results/Binary/ar_en_DEEP_binary_full_report.csv',  header = TRUE)
data.bin.AR.EN.DEEPL <- read.csv(file = 'https://raw.githubusercontent.com/javierosorio/translation_coling24/main/Results/Binary/ar_en_DEEPL_binary_full_report.csv',  header = TRUE)
data.bin.AR.EN.GT <- read.csv(file = 'https://raw.githubusercontent.com/javierosorio/translation_coling24/main/Results/Binary/Binary_AR_EN_GT_full_report.csv',  header = TRUE)
data.bin.AR.EN.TR <- read.csv(file = 'https://raw.githubusercontent.com/javierosorio/translation_coling24/main/Results/Binary/Binary_AR_EN_TR_full_report.csv',  header = TRUE)
data.bin.ES.EN.DEEP <- read.csv(file = 'https://raw.githubusercontent.com/javierosorio/translation_coling24/main/Results/Binary/es_en_DEEP_binary_full_report.csv',  header = TRUE)
data.bin.ES.EN.DEEPL <- read.csv(file = 'https://raw.githubusercontent.com/javierosorio/translation_coling24/main/Results/Binary/es_en_DEEPL_binary_full_report.csv',  header = TRUE)
data.bin.ES.EN.GT <- read.csv(file = 'https://raw.githubusercontent.com/javierosorio/translation_coling24/main/Results/Binary/Binary_ES_EN_GT_full_report.csv',  header = TRUE)
data.bin.ES.EN.TR <- read.csv(file = 'https://raw.githubusercontent.com/javierosorio/translation_coling24/main/Results/Binary/Binary_ES_EN_TR_full_report.csv',  header = TRUE)
# ES
data.bin.ES <- read.csv(file = 'https://raw.githubusercontent.com/javierosorio/translation_coling24/main/Results/Binary/es_binary_full_report.csv',  header = TRUE)
data.bin.EN.ES.DEEP <- read.csv(file = 'https://raw.githubusercontent.com/javierosorio/translation_coling24/main/Results/Binary/en_es_DEEP_binary_full_report.csv',  header = TRUE)
data.bin.EN.ES.DEEPL <- read.csv(file = 'https://raw.githubusercontent.com/javierosorio/translation_coling24/main/Results/Binary/en_es_DEEPL_binary_full_report.csv',  header = TRUE)
data.bin.EN.ES.GT <- read.csv(file = 'https://raw.githubusercontent.com/javierosorio/translation_coling24/main/Results/Binary/Binary_EN_ES_GT_full_report.csv',  header = TRUE)
data.bin.EN.ES.TR <- read.csv(file = 'https://raw.githubusercontent.com/javierosorio/translation_coling24/main/Results/Binary/Binary_EN_ES_TR_full_report.csv',  header = TRUE)
# AR 
data.bin.AR <- read.csv(file = 'https://raw.githubusercontent.com/javierosorio/translation_coling24/main/Results/Binary/ar_binary_full_report.csv',  header = TRUE)
data.bin.EN.AR.DEEP <- read.csv(file = 'https://raw.githubusercontent.com/javierosorio/translation_coling24/main/Results/Binary/en_ar_DEEP_binary_full_report.csv',  header = TRUE)
data.bin.EN.AR.DEEPL <- read.csv(file = 'https://raw.githubusercontent.com/javierosorio/translation_coling24/main/Results/Binary/en_ar_DEEPL_binary_full_report.csv',  header = TRUE)
data.bin.EN.AR.GT <- read.csv(file = 'https://raw.githubusercontent.com/javierosorio/translation_coling24/main/Results/Binary/Binary_EN_AR_GT_full_report.csv',  header = TRUE)
data.bin.EN.AR.TR <- read.csv(file = 'https://raw.githubusercontent.com/javierosorio/translation_coling24/main/Results/Binary/Binary_EN_AR_TR_full_report.csv',  header = TRUE)




## QuadClass ----------------------

# EN
data.quad.EN <- read.csv(file = 'https://raw.githubusercontent.com/javierosorio/translation_coling24/main/Results/Quad/Quad_EN_full_report.csv',  header = TRUE)
data.quad.ES.EN.GT <- read.csv(file = 'https://raw.githubusercontent.com/javierosorio/translation_coling24/main/Results/Quad/Quad_ES_EN_GT_full_report.csv',  header = TRUE)
data.quad.ES.EN.TR <- read.csv(file = 'https://raw.githubusercontent.com/javierosorio/translation_coling24/main/Results/Quad/Quad_ES_EN_TR_full_report.csv',  header = TRUE)
data.quad.AR.EN.GT <- read.csv(file = 'https://raw.githubusercontent.com/javierosorio/translation_coling24/main/Results/Quad/Quad_AR_EN_GT_full_report.csv',  header = TRUE)
data.quad.AR.EN.TR <- read.csv(file = 'https://raw.githubusercontent.com/javierosorio/translation_coling24/main/Results/Quad/Quad_AR_EN_TR_full_report.csv',  header = TRUE)
data.quad.AR.EN.DEEPL <- read.csv(file = 'https://raw.githubusercontent.com/javierosorio/translation_coling24/main/Results/Quad/ar_en_DEEPL_quad_full_report.csv',  header = TRUE)
data.quad.AR.EN.DEEP <- read.csv(file = 'https://raw.githubusercontent.com/javierosorio/translation_coling24/main/Results/Quad/ar_en_DEEP_quad_full_report.csv',  header = TRUE)
data.quad.ES.EN.DEEPL <- read.csv(file = 'https://raw.githubusercontent.com/javierosorio/translation_coling24/main/Results/Quad/es_en_DEEPL_quad_full_report.csv',  header = TRUE)
data.quad.ES.EN.DEEP <- read.csv(file = 'https://raw.githubusercontent.com/javierosorio/translation_coling24/main/Results/Quad/es_en_DEEP_quad_full_report.csv',  header = TRUE)
# AR
data.quad.AR <- read.csv(file = 'https://raw.githubusercontent.com/javierosorio/translation_coling24/main/Results/Quad/Quad_AR_full_report.csv',  header = TRUE)
data.quad.EN.AR.GT <- read.csv(file = 'https://raw.githubusercontent.com/javierosorio/translation_coling24/main/Results/Quad/Quad_EN_AR_GT_full_report.csv',  header = TRUE)
data.quad.EN.AR.TR <- read.csv(file = 'https://raw.githubusercontent.com/javierosorio/translation_coling24/main/Results/Quad/Quad_EN_AR_TR_full_report.csv',  header = TRUE)
data.quad.EN.AR.DEEPL <- read.csv(file = 'https://raw.githubusercontent.com/javierosorio/translation_coling24/main/Results/Quad/en_ar_DEEPL_quad_full_report.csv',  header = TRUE)
data.quad.EN.AR.DEEP <- read.csv(file = 'https://raw.githubusercontent.com/javierosorio/translation_coling24/main/Results/Quad/en_ar_DEEP_quad_full_report.csv',  header = TRUE)
# ES
data.quad.ES <- read.csv(file = 'https://raw.githubusercontent.com/javierosorio/translation_coling24/main/Results/Quad/Quad_ES_full_report.csv',  header = TRUE)
data.quad.EN.ES.TR <- read.csv(file = 'https://raw.githubusercontent.com/javierosorio/translation_coling24/main/Results/Quad/Quad_EN_ES_TR_full_report.csv',  header = TRUE)
data.quad.EN.ES.GT <- read.csv(file = 'https://raw.githubusercontent.com/javierosorio/translation_coling24/main/Results/Quad/Quad_EN_ES_GT_full_report.csv',  header = TRUE)
data.quad.EN.ES.DEEPL <- read.csv(file = 'https://raw.githubusercontent.com/javierosorio/translation_coling24/main/Results/Quad/en_es_DEEPL_quad_full_report.csv',  header = TRUE)
data.quad.EN.ES.DEEP <- read.csv(file = 'https://raw.githubusercontent.com/javierosorio/translation_coling24/main/Results/Quad/en_es_DEEP_quad_full_report.csv',  header = TRUE)




## BinQuad ----------------------


data.binquad.AR.EN.DEEP.matconf <- read.csv(file = 'https://raw.githubusercontent.com/javierosorio/translation_coling24/main/Results/BinQuad/BinQuad_AR_EN_DEEP_matconf_full_report.csv', header = TRUE)
data.binquad.AR.EN.DEEP.matcoop <- read.csv(file = 'https://raw.githubusercontent.com/javierosorio/translation_coling24/main/Results/BinQuad/BinQuad_AR_EN_DEEP_matcoop_full_report.csv', header = TRUE)
data.binquad.AR.EN.DEEP.verconf <- read.csv(file = 'https://raw.githubusercontent.com/javierosorio/translation_coling24/main/Results/BinQuad/BinQuad_AR_EN_DEEP_verconf_full_report.csv', header = TRUE)
data.binquad.AR.EN.DEEP.vercoop <- read.csv(file = 'https://raw.githubusercontent.com/javierosorio/translation_coling24/main/Results/BinQuad/BinQuad_AR_EN_DEEP_vercoop_full_report.csv', header = TRUE)
data.binquad.AR.EN.DEEPL.matconf <- read.csv(file = 'https://raw.githubusercontent.com/javierosorio/translation_coling24/main/Results/BinQuad/BinQuad_AR_EN_DEEPL_matconf_full_report.csv', header = TRUE)
data.binquad.AR.EN.DEEPL.matcoop <- read.csv(file = 'https://raw.githubusercontent.com/javierosorio/translation_coling24/main/Results/BinQuad/BinQuad_AR_EN_DEEPL_matcoop_full_report.csv', header = TRUE)
data.binquad.AR.EN.DEEPL.verconf <- read.csv(file = 'https://raw.githubusercontent.com/javierosorio/translation_coling24/main/Results/BinQuad/BinQuad_AR_EN_DEEPL_verconf_full_report.csv', header = TRUE)
data.binquad.AR.EN.DEEPL.vercoop <- read.csv(file = 'https://raw.githubusercontent.com/javierosorio/translation_coling24/main/Results/BinQuad/BinQuad_AR_EN_DEEPL_vercoop_full_report.csv', header = TRUE)
data.binquad.AR.EN.GT.matconf <- read.csv(file = 'https://raw.githubusercontent.com/javierosorio/translation_coling24/main/Results/BinQuad/BinQuad_AR_EN_GOOGLE_matconf_full_report.csv', header = TRUE)
data.binquad.AR.EN.GT.matcoop <- read.csv(file = 'https://raw.githubusercontent.com/javierosorio/translation_coling24/main/Results/BinQuad/ar_en_GOOGLE_binary_matcoop_full_report.csv', header = TRUE)
data.binquad.AR.EN.GT.verconf <- read.csv(file = 'https://raw.githubusercontent.com/javierosorio/translation_coling24/main/Results/BinQuad/ar_en_GOOGLE_binary_verconf_full_report.csv', header = TRUE)
data.binquad.AR.EN.GT.vercoop <- read.csv(file = 'https://raw.githubusercontent.com/javierosorio/translation_coling24/main/Results/BinQuad/ar_en_GOOGLE_binary_vercoop_full_report.csv', header = TRUE)
data.binquad.AR.EN.OPUS.matconf <- read.csv(file = 'https://raw.githubusercontent.com/javierosorio/translation_coling24/main/Results/BinQuad/ar_en_TRANSFORMERS_binary_matconf_full_report.csv', header = TRUE)
data.binquad.AR.EN.OPUS.matcoop <- read.csv(file = 'https://raw.githubusercontent.com/javierosorio/translation_coling24/main/Results/BinQuad/ar_en_TRANSFORMERS_binary_matcoop_full_report.csv', header = TRUE)
data.binquad.AR.EN.OPUS.verconf <- read.csv(file = 'https://raw.githubusercontent.com/javierosorio/translation_coling24/main/Results/BinQuad/ar_en_TRANSFORMERS_binary_verconf_full_report.csv', header = TRUE)
data.binquad.AR.EN.OPUS.vercoop <- read.csv(file = 'https://raw.githubusercontent.com/javierosorio/translation_coling24/main/Results/BinQuad/ar_en_TRANSFORMERS_binary_vercoop_full_report.csv', header = TRUE)
data.binquad.AR.matconf <- read.csv(file = 'https://raw.githubusercontent.com/javierosorio/translation_coling24/main/Results/BinQuad/BinQuad_AR_matconf_full_report.csv', header = TRUE)
data.binquad.AR.matcoop <- read.csv(file = 'https://raw.githubusercontent.com/javierosorio/translation_coling24/main/Results/BinQuad/BinQuad_AR_matcoop_full_report.csv', header = TRUE)
data.binquad.AR.vercoop <- read.csv(file = 'https://raw.githubusercontent.com/javierosorio/translation_coling24/main/Results/BinQuad/BinQuad_AR_vercoop_full_report.csv', header = TRUE)
# Updated ->
data.binquad.AR.verconf <- read.csv(file = 'https://raw.githubusercontent.com/javierosorio/translation_coling24/main/Results/BinQuad/BinQuad_AR_verconf_full_report.csv', header = TRUE)
# <- Updated 
data.binquad.EN.AR.DEEP.matconf <- read.csv(file = 'https://raw.githubusercontent.com/javierosorio/translation_coling24/main/Results/BinQuad/en_ar_DEEP_binary_matconf_full_report.csv', header = TRUE)
data.binquad.EN.AR.DEEP.matcoop <- read.csv(file = 'https://raw.githubusercontent.com/javierosorio/translation_coling24/main/Results/BinQuad/en_ar_DEEP_binary_matcoop_full_report.csv', header = TRUE)
# Updated ->
data.binquad.EN.AR.DEEP.verconf <- read.csv(file = 'https://raw.githubusercontent.com/javierosorio/translation_coling24/main/Results/BinQuad/BinQuad_EN_AR_DEEP_verconf_full_report.csv', header = TRUE)
data.binquad.EN.AR.DEEP.vercoop <- read.csv(file = 'https://raw.githubusercontent.com/javierosorio/translation_coling24/main/Results/BinQuad/BinQuad_EN_AR_DEEP_vercoop_full_report.csv', header = TRUE)
# <- Updated 
data.binquad.EN.AR.DEEPL.matconf <- read.csv(file = 'https://raw.githubusercontent.com/javierosorio/translation_coling24/main/Results/BinQuad/en_ar_DEEPL_binary_matconf_full_report.csv', header = TRUE)
data.binquad.EN.AR.DEEPL.matcoop <- read.csv(file = 'https://raw.githubusercontent.com/javierosorio/translation_coling24/main/Results/BinQuad/en_ar_DEEPL_binary_matcoop_full_report.csv', header = TRUE)
data.binquad.EN.AR.DEEPL.verconf <- read.csv(file = 'https://raw.githubusercontent.com/javierosorio/translation_coling24/main/Results/BinQuad/en_ar_DEEPL_binary_verconf_full_report.csv', header = TRUE)
data.binquad.EN.AR.DEEPL.vercoop <- read.csv(file = 'https://raw.githubusercontent.com/javierosorio/translation_coling24/main/Results/BinQuad/en_ar_DEEPL_binary_vercoop_full_report.csv', header = TRUE)
data.binquad.EN.AR.GT.matconf <- read.csv(file = 'https://raw.githubusercontent.com/javierosorio/translation_coling24/main/Results/BinQuad/BinQuad_EN_AR_GOOGLE_matconf_full_report.csv', header = TRUE)
data.binquad.EN.AR.GT.matcoop <- read.csv(file = 'https://raw.githubusercontent.com/javierosorio/translation_coling24/main/Results/BinQuad/BinQuad_EN_AR_GOOGLE_matcoop_full_report.csv', header = TRUE)
data.binquad.EN.AR.GT.verconf <- read.csv(file = 'https://raw.githubusercontent.com/javierosorio/translation_coling24/main/Results/BinQuad/BinQuad_EN_AR_GOOGLE_verconf_full_report.csv', header = TRUE)
data.binquad.EN.AR.GT.vercoop <- read.csv(file = 'https://raw.githubusercontent.com/javierosorio/translation_coling24/main/Results/BinQuad/BinQuad_EN_AR_GOOGLE_vercoop_full_report.csv', header = TRUE)
data.binquad.EN.AR.OPUS.matconf <- read.csv(file = 'https://raw.githubusercontent.com/javierosorio/translation_coling24/main/Results/BinQuad/BinQuad_EN_AR_TRANSFORMERS_matconf_full_report.csv', header = TRUE)
data.binquad.EN.AR.OPUS.matcoop <- read.csv(file = 'https://raw.githubusercontent.com/javierosorio/translation_coling24/main/Results/BinQuad/BinQuad_EN_AR_TRANSFORMERS_matcoop_full_report.csv', header = TRUE)
data.binquad.EN.AR.OPUS.verconf <- read.csv(file = 'https://raw.githubusercontent.com/javierosorio/translation_coling24/main/Results/BinQuad/BinQuad_EN_AR_TRANSFORMERS_verconf_full_report.csv', header = TRUE)
data.binquad.EN.AR.OPUS.vercoop <- read.csv(file = 'https://raw.githubusercontent.com/javierosorio/translation_coling24/main/Results/BinQuad/BinQuad_EN_AR_TRANSFORMERS_vercoop_full_report.csv', header = TRUE)
data.binquad.EN.ES.DEEP.matcon <- read.csv(file = 'https://raw.githubusercontent.com/javierosorio/translation_coling24/main/Results/BinQuad/BinQuad_EN_ES_DEEP_matconf_full_report.csv', header = TRUE)
data.binquad.EN.ES.DEEP.matcoop <- read.csv(file = 'https://raw.githubusercontent.com/javierosorio/translation_coling24/main/Results/BinQuad/BinQuad_EN_ES_DEEP_matcoop_full_report.csv', header = TRUE)
data.binquad.EN.ES.DEEP.vercon <- read.csv(file = 'https://raw.githubusercontent.com/javierosorio/translation_coling24/main/Results/BinQuad/BinQuad_EN_ES_DEEP_verconf_full_report.csv', header = TRUE)
data.binquad.EN.ES.DEEP.vercoop <- read.csv(file = 'https://raw.githubusercontent.com/javierosorio/translation_coling24/main/Results/BinQuad/BinQuad_EN_ES_DEEP_vercoop_full_report.csv', header = TRUE)
data.binquad.EN.ES.DEEPL.matconf <- read.csv(file = 'https://raw.githubusercontent.com/javierosorio/translation_coling24/main/Results/BinQuad/BinQuad_EN_ES_DEEPL_matconf_full_report.csv', header = TRUE)
data.binquad.EN.ES.DEEPL.matcoop <- read.csv(file = 'https://raw.githubusercontent.com/javierosorio/translation_coling24/main/Results/BinQuad/BinQuad_EN_ES_DEEPL_matcoop_full_report.csv', header = TRUE)
data.binquad.EN.ES.DEEPL.verconf <- read.csv(file = 'https://raw.githubusercontent.com/javierosorio/translation_coling24/main/Results/BinQuad/BinQuad_EN_ES_DEEPL_verconf_full_report.csv', header = TRUE)
data.binquad.EN.ES.DEEPL.vercoop <- read.csv(file = 'https://raw.githubusercontent.com/javierosorio/translation_coling24/main/Results/BinQuad/BinQuad_EN_ES_DEEPL_vercoop_full_report.csv', header = TRUE)
data.binquad.EN.ES.GT.matconf <- read.csv(file = 'https://raw.githubusercontent.com/javierosorio/translation_coling24/main/Results/BinQuad/BinQuad_EN_ES_GOOGLE_matconf_full_report.csv', header = TRUE)
data.binquad.EN.ES.GT.matcoop <- read.csv(file = 'https://raw.githubusercontent.com/javierosorio/translation_coling24/main/Results/BinQuad/BinQuad_EN_ES_GOOGLE_matcoop_full_report.csv', header = TRUE)
data.binquad.EN.ES.GT.verconf <- read.csv(file = 'https://raw.githubusercontent.com/javierosorio/translation_coling24/main/Results/BinQuad/BinQuad_EN_ES_GOOGLE_verconf_full_report.csv', header = TRUE)
data.binquad.EN.ES.GT.vercoop <- read.csv(file = 'https://raw.githubusercontent.com/javierosorio/translation_coling24/main/Results/BinQuad/BinQuad_EN_ES_GOOGLE_vercoop_full_report.csv', header = TRUE)
data.binquad.EN.ES.OPUS.matconf <- read.csv(file = 'https://raw.githubusercontent.com/javierosorio/translation_coling24/main/Results/BinQuad/en_es_TRANSFORMERS_binary_matconf_full_report.csv', header = TRUE)
data.binquad.EN.ES.OPUS.matcoop <- read.csv(file = 'https://raw.githubusercontent.com/javierosorio/translation_coling24/main/Results/BinQuad/en_es_TRANSFORMERS_binary_matcoop_full_report.csv', header = TRUE)
data.binquad.EN.ES.OPUS.verconf <- read.csv(file = 'https://raw.githubusercontent.com/javierosorio/translation_coling24/main/Results/BinQuad/en_es_TRANSFORMERS_binary_verconf_full_report.csv', header = TRUE)
data.binquad.EN.ES.OPUS.vercoop <- read.csv(file = 'https://raw.githubusercontent.com/javierosorio/translation_coling24/main/Results/BinQuad/en_es_TRANSFORMERS_binary_vercoop_full_report.csv', header = TRUE)
data.binquad.EN.matconf <- read.csv(file = 'https://raw.githubusercontent.com/javierosorio/translation_coling24/main/Results/BinQuad/BinQuad_EN_matconf_full_report.csv', header = TRUE)
data.binquad.EN.matcoop <- read.csv(file = 'https://raw.githubusercontent.com/javierosorio/translation_coling24/main/Results/BinQuad/BinQuad_EN_matcoop_full_report.csv', header = TRUE)
data.binquad.EN.verconf <- read.csv(file = 'https://raw.githubusercontent.com/javierosorio/translation_coling24/main/Results/BinQuad/BinQuad_EN_verconf_full_report.csv', header = TRUE)
data.binquad.EN.vercoop <- read.csv(file = 'https://raw.githubusercontent.com/javierosorio/translation_coling24/main/Results/BinQuad/BinQuad_EN_vercoop_full_report.csv', header = TRUE)
data.binquad.ES.EN.DEEP.matconf <- read.csv(file = 'https://raw.githubusercontent.com/javierosorio/translation_coling24/main/Results/BinQuad/es_en_DEEP_binary_matconf_full_report.csv', header = TRUE)
data.binquad.ES.EN.DEEP.matcoop <- read.csv(file = 'https://raw.githubusercontent.com/javierosorio/translation_coling24/main/Results/BinQuad/es_en_DEEP_binary_matcoop_full_report.csv', header = TRUE)
data.binquad.ES.EN.DEEP.verconf <- read.csv(file = 'https://raw.githubusercontent.com/javierosorio/translation_coling24/main/Results/BinQuad/es_en_DEEP_binary_verconf_full_report.csv', header = TRUE)
data.binquad.ES.EN.DEEP.vercoop <- read.csv(file = 'https://raw.githubusercontent.com/javierosorio/translation_coling24/main/Results/BinQuad/es_en_DEEP_binary_vercoop_full_report.csv', header = TRUE)
data.binquad.ES.EN.DEEPL.matconf <- read.csv(file = 'https://raw.githubusercontent.com/javierosorio/translation_coling24/main/Results/BinQuad/es_en_DEEPL_binary_matconf_full_report.csv', header = TRUE)
data.binquad.ES.EN.DEEPL.matcoop <- read.csv(file = 'https://raw.githubusercontent.com/javierosorio/translation_coling24/main/Results/BinQuad/es_en_DEEPL_binary_matcoop_full_report.csv', header = TRUE)
data.binquad.ES.EN.DEEPL.verconf <- read.csv(file = 'https://raw.githubusercontent.com/javierosorio/translation_coling24/main/Results/BinQuad/es_en_DEEPL_binary_verconf_full_report.csv', header = TRUE)
data.binquad.ES.EN.DEEPL.vercoop <- read.csv(file = 'https://raw.githubusercontent.com/javierosorio/translation_coling24/main/Results/BinQuad/es_en_DEEPL_binary_vercoop_full_report.csv', header = TRUE)
data.binquad.ES.EN.GT.matconf <- read.csv(file = 'https://raw.githubusercontent.com/javierosorio/translation_coling24/main/Results/BinQuad/es_en_GOOGLE_binary_matconf_full_report.csv', header = TRUE)
data.binquad.ES.EN.GT.matcoop <- read.csv(file = 'https://raw.githubusercontent.com/javierosorio/translation_coling24/main/Results/BinQuad/es_en_GOOGLE_binary_matcoop_full_report.csv', header = TRUE)
data.binquad.ES.EN.GT.verconf <- read.csv(file = 'https://raw.githubusercontent.com/javierosorio/translation_coling24/main/Results/BinQuad/es_en_GOOGLE_binary_verconf_full_report.csv', header = TRUE)
data.binquad.ES.EN.GT.vercoop <- read.csv(file = 'https://raw.githubusercontent.com/javierosorio/translation_coling24/main/Results/BinQuad/es_en_GOOGLE_binary_vercoop_full_report.csv', header = TRUE)
data.binquad.ES.EN.OPUS.matconf <- read.csv(file = 'https://raw.githubusercontent.com/javierosorio/translation_coling24/main/Results/BinQuad/es_en_TRANSOFMERS_binary_matconf_full_report.csv', header = TRUE)
data.binquad.ES.EN.OPUS.matcoop <- read.csv(file = 'https://raw.githubusercontent.com/javierosorio/translation_coling24/main/Results/BinQuad/es_en_TRANSOFMERS_binary_matcoop_full_report.csv', header = TRUE)
data.binquad.ES.EN.OPUS.verconf <- read.csv(file = 'https://raw.githubusercontent.com/javierosorio/translation_coling24/main/Results/BinQuad/es_en_TRANSOFMERS_binary_verconf_full_report.csv', header = TRUE)
data.binquad.ES.EN.OPUS.vercoop <- read.csv(file = 'https://raw.githubusercontent.com/javierosorio/translation_coling24/main/Results/BinQuad/es_en_TRANSOFMERS_binary_vercoop_full_report.csv', header = TRUE)
data.binquad.ES.matconf <- read.csv(file = 'https://raw.githubusercontent.com/javierosorio/translation_coling24/main/Results/BinQuad/es_binary_matconf_full_report.csv', header = TRUE)
data.binquad.ES.matcoop <- read.csv(file = 'https://raw.githubusercontent.com/javierosorio/translation_coling24/main/Results/BinQuad/es_binary_matcoop_full_report.csv', header = TRUE)
data.binquad.ES.verconf <- read.csv(file = 'https://raw.githubusercontent.com/javierosorio/translation_coling24/main/Results/BinQuad/es_binary_verconf_full_report.csv', header = TRUE)
data.binquad.ES.vercoop <- read.csv(file = 'https://raw.githubusercontent.com/javierosorio/translation_coling24/main/Results/BinQuad/es_binary_vercoop_full_report.csv', header = TRUE)














# INTEGRATE THE DATA --------------------------------------------------

# Binary
data.bin <- rbind(data.bin.AR, 
              data.bin.AR.EN.DEEP, 
              data.bin.AR.EN.DEEPL, 
              data.bin.AR.EN.GT,
              data.bin.AR.EN.TR, 
              data.bin.EN, 
              data.bin.EN.AR.DEEP, 
              data.bin.EN.AR.DEEPL, 
              data.bin.EN.AR.GT, 
              data.bin.EN.AR.TR, 
              data.bin.EN.ES.DEEP, 
              data.bin.EN.ES.DEEPL, 
              data.bin.EN.ES.GT, 
              data.bin.EN.ES.TR, 
              data.bin.ES, 
              data.bin.ES.EN.DEEP, 
              data.bin.ES.EN.DEEPL, 
              data.bin.ES.EN.GT, 
              data.bin.ES.EN.TR ) 


# Quadclass
data.quad <- rbind(data.quad.AR,
                   data.quad.AR.EN.DEEP,
                   data.quad.AR.EN.DEEPL,
                   data.quad.AR.EN.GT,
                   data.quad.AR.EN.TR,
                   data.quad.EN,
                   data.quad.EN.AR.DEEP,
                   data.quad.EN.AR.DEEPL,
                   data.quad.EN.AR.GT,
                   data.quad.EN.AR.TR,
                   data.quad.EN.ES.DEEP,
                   data.quad.EN.ES.DEEPL,
                   data.quad.EN.ES.GT,
                   data.quad.EN.ES.TR,
                   data.quad.ES,
                   data.quad.ES.EN.DEEP,
                   data.quad.ES.EN.DEEPL,
                   data.quad.ES.EN.GT,
                   data.quad.ES.EN.TR)


# BinQuad
data.binquad <- rbind(data.binquad.AR.EN.DEEP.matconf,
                      data.binquad.AR.EN.DEEP.matcoop,
                      data.binquad.AR.EN.DEEP.verconf,
                      data.binquad.AR.EN.DEEP.vercoop,
                      data.binquad.AR.EN.DEEPL.matcoop,
                      data.binquad.AR.EN.DEEPL.verconf,
                      data.binquad.AR.EN.DEEPL.vercoop,
                      data.binquad.AR.EN.DEEPL.matconf,
                      data.binquad.AR.EN.GT.matconf,
                      data.binquad.AR.EN.GT.matcoop,
                      data.binquad.AR.EN.GT.verconf,
                      data.binquad.AR.EN.GT.vercoop,
                      data.binquad.AR.EN.OPUS.matconf,
                      data.binquad.AR.EN.OPUS.matcoop,
                      data.binquad.AR.EN.OPUS.verconf,
                      data.binquad.AR.EN.OPUS.vercoop,
                      data.binquad.AR.matconf,
                      data.binquad.AR.matcoop,
                      data.binquad.AR.verconf,
                      data.binquad.AR.vercoop,
                      data.binquad.EN.AR.DEEP.matconf,
                      data.binquad.EN.AR.DEEP.matcoop,
                      data.binquad.EN.AR.DEEP.verconf,
                      data.binquad.EN.AR.DEEP.vercoop,
                      data.binquad.EN.AR.DEEPL.matconf,
                      data.binquad.EN.AR.DEEPL.matcoop,
                      data.binquad.EN.AR.DEEPL.verconf,
                      data.binquad.EN.AR.DEEPL.vercoop,
                      data.binquad.EN.AR.GT.matconf,
                      data.binquad.EN.AR.GT.matcoop, 
                      data.binquad.EN.AR.GT.verconf,
                      data.binquad.EN.AR.GT.vercoop,
                      data.binquad.EN.AR.OPUS.matconf,
                      data.binquad.EN.AR.OPUS.matcoop,
                      data.binquad.EN.AR.OPUS.verconf,
                      data.binquad.EN.AR.OPUS.vercoop,
                      data.binquad.EN.ES.DEEP.matcon,
                      data.binquad.EN.ES.DEEP.matcoop,
                      data.binquad.EN.ES.DEEP.vercon,
                      data.binquad.EN.ES.DEEP.vercoop,
                      data.binquad.EN.ES.DEEPL.matconf,
                      data.binquad.EN.ES.DEEPL.matcoop,
                      data.binquad.EN.ES.DEEPL.verconf,
                      data.binquad.EN.ES.DEEPL.vercoop,
                      data.binquad.EN.ES.GT.matconf,
                      data.binquad.EN.ES.GT.matcoop,
                      data.binquad.EN.ES.GT.verconf,
                      data.binquad.EN.ES.GT.vercoop,
                      data.binquad.EN.ES.OPUS.matconf,
                      data.binquad.EN.ES.OPUS.matcoop,
                      data.binquad.EN.ES.OPUS.verconf,
                      data.binquad.EN.ES.OPUS.vercoop,
                      data.binquad.EN.matconf,
                      data.binquad.EN.matcoop,
                      data.binquad.EN.verconf,
                      data.binquad.EN.vercoop,
                      data.binquad.ES.EN.DEEP.matconf,
                      data.binquad.ES.EN.DEEP.matcoop,
                      data.binquad.ES.EN.DEEP.verconf,
                      data.binquad.ES.EN.DEEP.vercoop,
                      data.binquad.ES.EN.DEEPL.matconf,
                      data.binquad.ES.EN.DEEPL.matcoop,
                      data.binquad.ES.EN.DEEPL.verconf,
                      data.binquad.ES.EN.DEEPL.vercoop,
                      data.binquad.ES.EN.GT.matconf,
                      data.binquad.ES.EN.GT.matcoop,
                      data.binquad.ES.EN.GT.verconf,
                      data.binquad.ES.EN.GT.vercoop,
                      data.binquad.ES.EN.OPUS.matconf,
                      data.binquad.ES.EN.OPUS.matcoop,
                      data.binquad.ES.EN.OPUS.verconf,
                      data.binquad.ES.EN.OPUS.vercoop,
                      data.binquad.ES.matconf,
                      data.binquad.ES.matcoop,
                      data.binquad.ES.verconf,
                      data.binquad.ES.vercoop
                      ) 


# Remove working databases
rm(data.bin.AR, data.bin.AR.EN.DEEP, data.bin.AR.EN.DEEPL, data.bin.AR.EN.GT, data.bin.AR.EN.TR, data.bin.EN, data.bin.EN.AR.DEEP, data.bin.EN.AR.DEEPL, data.bin.EN.AR.GT, data.bin.EN.AR.TR, data.bin.EN.ES.DEEP, data.bin.EN.ES.DEEPL, data.bin.EN.ES.GT, data.bin.EN.ES.TR, data.bin.ES, data.bin.ES.EN.DEEP, data.bin.ES.EN.DEEPL, data.bin.ES.EN.GT, data.bin.ES.EN.TR )
rm(data.quad.AR, data.quad.AR.EN.DEEP, data.quad.AR.EN.DEEPL, data.quad.AR.EN.GT, data.quad.AR.EN.TR, data.quad.EN, data.quad.EN.AR.DEEP, data.quad.EN.AR.DEEPL, data.quad.EN.AR.GT, data.quad.EN.AR.TR, data.quad.EN.ES.DEEP, data.quad.EN.ES.DEEPL, data.quad.EN.ES.GT, data.quad.EN.ES.TR, data.quad.ES, data.quad.ES.EN.DEEP, data.quad.ES.EN.DEEPL, data.quad.ES.EN.GT, data.quad.ES.EN.TR)
rm(data.binquad.AR.EN.DEEPL.matconf, data.binquad.AR.EN.DEEPL.matcoop, data.binquad.AR.EN.DEEPL.verconf, data.binquad.AR.EN.DEEPL.vercoop, data.binquad.AR.EN.DEEP.matconf, data.binquad.AR.EN.DEEP.matcoop, data.binquad.AR.EN.DEEP.verconf, data.binquad.AR.EN.DEEP.vercoop, data.binquad.AR.EN.GT.matconf, data.binquad.AR.matconf, data.binquad.AR.matcoop, data.binquad.AR.vercoop, data.binquad.AR.verconf,
   data.binquad.EN.AR.GT.verconf, data.binquad.EN.AR.GT.vercoop,   data.binquad.EN.AR.GT.matconf,   data.binquad.EN.AR.GT.matcoop, data.binquad.EN.AR.OPUS.matconf, data.binquad.EN.AR.OPUS.matcoop, data.binquad.EN.AR.OPUS.verconf, data.binquad.EN.AR.OPUS.vercoop, data.binquad.EN.ES.DEEPL.matconf, data.binquad.EN.ES.DEEPL.matcoop, data.binquad.EN.ES.DEEPL.verconf, data.binquad.EN.ES.DEEPL.vercoop, data.binquad.EN.ES.DEEP.matcon, 
   data.binquad.EN.ES.DEEP.matcoop, data.binquad.EN.ES.DEEP.vercon, data.binquad.EN.ES.DEEP.vercoop, data.binquad.EN.ES.GT.matconf, data.binquad.EN.ES.GT.matcoop, data.binquad.EN.ES.GT.verconf, data.binquad.EN.ES.GT.vercoop, data.binquad.EN.matconf, data.binquad.EN.matcoop, data.binquad.EN.verconf, data.binquad.EN.vercoop, data.binquad.AR.EN.GT.matcoop, 
   data.binquad.AR.EN.GT.verconf, data.binquad.AR.EN.GT.vercoop, data.binquad.AR.EN.OPUS.matconf, data.binquad.AR.EN.OPUS.matcoop, data.binquad.AR.EN.OPUS.verconf, data.binquad.AR.EN.OPUS.vercoop, data.binquad.EN.AR.DEEPL.matconf, data.binquad.EN.AR.DEEPL.matcoop, data.binquad.EN.AR.DEEPL.verconf, data.binquad.EN.AR.DEEPL.vercoop, data.binquad.EN.AR.DEEP.matconf, 
   data.binquad.EN.AR.DEEP.matcoop, data.binquad.EN.AR.DEEP.verconf, data.binquad.EN.AR.DEEP.vercoop, data.binquad.EN.ES.OPUS.matconf, data.binquad.EN.ES.OPUS.matcoop, data.binquad.EN.ES.OPUS.verconf, data.binquad.EN.ES.OPUS.vercoop, data.binquad.ES.matconf, data.binquad.ES.matcoop, data.binquad.ES.verconf, data.binquad.ES.vercoop, data.binquad.ES.EN.DEEPL.matconf, data.binquad.ES.EN.DEEPL.matcoop, data.binquad.ES.EN.DEEPL.verconf, 
   data.binquad.ES.EN.DEEPL.vercoop, data.binquad.ES.EN.DEEP.matconf, data.binquad.ES.EN.DEEP.matcoop, data.binquad.ES.EN.DEEP.verconf, data.binquad.ES.EN.DEEP.vercoop, data.binquad.ES.EN.GT.matconf, data.binquad.ES.EN.GT.matcoop, data.binquad.ES.EN.GT.verconf, data.binquad.ES.EN.GT.vercoop, data.binquad.ES.EN.OPUS.matconf, data.binquad.ES.EN.OPUS.matcoop, 
   data.binquad.ES.EN.OPUS.verconf, data.binquad.ES.EN.OPUS.vercoop)













# RENAME LANGUAGES AND MODELS  --------------------------------------------------

## Bin -------------

table(data.bin$data_name)
table(data.bin$model_name)


# Language
data.bin <- data.bin %>%
  mutate(lang = case_when(
    data_name=="ar_binary" ~ "AR",
    data_name=="en_binary" ~ "EN",
    data_name=="es_binary" ~ "ES",
    data_name=="ar_en_DEEP_binary" ~ "EN",
    data_name=="ar_en_DEEPL_binary" ~ "EN",
    data_name=="Binary_AR_EN_GT" ~ "EN",
    data_name=="Binary_AR_EN_TR" ~ "EN",
    data_name=="Binary_EN_AR_GT" ~ "AR",
    data_name=="Binary_EN_AR_TR" ~ "AR",
    data_name=="Binary_EN_ES_GT" ~ "ES",
    data_name=="Binary_EN_ES_TR" ~ "ES",
    data_name=="Binary_ES_EN_GT" ~ "EN",
    data_name=="Binary_ES_EN_TR" ~ "EN",
    data_name=="en_ar_DEEP_binary" ~ "AR",
    data_name=="en_ar_DEEPL_binary" ~ "AR",
    data_name=="en_es_DEEP_binary" ~ "ES",
    data_name=="en_es_DEEPL_binary" ~ "ES",
    data_name=="es_en_DEEP_binary" ~ "EN",
    data_name=="es_en_DEEPL_binary" ~ "EN", 
    TRUE ~ data_name
  ))

table(data.bin$lang)

# Language
data.bin <- data.bin %>%
  mutate(trans.lang = case_when(
    data_name=="ar_binary" ~ "AR Native",
    data_name=="ar_en_DEEP_binary" ~ "AR to EN",
    data_name=="ar_en_DEEPL_binary" ~ "AR to EN",
    data_name=="Binary_AR_EN_GT" ~ "AR to EN",
    data_name=="Binary_AR_EN_TR" ~ "AR to EN",
    data_name=="Binary_EN_AR_GT" ~ "EN to AR",
    data_name=="Binary_EN_AR_TR" ~ "EN to AR",
    data_name=="Binary_EN_ES_GT" ~ "EN to ES",
    data_name=="Binary_EN_ES_TR" ~ "EN to ES",
    data_name=="Binary_ES_EN_GT" ~ "ES to EN",
    data_name=="Binary_ES_EN_TR" ~ "ES to EN",
    data_name=="en_ar_DEEP_binary" ~ "EN to AR",
    data_name=="en_ar_DEEPL_binary" ~ "EN to AR",
    data_name=="en_binary" ~ "EN Native",
    data_name=="en_es_DEEP_binary" ~ "EN to ES",
    data_name=="en_es_DEEPL_binary" ~ "EN to ES",
    data_name=="es_binary" ~ "ES Native",
    data_name=="es_en_DEEP_binary" ~ "ES to EN",
    data_name=="es_en_DEEPL_binary" ~ "ES to EN", 
    TRUE ~ data_name
  ))

table(data.bin$trans.lang)



# Translation tool
data.bin <- data.bin %>%
  mutate(mt.tool = case_when(
    data_name=="ar_binary" ~ "Native",
    data_name=="ar_en_DEEP_binary" ~ "Deep",
    data_name=="ar_en_DEEPL_binary" ~ "DeepL",
    data_name=="Binary_AR_EN_GT" ~ "GT",
    data_name=="Binary_AR_EN_TR" ~ "OPUS",
    data_name=="Binary_EN_AR_GT" ~ "GT",
    data_name=="Binary_EN_AR_TR" ~ "OPUS",
    data_name=="Binary_EN_ES_GT" ~ "GT",
    data_name=="Binary_EN_ES_TR" ~ "OPUS",
    data_name=="Binary_ES_EN_GT" ~ "GT",
    data_name=="Binary_ES_EN_TR" ~ "OPUS",
    data_name=="en_ar_DEEP_binary" ~ "Deep",
    data_name=="en_ar_DEEPL_binary" ~ "DeepL",
    data_name=="en_binary" ~ "Native",
    data_name=="en_es_DEEP_binary" ~ "Deep",
    data_name=="en_es_DEEPL_binary" ~ "DeepL",
    data_name=="es_binary" ~ "Native",
    data_name=="es_en_DEEP_binary" ~ "Deep",
    data_name=="es_en_DEEPL_binary" ~ "DeepL", 
    TRUE ~ data_name
  ))

table(data.bin$mt.tool)


# Model
## Attention, some models have to be deleted as duplicates (?)
data.bin <- data.bin %>%
  mutate(model = case_when(
    model_name=="ConfliBERT-Arabic-multilingual-cased-v2_EN_AR_TR" ~ "ConfliBERT cont case",
    model_name=="ConfliBERT-Arabic-multilingual-cased-v2" ~ "ConfliBERT cont case",
    model_name=="ConfliBERT-Arabic-multilingual-uncased-v2_EN_AR_TR" ~ "ConfliBERT cont unc",
    model_name=="ConfliBERT-Arabic-multilingual-uncased-v2" ~ "ConfliBERT cont unc",
    model_name=="ConfliBERT-Spanish-multilingual-cased-v2_en_es_tr" ~ "ConfliBERT cont case",
    model_name=="ConfliBERT-Spanish-multilingual-cased-v2" ~ "ConfliBERT cont case",
    model_name=="ConfliBERT-Spanish-multilingual-uncased-v2_en_es_tr" ~ "ConfliBERT cont unc",
    model_name=="ConfliBERT-Spanish-multilingual-uncased-v2" ~ "ConfliBERT cont unc",
    model_name=="ConfliBERT-cont-cased-es_en_TR" ~ "ConfliBERT cont case",
    model_name=="ConfliBERT-cont-cased" ~ "ConfliBERT cont case",
    model_name=="ConfliBERT-cont-uncased-es_en_TR" ~ "ConfliBERT cont unc",
    model_name=="ConfliBERT-cont-uncased" ~ "ConfliBERT cont unc", 
    TRUE ~ model_name
  ))

table(data.bin$model)


# Case
data.bin <- data.bin %>%
  mutate(case = case_when(
    model_name=="ConfliBERT-Arabic-multilingual-cased-v2" ~ "Cased",
    model_name=="ConfliBERT-Arabic-multilingual-cased-v2_EN_AR_TR" ~ "Cased",
    model_name=="ConfliBERT-Arabic-multilingual-uncased-v2" ~ "Uncased",
    model_name=="ConfliBERT-Arabic-multilingual-uncased-v2_EN_AR_TR" ~ "Uncased",
    model_name=="ConfliBERT-Spanish-multilingual-cased-v2" ~ "Cased",
    model_name=="ConfliBERT-Spanish-multilingual-cased-v2_en_es_tr" ~ "Cased",
    model_name=="ConfliBERT-Spanish-multilingual-uncased-v2" ~ "Uncased",
    model_name=="ConfliBERT-Spanish-multilingual-uncased-v2_en_es_tr" ~ "Uncased",
    model_name=="ConfliBERT-cont-cased" ~ "Cased",
    model_name=="ConfliBERT-cont-cased-es_en_TR" ~ "Cased",
    model_name=="ConfliBERT-cont-uncased" ~ "Uncased",
    model_name=="ConfliBERT-cont-uncased-es_en_TR" ~ "Uncased", 
    TRUE ~ model_name
  ))

table(data.bin$case)


# Delete duplicate models
#data.bin <- data.bin %>% filter(model!="Delete")



# Check names
table(data.bin$lang)
table(data.bin$mt.tool)
table(data.bin$model)
table(data.bin$trans.lang,data.bin$model)



## Quadclass -------------

table(data.quad$data_name)
table(data.quad$model_name)


# Lang
data.quad <- data.quad %>%
  mutate(lang = case_when(
    data_name=="ar_en_DEEP_quad" ~ "EN", 
    data_name=="ar_en_DEEPL_quad" ~ "EN",  
    data_name=="en_ar_DEEP_quad" ~ "AR", 
    data_name=="en_ar_DEEPL_quad" ~ "AR",  
    data_name=="en_es_DEEP_quad" ~ "ES", 
    data_name=="en_es_DEEPL_quad" ~ "ES",  
    data_name=="es_en_DEEP_quad" ~ "EN", 
    data_name=="es_en_DEEPL_quad" ~ "EN",          
    data_name=="Quad_AR" ~ "AR",    
    data_name=="Quad_AR_EN_GT" ~ "EN", 
    data_name=="Quad_AR_EN_TR" ~ "EN",          
    data_name=="Quad_EN" ~ "EN",    
    data_name=="Quad_EN_AR_GT" ~ "AR",    
    data_name=="Quad_EN_AR_TR" ~ "AR",    
    data_name=="Quad_EN_ES_GT" ~ "ES", 
    data_name=="Quad_EN_ES_TR" ~ "ES",          
    data_name=="Quad_ES" ~ "ES",
    data_name=="Quad_ES_EN_GT" ~ "EN",   
    data_name=="Quad_ES_EN_TR" ~ "EN", 
    TRUE ~ data_name
  ))

table(data.quad$lang)



# Language
data.quad <- data.quad %>%
  mutate(trans.lang = case_when(
    data_name=="ar_en_DEEP_quad" ~ "AR to EN", 
    data_name=="ar_en_DEEPL_quad" ~ "AR to EN",  
    data_name=="en_ar_DEEP_quad" ~ "EN to AR", 
    data_name=="en_ar_DEEPL_quad" ~ "EN to AR",  
    data_name=="en_es_DEEP_quad" ~ "EN to ES", 
    data_name=="en_es_DEEPL_quad" ~ "EN to ES",  
    data_name=="es_en_DEEP_quad" ~ "ES to EN", 
    data_name=="es_en_DEEPL_quad" ~ "ES to EN",          
    data_name=="Quad_AR" ~ "AR Native",    
    data_name=="Quad_AR_EN_GT" ~ "AR to EN", 
    data_name=="Quad_AR_EN_TR" ~ "AR to EN",          
    data_name=="Quad_EN" ~ "EN Native",    
    data_name=="Quad_EN_AR_GT" ~ "EN to AR",    
    data_name=="Quad_EN_AR_TR" ~ "EN to AR",    
    data_name=="Quad_EN_ES_GT" ~ "EN to ES", 
    data_name=="Quad_EN_ES_TR" ~ "EN to ES",          
    data_name=="Quad_ES" ~ "ES Native",
    data_name=="Quad_ES_EN_GT" ~ "ES to EN",   
    data_name=="Quad_ES_EN_TR" ~ "ES to EN", 
    TRUE ~ data_name
  ))

table(data.quad$trans.lang)



# Translation tool
data.quad <- data.quad %>%
  mutate(mt.tool = case_when(
    data_name=="ar_en_DEEP_quad" ~ "Deep", 
    data_name=="ar_en_DEEPL_quad" ~ "DeepL",  
    data_name=="en_ar_DEEP_quad" ~ "Deep", 
    data_name=="en_ar_DEEPL_quad" ~ "DeepL",  
    data_name=="en_es_DEEP_quad" ~ "Deep", 
    data_name=="en_es_DEEPL_quad" ~ "DeepL",  
    data_name=="es_en_DEEP_quad" ~ "Deep", 
    data_name=="es_en_DEEPL_quad" ~ "DeepL",          
    data_name=="Quad_AR" ~ "Native",    
    data_name=="Quad_AR_EN_GT" ~ "GT", 
    data_name=="Quad_AR_EN_TR" ~ "OPUS",          
    data_name=="Quad_EN" ~ "Native",    
    data_name=="Quad_EN_AR_GT" ~ "GT",    
    data_name=="Quad_EN_AR_TR" ~ "OPUS",    
    data_name=="Quad_EN_ES_GT" ~ "GT", 
    data_name=="Quad_EN_ES_TR" ~ "OPUS",          
    data_name=="Quad_ES" ~ "Native",
    data_name=="Quad_ES_EN_GT" ~ "GT",   
    data_name=="Quad_ES_EN_TR" ~ "OPUS", 
    TRUE ~ data_name
  ))

table(data.quad$mt.tool)


# Model
## Attention, some had to be deleted due to duplicates (?)
data.quad <- data.quad %>%
  mutate(model = case_when(
    model_name=="ConfliBERT-Arabic-multilingual-cased-v2" ~ "ConfliBERT cont case",
    model_name=="ConfliBERT-Arabic-multilingual-cased-v2-Quad-en-ar" ~ "ConfliBERT cont case",
    model_name=="ConfliBERT-Arabic-multilingual-uncased-v2" ~ "ConfliBERT cont unc",
    model_name=="ConfliBERT-cont-cased" ~ "ConfliBERT cont case",
    model_name=="ConfliBERT-cont-cased-quad-ar-en" ~ "ConfliBERT cont case",
    model_name=="ConfliBERT-cont-uncased" ~ "ConfliBERT cont unc",
    model_name=="ConfliBERT-cont-uncased-quad-ar-en" ~ "ConfliBERT cont unc",
    model_name=="ConfliBERT-Spanish-multilingual-cased-v2" ~ "ConfliBERT cont case",
    model_name=="ConfliBERT-Spanish-multilingual-cased-v2-Quad-en-es" ~ "ConfliBERT cont case",
    model_name=="ConfliBERT-Spanish-multilingual-uncased-v2" ~ "ConfliBERT cont unc",
    model_name=="ConfliBERT-Spanish-multilingual-uncased-v2-Quad-en-es" ~ "ConfliBERT cont unc", 
    TRUE ~ model_name
  ))

table(data.quad$model)


# Case
data.quad <- data.quad %>%
  mutate(case = case_when(
    model_name=="ConfliBERT-Arabic-multilingual-cased-v2" ~ "Cased",
    model_name=="ConfliBERT-Arabic-multilingual-cased-v2-Quad-en-ar" ~ "Cased",
    model_name=="ConfliBERT-Arabic-multilingual-uncased-v2" ~ "Uncased",
    model_name=="ConfliBERT-cont-cased" ~ "Cased",
    model_name=="ConfliBERT-cont-cased-quad-ar-en" ~ "Cased",
    model_name=="ConfliBERT-cont-uncased" ~ "Uncased",
    model_name=="ConfliBERT-cont-uncased-quad-ar-en" ~ "Uncased",
    model_name=="ConfliBERT-Spanish-multilingual-cased-v2" ~ "Cased",
    model_name=="ConfliBERT-Spanish-multilingual-cased-v2-Quad-en-es" ~ "Cased",
    model_name=="ConfliBERT-Spanish-multilingual-uncased-v2" ~ "Uncased",
    model_name=="ConfliBERT-Spanish-multilingual-uncased-v2-Quad-en-es" ~ "Uncased", 
    TRUE ~ model_name
  ))

table(data.quad$case)



# Delete duplicate models
#data.quad <- data.quad %>% filter(model!="Delete")

# Check names
table(data.quad$lang)
table(data.quad$mt.tool)
table(data.quad$model)





## BinQuad -------------

table(data.binquad$data_name)
table(data.binquad$model_name)



# Language
data.binquad <- data.binquad %>%
  mutate(lang = case_when(
    data_name=="BinQuad_AR_EN_DEEPL_matconf" ~ "EN", 
    data_name=="BinQuad_AR_EN_DEEPL_matcoop" ~ "EN", 
    data_name=="BinQuad_AR_EN_DEEPL_verconf" ~ "EN", 
    data_name=="BinQuad_AR_EN_DEEPL_vercoop" ~ "EN", 
    data_name=="BinQuad_AR_EN_DEEP_matconf" ~ "EN", 
    data_name=="BinQuad_AR_EN_DEEP_matcoop" ~ "EN", 
    data_name=="BinQuad_AR_EN_DEEP_verconf" ~ "EN", 
    data_name=="BinQuad_AR_EN_DEEP_vercoop" ~ "EN", 
    data_name=="BinQuad_AR_EN_GOOGLE_matconf" ~ "EN", 
    data_name=="BinQuad_AR_EN_GOOGLE_matcoop" ~ "EN",  # new
    data_name=="BinQuad_AR_EN_GOOGLE_verconf" ~ "EN",  # new
    data_name=="BinQuad_AR_EN_GOOGLE_vercoop" ~ "EN",  # new
    data_name=="BinQuad_AR_matconf" ~ "AR", 
    data_name=="BinQuad_AR_matcoop" ~ "AR", 
    data_name=="BinQuad_AR_vercoop" ~ "AR", 
    data_name=="BinQuad_AR_verconf" ~ "AR",            # new
    data_name=="BinQuad_EN_AR_GOOGLE_matconf" ~ "AR", 
    data_name=="BinQuad_EN_AR_GOOGLE_matcoop" ~ "AR", 
    data_name=="BinQuad_EN_AR_GOOGLE_verconf" ~ "AR", 
    data_name=="BinQuad_EN_AR_GOOGLE_vercoop" ~ "AR", 
    data_name=="BinQuad_EN_AR_TRANSFORMERS_matconf" ~ "AR", 
    data_name=="BinQuad_EN_AR_TRANSFORMERS_matcoop" ~ "AR", 
    data_name=="BinQuad_EN_AR_TRANSFORMERS_verconf" ~ "AR", 
    data_name=="BinQuad_EN_AR_TRANSFORMERS_vercoop" ~ "AR", 
    data_name=="BinQuad_EN_ES_DEEPL_matconf" ~ "ES", 
    data_name=="BinQuad_EN_ES_DEEPL_matcoop" ~ "ES", 
    data_name=="BinQuad_EN_ES_DEEPL_verconf" ~ "ES", 
    data_name=="BinQuad_EN_ES_DEEPL_vercoop" ~ "ES", 
    data_name=="BinQuad_EN_ES_DEEP_matconf" ~ "ES", 
    data_name=="BinQuad_EN_ES_DEEP_matcoop" ~ "ES",  
    data_name=="BinQuad_EN_ES_DEEP_verconf" ~ "ES", 
    data_name=="BinQuad_EN_ES_DEEP_vercoop" ~ "ES", 
    data_name=="BinQuad_EN_ES_GOOGLE_matconf" ~ "ES", 
    data_name=="BinQuad_EN_ES_GOOGLE_matcoop" ~ "ES", 
    data_name=="BinQuad_EN_ES_GOOGLE_verconf" ~ "ES", 
    data_name=="BinQuad_EN_ES_GOOGLE_vercoop" ~ "ES", 
    data_name=="BinQuad_EN_matconf" ~ "EN", 
    data_name=="BinQuad_EN_matcoop" ~ "EN", 
    data_name=="BinQuad_EN_verconf" ~ "EN", 
    data_name=="BinQuad_EN_vercoop" ~ "EN", 
    data_name=="BinQuad_AR_matconf" ~ "AR",                # new 
    data_name=="BinQuad_AR_matcoop" ~ "AR",                # new 
    data_name=="BinQuad_AR_verconf" ~ "AR",                # new 
    data_name=="BinQuad_AR_vercoop" ~ "AR",                # new
    data_name=="ar_en_GOOGLE_binary_matcoop" ~ "EN", 
    data_name=="ar_en_GOOGLE_binary_verconf" ~ "EN", 
    data_name=="ar_en_GOOGLE_binary_vercoop" ~ "EN", 
    data_name=="ar_en_GOOGLE_binary_verconf" ~ "EN",       # new
    data_name=="ar_en_TRANSFORMERS_binary_matconf" ~ "EN", 
    data_name=="ar_en_TRANSFORMERS_binary_matcoop" ~ "EN", 
    data_name=="ar_en_TRANSFORMERS_binary_verconf" ~ "EN", 
    data_name=="ar_en_TRANSFORMERS_binary_vercoop" ~ "EN", 
    data_name=="en_ar_DEEPL_binary_matconf" ~ "AR", 
    data_name=="en_ar_DEEPL_binary_matcoop" ~ "AR", 
    data_name=="en_ar_DEEPL_binary_verconf" ~ "AR", 
    data_name=="en_ar_DEEPL_binary_vercoop" ~ "AR", 
    data_name=="en_ar_DEEP_binary_matconf" ~ "AR", 
    data_name=="en_ar_DEEP_binary_matcoop" ~ "AR", 
    data_name=="en_ar_DEEP_binary_verconf" ~ "AR",          # new
    data_name=="en_ar_DEEP_binary_vercoop" ~ "AR",         # new
    data_name=="en_es_TRANSFORMERS_binary_matconf" ~ "ES", 
    data_name=="en_es_TRANSFORMERS_binary_matcoop" ~ "ES", 
    data_name=="en_es_TRANSFORMERS_binary_verconf" ~ "ES", 
    data_name=="en_es_TRANSFORMERS_binary_vercoop" ~ "ES", 
    data_name=="es_binary_matconf" ~ "ES", 
    data_name=="es_binary_matcoop" ~ "ES", 
    data_name=="es_binary_verconf" ~ "ES", 
    data_name=="es_binary_vercoop" ~ "ES", 
    data_name=="es_en_DEEPL_binary_matconf" ~ "EN", 
    data_name=="es_en_DEEPL_binary_matcoop" ~ "EN", 
    data_name=="es_en_DEEPL_binary_verconf" ~ "EN", 
    data_name=="es_en_DEEPL_binary_vercoop" ~ "EN", 
    data_name=="es_en_DEEP_binary_matconf" ~ "EN", 
    data_name=="es_en_DEEP_binary_matcoop" ~ "EN", 
    data_name=="es_en_DEEP_binary_verconf" ~ "EN", 
    data_name=="es_en_DEEP_binary_vercoop" ~ "EN", 
    data_name=="es_en_GOOGLE_binary_matconf" ~ "EN", 
    data_name=="es_en_GOOGLE_binary_matcoop" ~ "EN", 
    data_name=="es_en_GOOGLE_binary_verconf" ~ "EN", 
    data_name=="es_en_GOOGLE_binary_vercoop" ~ "EN", 
    data_name=="es_en_TRANSOFMERS_binary_matconf" ~ "EN", 
    data_name=="es_en_TRANSOFMERS_binary_matcoop" ~ "EN", 
    data_name=="es_en_TRANSOFMERS_binary_verconf" ~ "EN", 
    data_name=="es_en_TRANSOFMERS_binary_vercoop" ~ "EN" , 
    data_name=="BinQuad_EN_AR_DEEP_verconf" ~ "AR" , 
    data_name=="BinQuad_EN_AR_DEEP_vercoop" ~ "AR" , 
    TRUE ~ data_name
  ))

table(data.binquad$lang)






# Translation language
data.binquad <- data.binquad %>%
  mutate(trans.lang = case_when(
    data_name=="BinQuad_AR_EN_DEEPL_matconf" ~ "AR to EN", 
    data_name=="BinQuad_AR_EN_DEEPL_matcoop" ~ "AR to EN", 
    data_name=="BinQuad_AR_EN_DEEPL_verconf" ~ "AR to EN", 
    data_name=="BinQuad_AR_EN_DEEPL_vercoop" ~ "AR to EN", 
    data_name=="BinQuad_AR_EN_DEEP_matconf" ~ "AR to EN", 
    data_name=="BinQuad_AR_EN_DEEP_matcoop" ~ "AR to EN", 
    data_name=="BinQuad_AR_EN_DEEP_verconf" ~ "AR to EN", 
    data_name=="BinQuad_AR_EN_DEEP_vercoop" ~ "AR to EN", 
    data_name=="BinQuad_AR_EN_GOOGLE_matconf" ~ "AR to EN", 
    data_name=="BinQuad_AR_EN_GOOGLE_matcoop" ~ "AR to EN",      # new
    data_name=="BinQuad_AR_EN_GOOGLE_verconf" ~ "AR to EN",      # new
    data_name=="BinQuad_AR_EN_GOOGLE_vercoop" ~ "AR to EN",      # new
    data_name=="BinQuad_AR_matconf" ~ "AR Native", 
    data_name=="BinQuad_AR_matconf" ~ "AR Native",                # new 
    data_name=="BinQuad_AR_matcoop" ~ "AR Native", 
    data_name=="BinQuad_AR_matcoop" ~ "AR Native",                # new 
    data_name=="BinQuad_AR_verconf" ~ "AR Native",                # new
    data_name=="BinQuad_AR_vercoop" ~ "AR Native", 
    data_name=="BinQuad_AR_vercoop" ~ "AR Native",                # new
    data_name=="BinQuad_EN_AR_DEEP_verconf" ~ "EN to AR",         # new, 
    data_name=="BinQuad_EN_AR_DEEP_vercoop" ~ "EN to AR",         # new, 
    data_name=="BinQuad_EN_AR_GOOGLE_matconf" ~ "EN to AR",
    data_name=="BinQuad_EN_AR_GOOGLE_matcoop" ~ "EN to AR",
    data_name=="BinQuad_EN_AR_GOOGLE_verconf" ~ "EN to AR", 
    data_name=="BinQuad_EN_AR_GOOGLE_vercoop" ~ "EN to AR", 
    data_name=="BinQuad_EN_AR_TRANSFORMERS_matconf" ~ "EN to AR", 
    data_name=="BinQuad_EN_AR_TRANSFORMERS_matcoop" ~ "EN to AR", 
    data_name=="BinQuad_EN_AR_TRANSFORMERS_verconf" ~ "EN to AR", 
    data_name=="BinQuad_EN_AR_TRANSFORMERS_vercoop" ~ "EN to AR", 
    data_name=="BinQuad_EN_ES_DEEPL_matconf" ~ "EN to ES", 
    data_name=="BinQuad_EN_ES_DEEPL_matcoop" ~ "EN to ES", 
    data_name=="BinQuad_EN_ES_DEEPL_verconf" ~ "EN to ES", 
    data_name=="BinQuad_EN_ES_DEEPL_vercoop" ~ "EN to ES", 
    data_name=="BinQuad_EN_ES_DEEP_matconf" ~ "EN to ES", 
    data_name=="BinQuad_EN_ES_DEEP_matcoop" ~ "EN to ES" ,
    data_name=="BinQuad_EN_ES_DEEP_verconf" ~ "EN to ES", 
    data_name=="BinQuad_EN_ES_DEEP_vercoop" ~ "EN to ES", 
    data_name=="BinQuad_EN_ES_GOOGLE_matconf" ~ "EN to ES", 
    data_name=="BinQuad_EN_ES_GOOGLE_matcoop" ~ "EN to ES", 
    data_name=="BinQuad_EN_ES_GOOGLE_verconf" ~ "EN to ES", 
    data_name=="BinQuad_EN_ES_GOOGLE_vercoop" ~ "EN to ES", 
    data_name=="BinQuad_EN_matconf" ~ "EN Native", 
    data_name=="BinQuad_EN_matcoop" ~ "EN Native", 
    data_name=="BinQuad_EN_verconf" ~ "EN Native", 
    data_name=="BinQuad_EN_vercoop" ~ "EN Native", 
    data_name=="ar_en_GOOGLE_binary_matcoop" ~ "AR to EN", 
    data_name=="ar_en_GOOGLE_binary_verconf" ~ "AR to EN", 
    data_name=="ar_en_GOOGLE_binary_verconf" ~ "AR to EN",       # new
    data_name=="ar_en_GOOGLE_binary_vercoop" ~ "AR to EN", 
    data_name=="ar_en_TRANSFORMERS_binary_matconf" ~ "AR to EN", 
    data_name=="ar_en_TRANSFORMERS_binary_matcoop" ~ "AR to EN", 
    data_name=="ar_en_TRANSFORMERS_binary_verconf" ~ "AR to EN", 
    data_name=="ar_en_TRANSFORMERS_binary_vercoop" ~ "AR to EN", 
    data_name=="en_ar_DEEPL_binary_matconf" ~ "EN to AR", 
    data_name=="en_ar_DEEPL_binary_matcoop" ~ "EN to AR", 
    data_name=="en_ar_DEEPL_binary_verconf" ~ "EN to AR", 
    data_name=="en_ar_DEEPL_binary_vercoop" ~ "EN to AR", 
    data_name=="en_ar_DEEP_binary_matconf" ~ "EN to AR", 
    data_name=="en_ar_DEEP_binary_matcoop" ~ "EN to AR", 
    data_name=="en_ar_DEEP_binary_verconf" ~ "EN to AR",         # new
    data_name=="en_ar_DEEP_binary_vercoop" ~ "EN to AR",         # new, 
    data_name=="en_es_TRANSFORMERS_binary_matconf" ~ "EN to ES", 
    data_name=="en_es_TRANSFORMERS_binary_matcoop" ~ "EN to ES", 
    data_name=="en_es_TRANSFORMERS_binary_verconf" ~ "EN to ES", 
    data_name=="en_es_TRANSFORMERS_binary_vercoop" ~ "EN to ES", 
    data_name=="es_binary_matconf" ~ "ES Native", 
    data_name=="es_binary_matcoop" ~ "ES Native", 
    data_name=="es_binary_verconf" ~ "ES Native", 
    data_name=="es_binary_vercoop" ~ "ES Native", 
    data_name=="es_en_DEEPL_binary_matconf" ~ "ES to EN", 
    data_name=="es_en_DEEPL_binary_matcoop" ~ "ES to EN", 
    data_name=="es_en_DEEPL_binary_verconf" ~ "ES to EN", 
    data_name=="es_en_DEEPL_binary_vercoop" ~ "ES to EN", 
    data_name=="es_en_DEEP_binary_matconf" ~ "ES to EN", 
    data_name=="es_en_DEEP_binary_matcoop" ~ "ES to EN", 
    data_name=="es_en_DEEP_binary_verconf" ~ "ES to EN", 
    data_name=="es_en_DEEP_binary_vercoop" ~ "ES to EN", 
    data_name=="es_en_GOOGLE_binary_matconf" ~ "ES to EN", 
    data_name=="es_en_GOOGLE_binary_matcoop" ~ "ES to EN", 
    data_name=="es_en_GOOGLE_binary_verconf" ~ "ES to EN", 
    data_name=="es_en_GOOGLE_binary_vercoop" ~ "ES to EN", 
    data_name=="es_en_TRANSOFMERS_binary_matconf" ~ "ES to EN", 
    data_name=="es_en_TRANSOFMERS_binary_matcoop" ~ "ES to EN", 
    data_name=="es_en_TRANSOFMERS_binary_verconf" ~ "ES to EN", 
    data_name=="es_en_TRANSOFMERS_binary_vercoop" ~ "ES to EN", 
    TRUE ~ data_name
  ))

table(data.binquad$trans.lang)



# Translation tool
data.binquad <- data.binquad %>%
  mutate(mt.tool = case_when(
    data_name=="BinQuad_AR_EN_DEEPL_matconf" ~ "DeepL", 
    data_name=="BinQuad_AR_EN_DEEPL_matcoop" ~ "DeepL", 
    data_name=="BinQuad_AR_EN_DEEPL_verconf" ~ "DeepL", 
    data_name=="BinQuad_AR_EN_DEEPL_vercoop" ~ "DeepL", 
    data_name=="BinQuad_AR_EN_DEEP_matconf" ~ "Deep", 
    data_name=="BinQuad_AR_EN_DEEP_matcoop" ~ "Deep", 
    data_name=="BinQuad_AR_EN_DEEP_verconf" ~ "Deep", 
    data_name=="BinQuad_AR_EN_DEEP_vercoop" ~ "Deep", 
    data_name=="BinQuad_AR_EN_GOOGLE_matconf" ~ "GT", 
    data_name=="BinQuad_AR_EN_GOOGLE_matcoop" ~ "GT",      # new
    data_name=="BinQuad_AR_EN_GOOGLE_verconf" ~ "GT",      # new
    data_name=="BinQuad_AR_EN_GOOGLE_vercoop" ~ "GT",      # new
    data_name=="BinQuad_AR_matconf" ~ "Native", 
    data_name=="BinQuad_AR_matconf" ~ "Native",                # new 
    data_name=="BinQuad_AR_matcoop" ~ "Native", 
    data_name=="BinQuad_AR_matcoop" ~ "Native",                # new 
    data_name=="BinQuad_AR_verconf" ~ "Native",                # new
    data_name=="BinQuad_AR_vercoop" ~ "Native", 
    data_name=="BinQuad_AR_vercoop" ~ "Native",                # new 
    data_name=="BinQuad_EN_AR_DEEP_verconf" ~ "Deep",         # new,
    data_name=="BinQuad_EN_AR_DEEP_vercoop" ~ "Deep",         # new,
    data_name=="BinQuad_EN_AR_DEEP_matconf" ~ "Deep",         # new,
    data_name=="BinQuad_EN_AR_DEEP_matcoop" ~ "Deep",         # new,
    data_name=="BinQuad_EN_AR_GOOGLE_matconf" ~ "GT", 
    data_name=="BinQuad_EN_AR_GOOGLE_matcoop" ~ "GT", 
    data_name=="BinQuad_EN_AR_GOOGLE_verconf" ~ "GT", 
    data_name=="BinQuad_EN_AR_GOOGLE_vercoop" ~ "GT", 
    data_name=="BinQuad_EN_AR_TRANSFORMERS_matconf" ~ "OPUS", 
    data_name=="BinQuad_EN_AR_TRANSFORMERS_matcoop" ~ "OPUS", 
    data_name=="BinQuad_EN_AR_TRANSFORMERS_verconf" ~ "OPUS", 
    data_name=="BinQuad_EN_AR_TRANSFORMERS_vercoop" ~ "OPUS", 
    data_name=="BinQuad_EN_ES_DEEPL_matconf" ~ "DeepL", 
    data_name=="BinQuad_EN_ES_DEEPL_matcoop" ~ "DeepL", 
    data_name=="BinQuad_EN_ES_DEEPL_verconf" ~ "DeepL", 
    data_name=="BinQuad_EN_ES_DEEPL_vercoop" ~ "DeepL", 
    data_name=="BinQuad_EN_ES_DEEP_matconf" ~ "Deep", 
    data_name=="BinQuad_EN_ES_DEEP_matcoop" ~ "Deep",
    data_name=="BinQuad_EN_ES_DEEP_verconf" ~ "Deep", 
    data_name=="BinQuad_EN_ES_DEEP_vercoop" ~ "Deep", 
    data_name=="BinQuad_EN_ES_GOOGLE_matconf" ~ "GT", 
    data_name=="BinQuad_EN_ES_GOOGLE_matcoop" ~ "GT", 
    data_name=="BinQuad_EN_ES_GOOGLE_verconf" ~ "GT", 
    data_name=="BinQuad_EN_ES_GOOGLE_vercoop" ~ "GT", 
    data_name=="BinQuad_EN_matconf" ~ "Native", 
    data_name=="BinQuad_EN_matcoop" ~ "Native", 
    data_name=="BinQuad_EN_verconf" ~ "Native", 
    data_name=="BinQuad_EN_vercoop" ~ "Native", 
    data_name=="ar_en_GOOGLE_binary_matcoop" ~ "GT", 
    data_name=="ar_en_GOOGLE_binary_verconf" ~ "GT", 
    data_name=="ar_en_GOOGLE_binary_verconf" ~ "GT",       # new
    data_name=="ar_en_GOOGLE_binary_vercoop" ~ "GT", 
    data_name=="ar_en_TRANSFORMERS_binary_matconf" ~ "OPUS", 
    data_name=="ar_en_TRANSFORMERS_binary_matcoop" ~ "OPUS", 
    data_name=="ar_en_TRANSFORMERS_binary_verconf" ~ "OPUS", 
    data_name=="ar_en_TRANSFORMERS_binary_vercoop" ~ "OPUS", 
    data_name=="en_ar_DEEPL_binary_matconf" ~ "DeepL", 
    data_name=="en_ar_DEEPL_binary_matcoop" ~ "DeepL", 
    data_name=="en_ar_DEEPL_binary_verconf" ~ "DeepL", 
    data_name=="en_ar_DEEPL_binary_vercoop" ~ "DeepL", 
    data_name=="en_ar_DEEP_binary_matconf" ~ "Deep", 
    data_name=="en_ar_DEEP_binary_matcoop" ~ "Deep", 
    data_name=="en_ar_DEEP_binary_verconf" ~ "Deep",         # new
    data_name=="en_ar_DEEP_binary_vercoop" ~ "Deep",         # new, 
    data_name=="en_es_TRANSFORMERS_binary_matconf" ~ "OPUS", 
    data_name=="en_es_TRANSFORMERS_binary_matcoop" ~ "OPUS", 
    data_name=="en_es_TRANSFORMERS_binary_verconf" ~ "OPUS", 
    data_name=="en_es_TRANSFORMERS_binary_vercoop" ~ "OPUS", 
    data_name=="es_binary_matconf" ~ "Native", 
    data_name=="es_binary_matcoop" ~ "Native", 
    data_name=="es_binary_verconf" ~ "Native", 
    data_name=="es_binary_vercoop" ~ "Native", 
    data_name=="es_en_DEEPL_binary_matconf" ~ "DeepL", 
    data_name=="es_en_DEEPL_binary_matcoop" ~ "DeepL", 
    data_name=="es_en_DEEPL_binary_verconf" ~ "DeepL", 
    data_name=="es_en_DEEPL_binary_vercoop" ~ "DeepL", 
    data_name=="es_en_DEEP_binary_matconf" ~ "Deep", 
    data_name=="es_en_DEEP_binary_matcoop" ~ "Deep", 
    data_name=="es_en_DEEP_binary_verconf" ~ "Deep", 
    data_name=="es_en_DEEP_binary_vercoop" ~ "Deep", 
    data_name=="es_en_GOOGLE_binary_matconf" ~ "GT", 
    data_name=="es_en_GOOGLE_binary_matcoop" ~ "GT", 
    data_name=="es_en_GOOGLE_binary_verconf" ~ "GT", 
    data_name=="es_en_GOOGLE_binary_vercoop" ~ "GT", 
    data_name=="es_en_TRANSOFMERS_binary_matconf" ~ "OPUS", 
    data_name=="es_en_TRANSOFMERS_binary_matcoop" ~ "OPUS", 
    data_name=="es_en_TRANSOFMERS_binary_verconf" ~ "OPUS", 
    data_name=="es_en_TRANSOFMERS_binary_vercoop" ~ "OPUS", 
    TRUE ~ data_name
  ))

table(data.binquad$mt.tool)




# Translation tool
data.binquad <- data.binquad %>%
  mutate(quadclass = case_when(
    #data_name=="BinQuad_AR_matconf" ~ "MatConf",                # new 
    #data_name=="BinQuad_AR_matcoop" ~ "MatCoop",                # new 
    #data_name=="BinQuad_AR_vercoop" ~ "VerbCoop",                # new 
    data_name=="BinQuad_AR_EN_DEEPL_matconf" ~ "MatConf", 
    data_name=="BinQuad_AR_EN_DEEPL_matcoop" ~ "MatCoop", 
    data_name=="BinQuad_AR_EN_DEEPL_verconf" ~ "VerbConf", 
    data_name=="BinQuad_AR_EN_DEEPL_vercoop" ~ "VerbCoop", 
    data_name=="BinQuad_AR_EN_DEEP_matconf" ~ "MatConf", 
    data_name=="BinQuad_AR_EN_DEEP_matcoop" ~ "MatCoop", 
    data_name=="BinQuad_AR_EN_DEEP_verconf" ~ "VerbConf", 
    data_name=="BinQuad_AR_EN_DEEP_vercoop" ~ "VerbCoop", 
    data_name=="BinQuad_AR_EN_GOOGLE_matconf" ~ "MatConf", 
    data_name=="BinQuad_AR_EN_GOOGLE_matcoop" ~ "MatCoop",      # new
    data_name=="BinQuad_AR_EN_GOOGLE_verconf" ~ "VerbConf",      # new
    data_name=="BinQuad_AR_EN_GOOGLE_vercoop" ~ "VerbCoop",      # new
    data_name=="BinQuad_AR_matconf" ~ "MatConf", 
    data_name=="BinQuad_AR_matcoop" ~ "MatCoop", 
    data_name=="BinQuad_AR_verconf" ~ "VerbConf",                # new
    data_name=="BinQuad_AR_vercoop" ~ "VerbCoop", 
    data_name=="BinQuad_EN_AR_DEEP_matconf" ~ "MatConf",         # new
    data_name=="BinQuad_EN_AR_DEEP_matcoop" ~ "MatCoop",         # new
    data_name=="BinQuad_EN_AR_DEEP_verconf" ~ "VerbConf",         # new
    data_name=="BinQuad_EN_AR_DEEP_vercoop" ~ "VerbCoop",         # new
    data_name=="BinQuad_EN_AR_GOOGLE_matconf" ~ "MatConf",
    data_name=="BinQuad_EN_AR_GOOGLE_matcoop" ~ "MatCoop",
    data_name=="BinQuad_EN_AR_GOOGLE_verconf" ~ "VerbConf", 
    data_name=="BinQuad_EN_AR_GOOGLE_vercoop" ~ "VerbCoop", 
    data_name=="BinQuad_EN_AR_TRANSFORMERS_matconf" ~ "MatConf", 
    data_name=="BinQuad_EN_AR_TRANSFORMERS_matcoop" ~ "MatCoop", 
    data_name=="BinQuad_EN_AR_TRANSFORMERS_verconf" ~ "VerbConf", 
    data_name=="BinQuad_EN_AR_TRANSFORMERS_vercoop" ~ "VerbCoop", 
    data_name=="BinQuad_EN_ES_DEEPL_matconf" ~ "MatConf", 
    data_name=="BinQuad_EN_ES_DEEPL_matcoop" ~ "MatCoop", 
    data_name=="BinQuad_EN_ES_DEEPL_verconf" ~ "VerbConf", 
    data_name=="BinQuad_EN_ES_DEEPL_vercoop" ~ "VerbCoop", 
    data_name=="BinQuad_EN_ES_DEEP_matconf" ~ "MatConf", 
    data_name=="BinQuad_EN_ES_DEEP_matcoop" ~ "MatCoop" ,
    data_name=="BinQuad_EN_ES_DEEP_verconf" ~ "VerbConf", 
    data_name=="BinQuad_EN_ES_DEEP_vercoop" ~ "VerbCoop", 
    data_name=="BinQuad_EN_ES_GOOGLE_matconf" ~ "MatConf", 
    data_name=="BinQuad_EN_ES_GOOGLE_matcoop" ~ "MatCoop", 
    data_name=="BinQuad_EN_ES_GOOGLE_verconf" ~ "VerbConf", 
    data_name=="BinQuad_EN_ES_GOOGLE_vercoop" ~ "VerbCoop", 
    data_name=="BinQuad_EN_matconf" ~ "MatConf", 
    data_name=="BinQuad_EN_matcoop" ~ "MatCoop", 
    data_name=="BinQuad_EN_verconf" ~ "VerbConf", 
    data_name=="BinQuad_EN_vercoop" ~ "VerbCoop", 
    data_name=="ar_en_GOOGLE_binary_matconf" ~ "MatConf",       # new
    data_name=="ar_en_GOOGLE_binary_matcoop" ~ "MatCoop", 
    data_name=="ar_en_GOOGLE_binary_verconf" ~ "VerbConf", 
    data_name=="ar_en_GOOGLE_binary_vercoop" ~ "VerbCoop", 
    data_name=="ar_en_TRANSFORMERS_binary_matconf" ~ "MatConf", 
    data_name=="ar_en_TRANSFORMERS_binary_matcoop" ~ "MatCoop", 
    data_name=="ar_en_TRANSFORMERS_binary_verconf" ~ "VerbConf", 
    data_name=="ar_en_TRANSFORMERS_binary_vercoop" ~ "VerbCoop", 
    data_name=="en_ar_DEEPL_binary_matconf" ~ "MatConf", 
    data_name=="en_ar_DEEPL_binary_matcoop" ~ "MatCoop", 
    data_name=="en_ar_DEEPL_binary_verconf" ~ "VerbConf", 
    data_name=="en_ar_DEEPL_binary_vercoop" ~ "VerbCoop", 
    data_name=="en_ar_DEEP_binary_matconf" ~ "MatConf", 
    data_name=="en_ar_DEEP_binary_matcoop" ~ "MatCoop", 
    data_name=="en_ar_DEEP_binary_verconf" ~ "VerbConf",         # new
    data_name=="en_ar_DEEP_binary_vercoop" ~ "VerbCoop",         # new,
    data_name=="en_es_TRANSFORMERS_binary_matconf" ~ "MatConf", 
    data_name=="en_es_TRANSFORMERS_binary_matcoop" ~ "MatCoop", 
    data_name=="en_es_TRANSFORMERS_binary_verconf" ~ "VerbConf", 
    data_name=="en_es_TRANSFORMERS_binary_vercoop" ~ "VerbCoop", 
    data_name=="es_binary_matconf" ~ "MatConf", 
    data_name=="es_binary_matcoop" ~ "MatCoop", 
    data_name=="es_binary_verconf" ~ "VerbConf", 
    data_name=="es_binary_vercoop" ~ "VerbCoop", 
    data_name=="es_en_DEEPL_binary_matconf" ~ "MatConf", 
    data_name=="es_en_DEEPL_binary_matcoop" ~ "MatCoop", 
    data_name=="es_en_DEEPL_binary_verconf" ~ "VerbConf", 
    data_name=="es_en_DEEPL_binary_vercoop" ~ "VerbCoop", 
    data_name=="es_en_DEEP_binary_matconf" ~ "MatConf", 
    data_name=="es_en_DEEP_binary_matcoop" ~ "MatCoop", 
    data_name=="es_en_DEEP_binary_verconf" ~ "VerbConf", 
    data_name=="es_en_DEEP_binary_vercoop" ~ "VerbCoop", 
    data_name=="es_en_GOOGLE_binary_matconf" ~ "MatConf", 
    data_name=="es_en_GOOGLE_binary_matcoop" ~ "MatCoop", 
    data_name=="es_en_GOOGLE_binary_verconf" ~ "VerbConf", 
    data_name=="es_en_GOOGLE_binary_vercoop" ~ "VerbCoop", 
    data_name=="es_en_TRANSOFMERS_binary_matconf" ~ "MatConf", 
    data_name=="es_en_TRANSOFMERS_binary_matcoop" ~ "MatCoop", 
    data_name=="es_en_TRANSOFMERS_binary_verconf" ~ "VerbConf", 
    data_name=="es_en_TRANSOFMERS_binary_vercoop" ~ "VerbCoop", 
    TRUE ~ data_name
  ))

table(data.binquad$quadclass)



# Model
data.binquad <- data.binquad %>%
  mutate(model = case_when(
    model_name=="ConfliBERT-Arabic-multilingual-cased-v2" ~ "ConfliBERT cont case", 
    model_name=="ConfliBERT-Arabic-multilingual-uncased-v2" ~ "ConfliBERT cont unc", 
    model_name=="ConfliBERT-cont-cased" ~ "ConfliBERT cont case", 
    model_name=="ConfliBERT-cont-uncased" ~ "ConfliBERT cont  unc", 
    model_name=="ConfliBERT-Spanish-multilingual-cased-v2" ~ "ConfliBERT cont case", 
    model_name=="ConfliBERT-Spanish-multilingual-uncased-v2" ~ "ConfliBERT cont unc",        
    TRUE ~ model_name
  ))

table(data.binquad$model)




# Model
data.binquad <- data.binquad %>%
  mutate(case = case_when(
    model_name=="ConfliBERT-Arabic-multilingual-cased-v2" ~ "Cased", 
    model_name=="ConfliBERT-Arabic-multilingual-uncased-v2" ~ "Uncased", 
    model_name=="ConfliBERT-cont-cased" ~ "Cased", 
    model_name=="ConfliBERT-cont-uncased" ~ "Uncased", 
    model_name=="ConfliBERT-Spanish-multilingual-cased-v2" ~ "Cased", 
    model_name=="ConfliBERT-Spanish-multilingual-uncased-v2" ~ "Uncased" ,        
    TRUE ~ model_name
  ))

table(data.binquad$case)




# Check names
table(data.binquad$lang)
table(data.binquad$mt.tool)
table(data.binquad$model)
table(data.binquad$trans.lang,data.binquad$model)
















# CALCULATE MEANS AND SD  --------------------------------------------------



## Binary -----------

bin.mean <- data.bin %>%
  #group_by(data_name,model_name) %>%
  group_by(lang,trans.lang,mt.tool,model,case) %>%
  dplyr::summarize(mean = mean(f1, na.rm=TRUE)) 

bin.sd <- data.bin %>%
  #group_by(data_name,model_name) %>%
  group_by(lang,trans.lang,mt.tool,model,case) %>%
  dplyr::summarize(sd = sd(f1))

bin.m.sd <- left_join(bin.mean,bin.sd, by=c("lang","trans.lang","mt.tool","model","case"))
bin.m.sd <- bin.m.sd %>% 
  arrange(lang,trans.lang,mt.tool,model,case) %>%
  rename(f1=mean)

rm(bin.mean,bin.sd)



## Quadclass -----------

quad.mean <- data.quad %>%
  #group_by(data_name,model_name) %>%
  group_by(lang,trans.lang,mt.tool,model,case) %>%
  dplyr::summarize(mean = mean(f1_macro, na.rm=TRUE)) 

quad.sd <- data.quad %>%
  #group_by(data_name,model_name) %>%
  group_by(lang,trans.lang,mt.tool,model,case) %>%
  dplyr::summarize(sd = sd(f1_macro))

quad.m.sd <- left_join(quad.mean,quad.sd, by=c("lang","trans.lang","mt.tool","model","case"))
quad.m.sd <- quad.m.sd %>% 
  arrange(lang,trans.lang,mt.tool,model,case)%>%
  rename(macro.f1=mean)

rm(quad.mean,quad.sd)




## BinQuad -----------

binquad.mean <- data.binquad %>%
  #group_by(data_name,model_name) %>%
  group_by(quadclass,lang,trans.lang,mt.tool,model,case) %>%
  dplyr::summarize(mean = mean(f1, na.rm=TRUE)) 

binquad.sd <- data.binquad %>%
  #group_by(data_name,model_name) %>%
  group_by(quadclass,lang,trans.lang,mt.tool,model,case) %>%
  dplyr::summarize(sd = sd(f1))

binquad.m.sd <- left_join(binquad.mean,binquad.sd, by=c("quadclass","lang","trans.lang","mt.tool","model","case"))
binquad.m.sd <- binquad.m.sd %>% 
  arrange(quadclass,lang,trans.lang,mt.tool,model,case)%>%
  rename(f1=mean)

rm(binquad.mean,binquad.sd)








# PLOT  --------------------------------------------------



## Bin -----------

ggplot(bin.m.sd, aes(x=mt.tool, 
                     y = trans.lang, 
                     fill= f1)) + 
  geom_tile() +  
  facet_grid(rows=vars(lang),cols=vars(case),scales="free") +
  theme(axis.text.x = element_text(angle=90, vjust=.5, hjust=1)) + 
  geom_text(aes(label=round(f1, 2)),color="white",  size=2.5)  +
  theme_bw() +  
  xlab("Translation Tools") + ylab("Language") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  theme(legend.position="bottom") +
  theme(legend.text=element_text(size=rel(0.7))) + 
  guides(fill = guide_colourbar(theme = theme(
    legend.key.width  = unit(5, "lines"),
    legend.key.height = unit(0.5, "lines"))))

ggsave("graphs/bin_results.pdf", width = 4.5, height = 4, units = "in")
ggsave("graphs/bin_results.png", width = 4.5, height = 4, units = "in")



## Quad -----------
 
ggplot(quad.m.sd, aes(x=mt.tool, 
                      y = trans.lang, 
                      fill= macro.f1)) + 
   geom_tile() +  
   facet_grid(rows=vars(lang),cols=vars(case),scales="free") +
   theme(axis.text.x = element_text(angle=90, vjust=.5, hjust=1)) + 
  geom_text(aes(label=round(macro.f1, 2)),color="white",  size=2.5)  +
  theme_bw() + 
   xlab("Translation Tools") + ylab("Language") + 
   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  theme(legend.position="bottom") +
  theme(legend.text=element_text(size=rel(0.8))) + 
  guides(fill = guide_colourbar(theme = theme(
    legend.key.width  = unit(5, "lines"),
    legend.key.height = unit(0.5, "lines"))))
 
ggsave("graphs/quad_results.pdf", width = 4.5, height = 4, units = "in")
ggsave("graphs/quad_results.png", width = 4.5, height = 4, units = "in")




## BinQuad -----------

binquad.m.sd.matconf <- binquad.m.sd %>% filter(quadclass=="MatConf")
binquad.m.sd.matcoop <- binquad.m.sd %>% filter(quadclass=="MatCoop")
binquad.m.sd.verbconf <- binquad.m.sd %>% filter(quadclass=="VerbConf")
binquad.m.sd.verbcoop <- binquad.m.sd %>% filter(quadclass=="VerbCoop")


# matconf

plot.matconf <- ggplot(binquad.m.sd.matconf, aes(x=mt.tool, 
                     y = trans.lang, 
                     fill= f1)) + 
  geom_tile() +  
  facet_grid(rows=vars(lang),cols=vars(case),scales="free") +
  theme(axis.text.x = element_text(angle=90, vjust=.5, hjust=1)) + 
  geom_text(aes(label=round(f1, 2)),color="white",  size=2.5)  +
  theme_bw() + 
  #xlab("Translation Tools") + ylab("Language") + 
  theme(axis.title.x=element_blank(), axis.title.y=element_blank()) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  theme(legend.position="bottom") +
  theme(legend.text=element_text(size=rel(0.8))) + 
  guides(fill = guide_colourbar(theme = theme(
    legend.key.width  = unit(10, "lines"),
    legend.key.height = unit(0.5, "lines")))) +
  ggtitle("(a) Material Conflict")

print(plot.matconf)

ggsave("graphs/binquad_matconf_results.pdf", width = 4.5, height = 4, units = "in")
ggsave("graphs/binquad_matconf_results.png", width = 4.5, height = 4, units = "in")


# matcoop

plot.matcoop <- ggplot(binquad.m.sd.matcoop, aes(x=mt.tool, 
                                 y = trans.lang, 
                                 fill= f1)) + 
  geom_tile() +  
  facet_grid(rows=vars(lang),cols=vars(case),scales="free") +
  theme(axis.text.x = element_text(angle=90, vjust=.5, hjust=1)) + 
  geom_text(aes(label=round(f1, 2)),color="white",  size=2.5)  +
  theme_bw() + 
  #xlab("Translation Tools") + ylab("Language") + 
  theme(axis.title.x=element_blank(), axis.title.y=element_blank()) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  theme(legend.position="bottom") +
  theme(legend.text=element_text(size=rel(0.8))) + 
  guides(fill = guide_colourbar(theme = theme(
    legend.key.width  = unit(10, "lines"),
    legend.key.height = unit(0.5, "lines")))) +
  ggtitle("(b) Material Cooperation")

print(plot.matcoop)

ggsave("graphs/binquad_matcoop_results.pdf", width = 4.5, height = 4, units = "in")
ggsave("graphs/binquad_matcoop_results.png", width = 4.5, height = 4, units = "in")


# verbconf

plot.verbconf <- ggplot(binquad.m.sd.verbconf, aes(x=mt.tool, 
                                 y = trans.lang, 
                                 fill= f1)) + 
  geom_tile() +  
  facet_grid(rows=vars(lang),cols=vars(case),scales="free") +
  theme(axis.text.x = element_text(angle=90, vjust=.5, hjust=1)) + 
  geom_text(aes(label=round(f1, 2)),color="white",  size=2.5)  +
  theme_bw() + 
  #xlab("Translation Tools") + ylab("Language") + 
  theme(axis.title.x=element_blank(), axis.title.y=element_blank()) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  theme(legend.position="bottom") +
  theme(legend.text=element_text(size=rel(0.8))) + 
  guides(fill = guide_colourbar(theme = theme(
    legend.key.width  = unit(10, "lines"),
    legend.key.height = unit(0.5, "lines")))) +
  ggtitle("(c) Verbal Conflict")

print(plot.verbconf)

ggsave("graphs/binquad_verbconf_results.pdf", width = 4.5, height = 4, units = "in")
ggsave("graphs/binquad_verbconf_results.png", width = 4.5, height = 4, units = "in")


# verbcoop

plot.verbcoop <- ggplot(binquad.m.sd.verbcoop, aes(x=mt.tool, 
                                 y=trans.lang, 
                                 fill= f1)) + 
  geom_tile() +  
  facet_grid(rows=vars(lang),cols=vars(case),scales="free") +
  theme(axis.text.x = element_text(angle=90, vjust=.5, hjust=1)) + 
  geom_text(aes(label=round(f1, 2)),color="white",  size=2.5)  +
  
  theme_bw() + 
  #xlab("Translation Tools") + ylab("Language") + 
  theme(axis.title.x=element_blank(), axis.title.y=element_blank()) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  theme(legend.position="bottom") +
  theme(legend.text=element_text(size=rel(0.8))) + 
  guides(fill = guide_colourbar(theme = theme(
    legend.key.width  = unit(10, "lines"),
    legend.key.height = unit(0.5, "lines")))) +
  ggtitle("(d) Verbal Cooperation")

print(plot.verbcoop)

ggsave("graphs/binquad_verbcoop_results.pdf", width = 4.5, height = 4, units = "in")
ggsave("graphs/binquad_verbcoop_results.png", width = 4.5, height = 4, units = "in")



# Merge all BinQuad plots

ggarrange(plot.matconf , plot.matcoop, plot.verbconf , plot.verbcoop,
          ncol = 1, nrow = 4)

ggsave("graphs/binquad_all_results.pdf", width = 5, height = 15, units = "in")
ggsave("graphs/binquad_all_results.png", width = 5, height = 15, units = "in")







# IDENTIFY BEST PERFORMANCE  --------------------------------------------------

# Bin 
max.bin.ar <- bin.m.sd %>% filter(lang=="AR") %>% arrange(desc(f1))
max.bin.en <- bin.m.sd %>% filter(lang=="EN") %>% arrange(desc(f1))
max.bin.es <- bin.m.sd %>% filter(lang=="ES") %>% arrange(desc(f1))


# Quad
max.quad.ar <- quad.m.sd %>% filter(lang=="AR") %>% arrange(desc(macro.f1))
max.quad.en <- quad.m.sd %>% filter(lang=="EN") %>% arrange(desc(macro.f1))
max.quad.es <- quad.m.sd %>% filter(lang=="ES") %>% arrange(desc(macro.f1))


# BinQuad - matconf
max.binquad.matconf.ar <- binquad.m.sd.matconf %>% filter(lang=="AR") %>% arrange(desc(f1))
max.binquad.matconf.en <- binquad.m.sd.matconf %>% filter(lang=="EN") %>% arrange(desc(f1))
max.binquad.matconf.es <- binquad.m.sd.matconf %>% filter(lang=="ES") %>% arrange(desc(f1))


# BinQuad - matcoop
max.binquad.matcoop.ar <- binquad.m.sd.matcoop %>% filter(lang=="AR") %>% arrange(desc(f1))
max.binquad.matcoop.en <- binquad.m.sd.matcoop %>% filter(lang=="EN") %>% arrange(desc(f1))
max.binquad.matcoop.es <- binquad.m.sd.matcoop %>% filter(lang=="ES") %>% arrange(desc(f1))


# BinQuad - verbconf
max.binquad.verbconf.ar <- binquad.m.sd.verbconf %>% filter(lang=="AR") %>% arrange(desc(f1))
max.binquad.verbconf.en <- binquad.m.sd.verbconf %>% filter(lang=="EN") %>% arrange(desc(f1))
max.binquad.verbconf.es <- binquad.m.sd.verbconf %>% filter(lang=="ES") %>% arrange(desc(f1))


# BinQuad - verbcoop
max.binquad.verbcoop.ar <- binquad.m.sd.verbcoop %>% filter(lang=="AR") %>% arrange(desc(f1))
max.binquad.verbcoop.en <- binquad.m.sd.verbcoop %>% filter(lang=="EN") %>% arrange(desc(f1))
max.binquad.verbcoop.es <- binquad.m.sd.verbcoop %>% filter(lang=="ES") %>% arrange(desc(f1))






# T-TEST FOR PERFORMANCE  #########################################


## Binary ------------------

### AR ------------------

# Check results
View(max.bin.ar)

# Extract results
#bin.ar.top <-  max.bin.ar %>% 
  filter(trans.lang=="EN to AR") %>% 
  filter(mt.tool=="OPUS") %>% 
  filter(case=="Uncased") %>%
  ungroup() %>%
  select(f1, sd)

bin.ar.native <- max.bin.ar %>% 
  filter(trans.lang=="AR Native") %>% 
  filter(mt.tool=="Native") %>% 
  filter(case=="Uncased") %>%
  ungroup() %>%
  select(f1, sd)

# T-test
bin.ar.ttest <-tsum.test(# x = native
  mean.x=as.numeric(bin.ar.top[1]),
  s.x   =as.numeric(bin.ar.top[2]),n.x=50,
  # y = trans
  mean.y=as.numeric(bin.ar.native[1]),
  s.y   =as.numeric(bin.ar.native[2]),n.y=50)

bin.ar.ttest

# OPUS > Native Statistically significant at p<0.000



### EN ------------------

# Check results
#View(max.bin.en)

# Extract results
bin.en.top <-  max.bin.en %>% 
  filter(trans.lang=="AR to EN") %>% 
  filter(mt.tool=="Deep") %>% 
  filter(case=="Cased") %>%
  ungroup() %>%
  select(f1, sd)

bin.en.native <- max.bin.en %>% 
  filter(trans.lang=="EN Native") %>% 
  filter(mt.tool=="Native") %>% 
  filter(case=="Cased") %>%
  ungroup() %>%
  select(f1, sd)

# T-test
bin.en.ttest <-tsum.test(# x = native
  mean.x=as.numeric(bin.en.native[1]),
  s.x   =as.numeric(bin.en.native[2]),n.x=50,
  # y = trans
  mean.y=as.numeric(bin.en.top[1]),
  s.y   =as.numeric(bin.en.top[2]),n.y=50)

bin.en.ttest

# Native > Deep statistically significant at p<0.008


### ES ------------------

# Check results
#View(max.bin.es)

# Extract results
bin.es.top <-  max.bin.es %>% 
  filter(trans.lang=="EN to ES") %>% 
  filter(mt.tool=="GT") %>% 
  filter(case=="Cased") %>%
  ungroup() %>%
  select(f1, sd)

bin.es.native <- max.bin.es %>% 
  filter(trans.lang=="ES Native") %>% 
  filter(mt.tool=="Native") %>% 
  filter(case=="Cased") %>%
  ungroup() %>%
  select(f1, sd)

# T-test
bin.es.ttest <-tsum.test(# x = native
  mean.x=as.numeric(bin.es.top[1]),
  s.x   =as.numeric(bin.es.top[2]),n.x=50,
  # y = trans
  mean.y=as.numeric(bin.es.native[1]),
  s.y   =as.numeric(bin.es.native[2]),n.y=50)

bin.es.ttest
bin.es.top
bin.es.native

# GT > Native Statistically significant at p<0.000






## QuadClass ------------------

### AR ------------------

# Check results
#View(max.quad.ar)

# Extract results
quad.ar.top <-  max.quad.ar %>% 
  filter(trans.lang=="EN to AR") %>% 
  filter(mt.tool=="Deep") %>% 
  filter(case=="Uncased") %>%
  ungroup() %>%
  select(macro.f1, sd)

quad.ar.native <- max.quad.ar %>% 
  filter(trans.lang=="AR Native") %>% 
  filter(mt.tool=="Native") %>% 
  filter(case=="Uncased") %>%
  ungroup() %>%
  select(macro.f1, sd)

# T-test
quad.ar.ttest <-tsum.test(# x = native
  mean.x=as.numeric(quad.ar.top[1]),
  s.x   =as.numeric(quad.ar.top[2]),n.x=50,
  # y = trans
  mean.y=as.numeric(quad.ar.native[1]),
  s.y   =as.numeric(quad.ar.native[2]),n.y=50)

quad.ar.ttest
quad.ar.top
quad.ar.native
# Deep > Native Statistically significant at p<0.02



### EN ------------------

# Check results
#View(max.quad.en)

# Extract results
quad.en.top <-  max.quad.en %>% 
  filter(trans.lang=="ES to EN") %>% 
  filter(mt.tool=="Deep") %>% 
  filter(case=="Cased") %>%
  ungroup() %>%
  select(macro.f1, sd)

quad.en.native <- max.quad.en %>% 
  filter(trans.lang=="EN Native") %>% 
  filter(mt.tool=="Native") %>% 
  filter(case=="Cased") %>%
  ungroup() %>%
  select(macro.f1, sd)

# T-test
quad.en.ttest <-tsum.test(# x = native
  mean.x=as.numeric(quad.en.top[1]),
  s.x   =as.numeric(quad.en.top[2]),n.x=50,
  # y = trans
  mean.y=as.numeric(quad.en.native[1]),
  s.y   =as.numeric(quad.en.native[2]),n.y=50)

quad.en.ttest
quad.en.top
quad.en.native

# Deep > Native statistically significant at p<0.000


### ES ------------------

# Check results
#View(max.quad.es)

# Extract results
quad.es.top <-  max.quad.es %>% 
  filter(trans.lang=="EN to ES") %>% 
  filter(mt.tool=="DeepL") %>% 
  filter(case=="Cased") %>%
  ungroup() %>%
  select(macro.f1, sd)

quad.es.native <- max.quad.es %>% 
  filter(trans.lang=="ES Native") %>% 
  filter(mt.tool=="Native") %>% 
  filter(case=="Cased") %>%
  ungroup() %>%
  select(macro.f1, sd)

# T-test
quad.es.ttest <-tsum.test(# x = native
  mean.x=as.numeric(quad.es.top[1]),
  s.x   =as.numeric(quad.es.top[2]),n.x=50,
  # y = trans
  mean.y=as.numeric(quad.es.native[1]),
  s.y   =as.numeric(quad.es.native[2]),n.y=50)

quad.es.ttest
quad.es.top
quad.es.native

# DeepL > Native Statistically significant at p<0.04





## BinQuad - matconf  -------------------

### AR ------------------

# Check results
View(max.binquad.matconf.ar)

# Extract results
binquad.ar.top <-  max.binquad.matconf.ar %>% 
  filter(trans.lang=="EN to AR") %>% 
  filter(mt.tool=="Deep") %>% 
  filter(case=="Uncased") %>%
  ungroup() %>%
  select(f1, sd)

binquad.ar.native <- max.binquad.matconf.ar %>% 
  filter(trans.lang=="AR Native") %>% 
  filter(mt.tool=="Native") %>% 
  filter(case=="Uncased") %>%
  ungroup() %>%
  select(f1, sd)

# T-test
binquad.ar.ttest <-tsum.test(# x = native
  mean.x=as.numeric(binquad.ar.top[1]),
  s.x   =as.numeric(binquad.ar.top[2]),n.x=50,
  # y = trans
  mean.y=as.numeric(binquad.ar.native[1]),
  s.y   =as.numeric(binquad.ar.native[2]),n.y=50)

binquad.ar.ttest
binquad.ar.top
binquad.ar.native

#    Statistically significant  



### EN ------------------

# Check results
View(max.binquad.matconf.en)

# Extract results
binquad.en.top <-  max.binquad.matconf.en %>% 
  filter(trans.lang=="ES to EN") %>% 
  filter(mt.tool=="Deep") %>% 
  filter(case=="Cased") %>%
  ungroup() %>%
  select(f1, sd)

binquad.en.native <- max.binquad.matconf.en %>% 
  filter(trans.lang=="EN Native") %>% 
  filter(mt.tool=="Native") %>% 
  filter(case=="Cased") %>%
  ungroup() %>%
  select(f1, sd)

# T-test
binquad.en.ttest <-tsum.test(# x = native
  mean.x=as.numeric(binquad.en.top[1]),
  s.x   =as.numeric(binquad.en.top[2]),n.x=50,
  # y = trans
  mean.y=as.numeric(binquad.en.native[1]),
  s.y   =as.numeric(binquad.en.native[2]),n.y=50)

binquad.en.ttest
binquad.en.top
binquad.en.native

#    Statistically significant  



### ES ------------------

# Check results
View(max.binquad.matconf.es)

# Extract results
binquad.es.top <-  max.binquad.matconf.es %>% 
  filter(trans.lang=="EN to ES") %>% 
  filter(mt.tool=="OPUS") %>% 
  filter(case=="Cased") %>%
  ungroup() %>%
  select(f1, sd)

binquad.es.native <- max.binquad.matconf.es %>% 
  filter(trans.lang=="ES Native") %>% 
  filter(mt.tool=="Native") %>% 
  filter(case=="Uncased") %>%
  ungroup() %>%
  select(f1, sd)

# T-test
binquad.es.ttest <-tsum.test(# x = native
  mean.x=as.numeric(binquad.es.native[1]),
  s.x   =as.numeric(binquad.es.native[2]),n.x=50,
  # y = trans
  mean.y=as.numeric(binquad.es.top[1]),
  s.y   =as.numeric(binquad.es.top[2]),n.y=50)

binquad.es.ttest
binquad.es.top
binquad.es.native

#    Statistically significant  





## BinQuad - matcoop  -------------------

### AR ------------------

# Check results
View(max.binquad.matcoop.ar)

# Extract results
binquad.ar.top <-  max.binquad.matcoop.ar %>% 
  filter(trans.lang=="EN to AR") %>% 
  filter(mt.tool=="OPUS") %>% 
  filter(case=="Uncased") %>%
  ungroup() %>%
  select(f1, sd)

binquad.ar.native <- max.binquad.matcoop.ar %>% 
  filter(trans.lang=="AR Native") %>% 
  filter(mt.tool=="Native") %>% 
  filter(case=="Uncased") %>%
  ungroup() %>%
  select(f1, sd)

# T-test
binquad.ar.ttest <-tsum.test(# x = native
  mean.x=as.numeric(binquad.ar.native[1]),
  s.x   =as.numeric(binquad.ar.native[2]),n.x=50,
  # y = trans
  mean.y=as.numeric(binquad.ar.top[1]),
  s.y   =as.numeric(binquad.ar.top[2]),n.y=50)

binquad.ar.ttest
binquad.ar.top
binquad.ar.native

#   Statistically no significant  p<0.9523 



### EN ------------------

# Check results
View(max.binquad.matcoop.en)

# Extract results
binquad.en.top <-  max.binquad.matcoop.en %>% 
  filter(trans.lang=="ES to EN") %>% 
  filter(mt.tool=="OPUS") %>% 
  filter(case=="Cased") %>%
  ungroup() %>%
  select(f1, sd)

binquad.en.native <- max.binquad.matcoop.en %>% 
  filter(trans.lang=="EN Native") %>% 
  filter(mt.tool=="Native") %>% 
  filter(case=="Cased") %>%
  ungroup() %>%
  select(f1, sd)

# T-test
binquad.en.ttest <-tsum.test(# x = native
  mean.x=as.numeric(binquad.en.top[1]),
  s.x   =as.numeric(binquad.en.top[2]),n.x=50,
  # y = trans
  mean.y=as.numeric(binquad.en.native[1]),
  s.y   =as.numeric(binquad.en.native[2]),n.y=50)

binquad.en.ttest
binquad.en.top
binquad.en.native

#   Statistically significant at p<0. 



### ES ------------------

# Check results
View(max.binquad.matcoop.es)

# Extract results
binquad.es.top <-  max.binquad.matcoop.es %>% 
  filter(trans.lang=="EN to ES") %>% 
  filter(mt.tool=="GT") %>% 
  filter(case=="Cased") %>%
  ungroup() %>%
  select(f1, sd)

binquad.es.native <- max.binquad.matcoop.es %>% 
  filter(trans.lang=="ES Native") %>% 
  filter(mt.tool=="Native") %>% 
  filter(case=="Cased") %>%
  ungroup() %>%
  select(f1, sd)

# T-test
binquad.es.ttest <-tsum.test(# x = native
  mean.x=as.numeric(binquad.es.top[1]),
  s.x   =as.numeric(binquad.es.top[2]),n.x=50,
  # y = trans
  mean.y=as.numeric(binquad.es.native[1]),
  s.y   =as.numeric(binquad.es.native[2]),n.y=50)

binquad.es.ttest
binquad.es.top
binquad.es.native

#   Statistically significant at p<0. 







## BinQuad - verbconf  -------------------

### AR ------------------

# Check results
View(max.binquad.verbconf.ar)

# Extract results
binquad.ar.top <-  max.binquad.verbconf.ar %>% 
  filter(trans.lang=="EN to AR") %>% 
  filter(mt.tool=="Deep") %>% 
  filter(case=="Uncased") %>%
  ungroup() %>%
  select(f1, sd)

binquad.ar.native <- max.binquad.verbconf.ar %>% 
  filter(trans.lang=="AR Native") %>% 
  filter(mt.tool=="Native") %>% 
  filter(case=="Uncased") %>%
  ungroup() %>%
  select(f1, sd)

# T-test
binquad.ar.ttest <-tsum.test(# x = native
  mean.x=as.numeric(binquad.ar.top[1]),
  s.x   =as.numeric(binquad.ar.top[2]),n.x=50,
  # y = trans
  mean.y=as.numeric(binquad.ar.native[1]),
  s.y   =as.numeric(binquad.ar.native[2]),n.y=50)

binquad.ar.ttest
binquad.ar.top
binquad.ar.native
# DeepL > Native Statistically significant at p<0.000



### EN ------------------

# Check results
View(max.binquad.verbconf.en)

# Extract results
binquad.en.top <-  max.binquad.verbconf.en %>% 
  filter(trans.lang=="AR to EN") %>% 
  filter(mt.tool=="GT") %>% 
  filter(case=="Cased") %>%
  ungroup() %>%
  select(f1, sd)

binquad.en.native <- max.binquad.verbconf.en %>% 
  filter(trans.lang=="EN Native") %>% 
  filter(mt.tool=="Native") %>% 
  filter(case=="Cased") %>%
  ungroup() %>%
  select(f1, sd)

# T-test
binquad.en.ttest <-tsum.test(# x = native
  mean.x=as.numeric(binquad.en.top[1]),
  s.x   =as.numeric(binquad.en.top[2]),n.x=50,
  # y = trans
  mean.y=as.numeric(binquad.en.native[1]),
  s.y   =as.numeric(binquad.en.native[2]),n.y=50)

binquad.en.ttest
binquad.en.top
binquad.en.native

# No Statistically significant 



### ES ------------------

# Check results
View(max.binquad.verbconf.es)

# Extract results
binquad.es.top <-  max.binquad.verbconf.es %>% 
  filter(trans.lang=="EN to ES") %>% 
  filter(mt.tool=="Deep") %>% 
  filter(case=="Cased") %>%
  ungroup() %>%
  select(f1, sd)

binquad.es.native <- max.binquad.verbconf.es %>% 
  filter(trans.lang=="ES Native") %>% 
  filter(mt.tool=="Native") %>% 
  filter(case=="Uncased") %>%
  ungroup() %>%
  select(f1, sd)

# T-test
binquad.es.ttest <-tsum.test(# x = native
  mean.x=as.numeric(binquad.es.native[1]),
  s.x   =as.numeric(binquad.es.native[2]),n.x=50,
  # y = trans
  mean.y=as.numeric(binquad.es.top[1]),
  s.y   =as.numeric(binquad.es.top[2]),n.y=50)

binquad.es.ttest
binquad.es.top
binquad.es.native

# not Statistically significant  




## BinQuad - verbcoop  -------------------


### AR ------------------

# Check results
View(max.binquad.verbcoop.ar)

# Extract results
binquad.ar.top <-  max.binquad.verbcoop.ar %>% 
  filter(trans.lang=="EN to AR") %>% 
  filter(mt.tool=="DeepL") %>% 
  filter(case=="Cased") %>%
  ungroup() %>%
  select(f1, sd)

binquad.ar.native <- max.binquad.verbcoop.ar %>% 
  filter(trans.lang=="AR Native") %>% 
  filter(mt.tool=="Native") %>% 
  filter(case=="Cased") %>%
  ungroup() %>%
  select(f1, sd)

# T-test
binquad.ar.ttest <-tsum.test(# x = native
  mean.x=as.numeric(binquad.ar.top[1]),
  s.x   =as.numeric(binquad.ar.top[2]),n.x=50,
  # y = trans
  mean.y=as.numeric(binquad.ar.native[1]),
  s.y   =as.numeric(binquad.ar.native[2]),n.y=50)

binquad.ar.ttest
binquad.ar.top
binquad.ar.native
# DeepL > Native Statistically significant at p<0.000



### EN ------------------

# Check results
View(max.binquad.verbcoop.en)

# Extract results
binquad.en.top <-  max.binquad.verbcoop.en %>% 
  filter(trans.lang=="AR to EN") %>% 
  filter(mt.tool=="DeepL") %>% 
  filter(case=="Cased") %>%
  ungroup() %>%
  select(f1, sd)

binquad.en.native <- max.binquad.verbcoop.en %>% 
  filter(trans.lang=="EN Native") %>% 
  filter(mt.tool=="Native") %>% 
  filter(case=="Cased") %>%
  ungroup() %>%
  select(f1, sd)

# T-test
binquad.en.ttest <-tsum.test(# x = native
  mean.x=as.numeric(binquad.en.top[1]),
  s.x   =as.numeric(binquad.en.top[2]),n.x=50,
  # y = trans
  mean.y=as.numeric(binquad.en.native[1]),
  s.y   =as.numeric(binquad.en.native[2]),n.y=50)

binquad.en.ttest
binquad.en.top
binquad.en.native

# Native > DeepL Statistically significant at p<0.01972



### ES ------------------

# Check results
View(max.binquad.verbcoop.es)

# Extract results
binquad.es.top <-  max.binquad.verbcoop.es %>% 
  filter(trans.lang=="EN to ES") %>% 
  filter(mt.tool=="Deep") %>% 
  filter(case=="Cased") %>%
  ungroup() %>%
  select(f1, sd)

binquad.es.native <- max.binquad.verbcoop.es %>% 
  filter(trans.lang=="ES Native") %>% 
  filter(mt.tool=="Native") %>% 
  filter(case=="Cased") %>%
  ungroup() %>%
  select(f1, sd)

# T-test
binquad.es.ttest <-tsum.test(# x = native
  mean.x=as.numeric(binquad.es.top[1]),
  s.x   =as.numeric(binquad.es.top[2]),n.x=50,
  # y = trans
  mean.y=as.numeric(binquad.es.native[1]),
  s.y   =as.numeric(binquad.es.native[2]),n.y=50)

binquad.es.ttest
binquad.es.top
binquad.es.native

# Deep > Native Statistically significant at p<0.000











# End of script