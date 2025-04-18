This folder contains text data in .xslx, .tsv and .csv format for each task and language.

<br>

The **data_master_text_clean.xlsx** file contains the text data at the sentence level in the native languages (English, Spanish, and Arabic) as well as all the different machine-translations from Spanish and Arabic into English and the reverse translation from English to Spanish and Arabic.

The **data_master_text_clean.tsv** and **data_master_text_clean.csv**  files include the same information in .tsv and .csv format.

<br>

The subfolders contain individual data files for each task:
* **bin**: Binary classification task for Relevance
* **quad**: Multi-class classification task for QuadClass
* **bin_quad**: Binary classification for each QuadClass category (MatCoop, MatConf, VerCoop, VerConf)

<br>

The text files include the following languages:
* **en** Native English
* **es** Native Spanish
* **ar** Native Arabic
* **en_es** Translation from English to Spanish 
* **en_ar** Translation from English to Arabic
* **es_en** Translation from Spanish to English
* **ar_en** Translation from Arabic to English

<br>
 
We used the following machine-translation tools to generate the translations from Spanish and Arabic into English (es_en, ar_en) and the reverse translation from English to Spanish and Arabic (en_es, en_ar):
* Google translate
* DeepL
* Deep Learning
* Transformers


