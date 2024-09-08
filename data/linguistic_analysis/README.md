# Linguistic Features Data

This file contains data relating to the linguistic features of the native and translated corpora, both of the entire corpus and sentence-by-sentence.

These data are to be used in regression models to see if any of these linguistics features are predictors of performance in the downstream machine-translated models.

There are 3 directories:
* corpus_level - Measurements of features over the entire corpus
* sentence_level - Measurements of features per sentence, for each language
* scripts - Scripts used to generate this data from data_master_text.csv

The features that are analyzed are:
* word count
* lemma count
* noun count
* verb count
* rarity
  * This is the proportion of tokens which are rare. There are two types of rarity: general and genre. General defines a rare token as a token which is not within the 5,000 most common tokens for a language. Genre defines a rare token as a token which is not within the 5,000 most common tokens for a specific genre, in our case, political text.

Within the sentence_level directory, there are two types of files:
* base_measurements - These are simple the measurements for each sentence of each linguistic feature.
* differences - These are differences between the measurements of a translated corpus and a native corpus.
  * In difference files, the name of the column is the name of the translated corpus that is being compared. The native corpus is denoted by the file name. For example, en_rarity_difference.csv contains the differences between all sentences machine translated **into** English and their equivalent English sentence. from_en_rarity_difference.csv contains the differences between all sentences machine-translated **from** English (therefore the sentence is in Spanish or Arabic) and their equivalent English sentence.