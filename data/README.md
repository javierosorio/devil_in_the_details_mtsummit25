This folder contains two subfolders:
* **text**: This folder contains text data in .xslx and .tsv format for each task and language. The subfolders contain data for each task:
   * **bin**: Binary classification task for Relevance
   * **quad**: Multi-class classification task for QuadClass
   * **bin_quad**: Binary classification for each QuadClass category (MatCoop, MatConf, VerCoop, VerConf)
* **quality**: XXX pending XXX
* **linguistic_features**: This folder contains data in csv format on linguistic features present in each corpus. Further information on the organization of this data is in the README within the folder. The linguistics features analyzed are:
  * **word count**
  * **lemma count**
  * **noun count**
  * **verb count**
  * **general rarity proportion**: The proportion of words in a sentence which are not within the 5,000 most common words in the respective language
  * **genre rarity proportion**: The proportion of words in a sentence which are not within the 5,000 most common words in the UN Parallel Corpus for the respective language. Defined as within the political genre

