# The Devil is in the Details: Assessing the Effects of Machine-translation on LLM Model Training and Performance

Conflict scholars increasingly rely on computational tools to track violence and cooperation at a global scale. To study foreign locations, researchers often use machine translation (MT), but rarely evaluate the quality of the MT output or its consequences on model performance. Using a multi-lingual parallel corpus, this study evaluates the quality of different MT tools for text written in English, Arabic, and Spanish.  Using ConfliBERT, a domain-specific model, we test the effect of MT variations on model performance, and find that MT texts tend to yield better results than native texts.   The analysis shows considerable quality variation across MT tools and reveals distortions in the output related to verbs, nouns, lemmas, and vocabulary rarity. Regression analysis at the sentence-level further shows that MT-induced data loss boosts model performance. 

---

# AUTHORS

Javier Osorio, Afraa Alshammari, Naiff Alatrush, Dagmar Heintze, Amber Converse, Sultan Alsarra, Mahrusa Billah, Latifur Khan, Patrick T. Brandt, Vito D'Orazio


**Citation:**

Pending


**Funding:**

This project was possible thanks to the generous funding of the National Science Foundation to the "Frameworks: Infrastructure For Political And Social Event Data using Machine Learning" reseearch project [award 2311142](https://www.nsf.gov/awardsearch/showAward?AWD_ID=2311142&HistoricalAwards=false).



---

# CONTENT

This repository contains the following folders:
* **data**: contains the different data bases used in the study.
  * **linguistic_analysis**:
     * **corpus_level**: Amber and Dagmar, is this the same as in 3_rarity?
     * **scripts**: Amber and Dagmar, is this the same as in 3_rarity?
     * **sentence_level**: Amber and Dagmar, is this the same as in 3_rarity?
  * **master_file**: Contains the merged results of the text, the machine translations, and predictions << we need to change the name of this folder. It is confusing. I would put it under the data folder.
  * **prediction_results**. << move this to folder 4_predictions.
  * **quality**: contains the quality scores from each assessment metric for each machine-translated oputput. 
  * **text**: contains the master data and subsets for each task.
     * **bin**: specfic databases for the Relevant binary classification task.
     * **bin_quad**: specfic databases for the QuadClass binary classification task.
     * **quad**: specfic databases for the QuadClass multi-class classification task.
     * **old**: do not use, this is deprecated data.
* **1_quality_assessment**
  * **scripts**: includes the R scripts used to analyze the translation quality metrics.
  * **graphs**: stores the graphs derived from the analysis.
* **2_model_performance**
  * **scripts**: includes the R scripts used to analyze the ConfliBERT performance.
  * **graphs**: stores teh graphs derived from the analysis.
* **3_rarity**
  * **corpus_level**: please describe.
  * **scripts**: please describe.
  * **sentence_level**: please describe.
* **4_prediction**
  * **x**
  * **x**
  * **x**










